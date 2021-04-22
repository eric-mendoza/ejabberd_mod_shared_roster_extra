
-module(mod_shared_roster_extra).
-author('https://github.com/eric-mendoza').

-behaviour(gen_mod).

-include("logger.hrl").
-include("translate.hrl").

-export([start/2, stop/1, reload/3, mod_options/1,
  get_commands_spec/0, depends/2, mod_doc/0]).



% Commands API
-export([
  % Shared roster
  srg_set_displayed_groups/3, srg_set_opts/4
]).


-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("mod_privacy.hrl").
-include("ejabberd_sm.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("mod_shared_roster.hrl").

-type group_options() :: [{atom(), any()}].
-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), [binary()]) -> ok.
-callback use_cache(binary()) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-optional_callbacks([use_cache/1, cache_nodes/1]).

-define(GROUP_OPTS_CACHE, shared_roster_group_opts_cache).
-define(USER_GROUPS_CACHE, shared_roster_user_groups_cache).
-define(GROUP_EXPLICIT_USERS_CACHE, shared_roster_group_explicit_cache).
-define(SPECIAL_GROUPS_CACHE, shared_roster_special_groups_cache).


%%%
%%% gen_mod
%%%

start(_Host, _Opts) ->
  ejabberd_commands:register_commands(get_commands_spec()).

stop(Host) ->
  case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
    false ->
      ejabberd_commands:unregister_commands(get_commands_spec());
    true ->
      ok
  end.

reload(_Host, _NewOpts, _OldOpts) ->
  ok.

depends(_Host, _Opts) ->
  [{mod_shared_roster, hard}].

%%%
%%% Register commands
%%%

get_commands_spec() ->
  [
    #ejabberd_commands{name = srg_set_displayed_groups, tags = [shared_roster_group],
      desc = "Add group id to the Shared Roster Group display list",
      module = ?MODULE, function = srg_set_displayed_groups,
      args = [{displayedgroups, binary}, {group, binary}, {grouphost, binary}],
      args_example = [<<"group1\ngroup2\ngroup3">>, <<"group3">>, <<"myserver.com">>],
      args_desc = ["Groups to be set as display list", "Group to modify display list identifier", "Group server name"],
      result = {res, rescode}},
    #ejabberd_commands{name = srg_set_opts, tags = [shared_roster_group],
      desc = "Update Shared Roster Group options (name and description)",
      module = ?MODULE, function = srg_set_opts,
      args = [{name, binary}, {description, binary}, {group, binary}, {grouphost, binary}],
      args_example = [<<"Group Test Name">>, <<"Group used for testing.">>, <<"group3">>, <<"myserver.com">>],
      args_desc = ["Shared roster group name", "Shared Roster Group description", "Group identifier", "Group server name"],
      result = {res, rescode}}
  ].

%%%
%%% Shared Roster Groups
%%%

to_list([]) -> [];
to_list([H|T]) -> [to_list(H)|to_list(T)];
to_list(E) when is_atom(E) -> atom_to_list(E);
to_list(E) -> binary_to_list(E).

srg_set_displayed_groups(DisplayedGroups1, Group, GroupHost) ->
  ?DEBUG("Adding group to display list.", []),
  Opts = mod_shared_roster:get_group_opts(GroupHost, Group),
  Label = get_opt(Opts, label, []),
  Description = get_opt(Opts, label, []),
  {DisplayedGroups, DisplayedGroupsOpt} = process_displayed_groups(GroupHost, Group, DisplayedGroups1),

  %% Update displayed groups
  CurrentDisplayedGroups = get_displayed_groups(Group, GroupHost),
  AddedDisplayedGroups =  DisplayedGroups -- CurrentDisplayedGroups,
  RemovedDisplayedGroups = CurrentDisplayedGroups -- DisplayedGroups,
  OldMembers = mod_shared_roster:get_group_explicit_users(GroupHost, Group),
  displayed_groups_update(OldMembers, RemovedDisplayedGroups, remove),
  displayed_groups_update(OldMembers, AddedDisplayedGroups, both),

%%  ?DEBUG("Options: ~p~n", [Label ++ Description ++ DisplayedGroupsOpt]),
  mod_shared_roster:set_group_opts(GroupHost, Group, Label ++ Description ++ DisplayedGroupsOpt),

  ok.

srg_set_opts(Label1, Description1, Group, GroupHost) ->
  ?DEBUG("Setting group options -> Label: ~p, Description: ~p~n", [Label1, Description1]),
  Opts = mod_shared_roster:get_group_opts(GroupHost, Group),
  Label = if Label1 == <<"">> -> [];
              true -> [{label, Label1}]
          end,
  Description = if Description1 == <<"">> -> [];
                  true -> [{description, Description1}]
                end,
  Displayed1 = get_opt(Opts, displayed_groups, []),
  Displayed = if Displayed1 == [] -> [];
                  true -> [{displayed_groups, Displayed1}]
              end,
%%  ?DEBUG("Options: ~p~n", [Label ++ Description ++ Displayed]),
  mod_shared_roster:set_group_opts(GroupHost, Group, Label ++ Description ++ Displayed),
  ok.

mod_options(_) -> [].

%% Description: Parses query received
%% Input: Query formatted as in web admin query to change group options
%% Output: Tuple with Label, Description, and DispGroups
process_displayed_groups(Host, Group, DisplayedGroups1) ->
  DispGroups1 = str:tokens(DisplayedGroups1, <<"\r\n">>),  %% Split string by \r or \n
  {DispGroups, WrongDispGroups} = filter_groups_existence(Host, DispGroups1),  %% Find which groups do not exist
  DispGroupsOpt = if DispGroups == [] -> [];
                    true -> [{displayed_groups, DispGroups}] %% Create property
                  end,
  {DispGroups, DispGroupsOpt}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functions obtained from mod_shared_roster original code.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filter_groups_existence(Host, Groups) ->
  %% Splits Groups which meet the condition
  lists:partition(
    fun(Group) -> error /= mod_shared_roster:get_group_opts(Host, Group) end, Groups).

get_opt(Opts, Opt, Default) ->
  case lists:keysearch(Opt, 1, Opts) of
    {value, {_, Val}} -> Val;
    false -> Default
  end.

get_displayed_groups(Group, LServer) ->
  get_group_opt(LServer, Group, displayed_groups, []).


get_group_opt(Host, Group, Opt, Default) ->
  case mod_shared_roster:get_group_opts(Host, Group) of
    error -> Default;
    Opts ->
      case lists:keysearch(Opt, 1, Opts) of
        {value, {_, Val}} -> Val;
        false -> Default
      end
  end.

displayed_groups_update(Members, DisplayedGroups, Subscription) ->
  lists:foreach(
    fun({U, S}) ->
      push_displayed_to_user(U, S, S, Subscription, DisplayedGroups)
    end, Members).


push_displayed_to_user(LUser, LServer, Host, Subscription, DisplayedGroups) ->
  [push_members_to_user(LUser, LServer, DGroup, Host,
    Subscription)
    || DGroup <- DisplayedGroups].

push_members_to_user(LUser, LServer, Group, Host,
    Subscription) ->
  GroupOpts = mod_shared_roster:get_group_opts(LServer, Group),
  GroupLabel = proplists:get_value(label, GroupOpts, Group), %++
  Members = mod_shared_roster:get_group_users(Host, Group),
  lists:foreach(fun ({U, S}) ->
    N = get_rosteritem_name(U, S),
    push_roster_item(LUser, LServer, U, S, N, GroupLabel,
      Subscription)
                end,
    Members).

push_roster_item(User, Server, ContactU, ContactS, ContactN,
    GroupLabel, Subscription) ->
  Item = #roster{usj =
  {User, Server, {ContactU, ContactS, <<"">>}},
    us = {User, Server}, jid = {ContactU, ContactS, <<"">>},
    name = ContactN, subscription = Subscription, ask = none,
    groups = [GroupLabel]},
  push_item(User, Server, Item).

push_item(User, Server, Item) ->
  mod_roster:push_item(jid:make(User, Server),
    Item#roster{subscription = none},
    Item).

get_rosteritem_name(U, S) ->
  case gen_mod:is_loaded(S, mod_vcard) of
    true ->
      SubEls = mod_vcard:get_vcard(U, S),
      get_rosteritem_name_vcard(SubEls);
    false ->
      <<"">>
  end.

-spec get_rosteritem_name_vcard([xmlel()]) -> binary().
get_rosteritem_name_vcard([Vcard|_]) ->
  case fxml:get_path_s(Vcard,
    [{elem, <<"NICKNAME">>}, cdata])
  of
    <<"">> ->
      fxml:get_path_s(Vcard, [{elem, <<"FN">>}, cdata]);
    Nickname -> Nickname
  end;
get_rosteritem_name_vcard(_) ->
  <<"">>.

mod_doc() ->
  #{desc =>
  ?T("This module provides additional administrative commands for shared rosters.")}.

