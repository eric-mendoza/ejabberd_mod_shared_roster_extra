
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
  srg_group_add/4, srg_group_del/4, srg_opts/3
]).


-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("mod_privacy.hrl").
-include("ejabberd_sm.hrl").
-include_lib("xmpp/include/xmpp.hrl").

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
  [].

%%
%% Cache stuff I dont understand
%%
-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
  case use_cache(Mod, Host) of
    true ->
      CacheOpts = cache_opts(Opts),
      ets_cache:new(?GROUP_OPTS_CACHE, CacheOpts),
      ets_cache:new(?USER_GROUPS_CACHE, CacheOpts),
      ets_cache:new(?GROUP_EXPLICIT_USERS_CACHE, CacheOpts),
      ets_cache:new(?SPECIAL_GROUPS_CACHE, CacheOpts);
    false ->
      ets_cache:delete(?GROUP_OPTS_CACHE),
      ets_cache:delete(?USER_GROUPS_CACHE),
      ets_cache:delete(?GROUP_EXPLICIT_USERS_CACHE),
      ets_cache:delete(?SPECIAL_GROUPS_CACHE)
  end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
  MaxSize = mod_private_opt:cache_size(Opts),
  CacheMissed = mod_private_opt:cache_missed(Opts),
  LifeTime = mod_private_opt:cache_life_time(Opts),
  [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
  case erlang:function_exported(Mod, use_cache, 1) of
    true -> Mod:use_cache(Host);
    false -> mod_shared_roster_opt:use_cache(Host)
  end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
  case erlang:function_exported(Mod, cache_nodes, 1) of
    true -> Mod:cache_nodes(Host);
    false -> ejabberd_cluster:get_nodes()
  end.



%%%
%%% Register commands
%%%

get_commands_spec() ->
  [
    #ejabberd_commands{name = srg_group_add, tags = [shared_roster_group],
      desc = "Add group id to the Shared Roster Group display list",
      module = ?MODULE, function = srg_group_add,
      args = [{user, binary}, {host, binary}, {group, binary}, {grouphost, binary}],
      args_example = [<<"group_to_add">>, <<"myserver.com">>, <<"group3">>, <<"myserver.com">>],
      args_desc = ["Username", "User server name", "Group identifier", "Group server name"],
      result = {res, rescode}},
    #ejabberd_commands{name = srg_group_del, tags = [shared_roster_group],
      desc = "Delete group id from the Shared Roster Group",
      module = ?MODULE, function = srg_group_del,
      args = [{user, binary}, {host, binary}, {group, binary}, {grouphost, binary}],
      args_example = [<<"group_to_delete">>, <<"myserver.com">>, <<"group3">>, <<"myserver.com">>],
      args_desc = ["Username", "User server name", "Group identifier", "Group server name"],
      result = {res, rescode}},
    #ejabberd_commands{name = srg_opts, tags = [shared_roster_group],
      desc = "Update Shared Roster Group options (name and description)",
      module = ?MODULE, function = srg_opts,
      args = [{name, binary}, {description, binary}, {group, binary}, {grouphost, binary}],
      args_example = [<<"name_to_set">>, <<"description_to_set">>, <<"group3">>, <<"myserver.com">>],
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

srg_group_add(NewGroup, NewGroupHost, Group, GroupHost) ->
  Opts = [],
  ?DEBUG("Hola Hsing! Agregando grupo.", []),
%%  mod_shared_roster:set_group_opts(GroupHost, Group, Opts),
  ok.

srg_group_del(DeleteGroup, DeleteGroupHost, Group, GroupHost) ->
  ?DEBUG("Hola Hsing! Quitando displayed grupo.", []),
  Opts = [],
%%  mod_shared_roster:set_group_opts(GroupHost, Group, Opts),
  ok.

srg_opts(Host, Group, Opts) ->
  ?DEBUG("Hola Hsing! Cambiando opciones de grupo.", []),
  ok.

mod_options(_) -> [].

%% Description: Parses query received
%% Input: Query formatted as in web admin query to change group options
%% Output: Tuple with Label, Description, and DispGroups
shared_roster_group_parse_query(Host, Group, Query) ->
  case lists:keysearch(<<"submit">>, 1, Query) of
    {value, _} ->
      %%  Fetch value of submitted values
      {value, {_, Label}} = lists:keysearch(<<"label">>, 1,
        Query), %++
      {value, {_, Description}} =
        lists:keysearch(<<"description">>, 1, Query),
      {value, {_, SMembers}} = lists:keysearch(<<"members">>,
        1, Query),
      {value, {_, SDispGroups}} =
        lists:keysearch(<<"dispgroups">>, 1, Query),

      %% Process found values
      LabelOpt = if Label == <<"">> -> [];
                   true -> [{label, Label}] %++
                 end,
      DescriptionOpt = if Description == <<"">> -> [];
                         true -> [{description, Description}]
                       end,
      DispGroups1 = str:tokens(SDispGroups, <<"\r\n">>),  %% Split string by \r or \n
      {DispGroups, WrongDispGroups} = filter_groups_existence(Host, DispGroups1),  %% Find which groups do not exist
      DispGroupsOpt = if DispGroups == [] -> [];
                        true -> [{displayed_groups, DispGroups}] %% Create property
                      end,
      {LabelOpt, DescriptionOpt, DispGroupsOpt};
    _ -> nothing
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functions obtained from mod_shared_roster original code.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filter_groups_existence(Host, Groups) ->
  %% Splits Groups which meet the condition
  lists:partition(
    fun(Group) -> error /= get_group_opts(Host, Group) end, Groups).


get_group_opts(Host1, Group1) ->
  {Host, Group} = split_grouphost(Host1, Group1),
  Mod = gen_mod:db_mod(Host, ?MODULE),
  Res = case use_cache(Mod, Host) of
          true ->
            ets_cache:lookup(
              ?GROUP_OPTS_CACHE, {Host, Group},
              fun() ->
                case Mod:get_group_opts(Host, Group) of
                  error -> error;
                  V -> {cache, V}
                end
              end);
          false ->
            Mod:get_group_opts(Host, Group)
        end,
  case Res of
    {ok, Opts} -> Opts;
    error -> error
  end.

split_grouphost(Host, Group) ->
  case str:tokens(Group, <<"@">>) of
    [GroupName, HostName] -> {HostName, GroupName};
    [_] -> {Host, Group}
  end.

mod_doc() ->
  #{desc =>
  ?T("This module provides additional administrative commands for shared rosters.")}.

