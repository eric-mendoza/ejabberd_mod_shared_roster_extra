# mod_shared_roster_extra
The [mod_shared_roster module](https://github.com/processone/ejabberd/blob/master/src/mod_shared_roster.erl) doesn't provides API commands to edit Shared Roster Group displayed groups and options _i.e._ name and description. This module adds extra commands to cover those scenarios in Shared Roster Groups. The commands added by this modules are:
 * srg_group_add
 * srg_group_del
 * srg_set_options

## Install
Tested with ejabberd 20.04:
```bash
git clone <URL of this git repo>
# Copy the source code folder to the module sources folder of your ejabberd
# Installation (may be different on your machine - see CONTRIB_MODULES_PATH in /etc/ejaberd/ejabberdctl.cfg)
sudo mkdir -p /var/lib/ejabberd/.ejabberd-modules/sources/mod_shared_roster_extra
sudo cp -R <name of this repo>/*.spec <name of this repo>/src /var/lib/ejabberd/.ejabberd-modules/sources/mod_shared_roster_extra
# If done right ejabberdctl will list mod_shared_roster_extra as available module
ejabberdctl modules_available
# Automatically compile and install mod_shared_roster_extra
ejabberdctl module_install mod_shared_roster_extra
```

## Usage

Add in ejabberd.yaml

~~~
modules:
  ...
  mod_shared_roster_extra: {}
  ...
~~~

## Credits
Created by [Eric Mendoza](https://github.com/eric-mendoza/ejabberd_mod_offline_post) 
