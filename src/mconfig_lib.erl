-module(mconfig_lib).

-export([
    master_appname/0
]).

master_appname() ->
    Node = atom_to_list(node()),
    [AppName,_] = string:tokens(Node, "@"),
    AppName.
