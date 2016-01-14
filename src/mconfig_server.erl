-module(mconfig_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    desub/2,
    sub/3
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

sub(App, Key, Callback) ->
    gen_server:cast(?SERVER, {sub, App, Key, Callback}).

desub(App, Key) ->
    gen_server:cast(?SERVER, {desub, App, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    FInfos = file_info(),
    ets:new(mconfig_subs, [set, {keypos, 1}, private, named_table]),
    tick(),
    {ok, FInfos}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({sub, App, Key, Callback}, State) ->
    Value = application:get_env(App, Key, undefined),
    ets:insert_new(mconfig_subs, {{App, Key}, Value, Callback}),
    {noreply, State};

handle_cast({desub, App, Key}, State) ->
    ets:delete(mconfig_subs, {App, Key}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State=[]) ->
    tick(),
    {noreply, State};
handle_info(tick, FInfos) ->
    NewFInfos = case need_reload(FInfos) of 
        false ->
            FInfos;
        true ->
            corman:reload(),
            update_subs(),
            file_info()
    end,
    tick(),
    {noreply, NewFInfos};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
need_reload([]) ->
    false;
need_reload([{Path, LastModified} | Tail]) ->
    case filelib:last_modified(Path) of 
        0 ->
            need_reload(Tail);
        LastModified ->
            need_reload(Tail);
        _ ->
            true
    end.

update_subs() ->
    Key = ets:first(mconfig_subs),
    update_subs(Key).

update_subs('$end_of_table') ->
    nop;
update_subs(Key={App, EKey}) ->
    [{Key, OldValue, Callback}] = ets:lookup(mconfig_subs, Key),
    case application:get_env(App, EKey, OldValue) of 
        OldValue ->
            nop;
        NewValue ->
            Callback ! {App, EKey, OldValue, NewValue},
            ets:update_element(mconfig_subs, Key, [{2, NewValue}])
    end,
    Next = ets:next(mconfig_subs, Key),
    update_subs(Next).

file_info() ->
    Paths = app_path() ++ config_path(),
    file_info(Paths, []).

file_info([], Result) ->
    Result;
file_info([Path|Tail], Result) ->
    Info = {Path, filelib:last_modified(Path)},
    file_info(Tail, [Info|Result]).

app_path() ->
    [code:where_is_file(mconfig_lib:master_appname() ++ ".app")].
config_path() ->
    {ok, [List]} = init:get_argument(config),
    List.

tick() ->
    Tick = application:get_env(mconfig, tick, 60000),
    erlang:send_after(Tick, self(), tick).
