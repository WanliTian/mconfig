-module(mconfig_wather).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    terminate/2, 
    code_change/3,

    add_config/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_config(binary() | list()) -> ok | has_existed | not_existed.
add_config(FilePath) when is_binary(FilePath) ->
    add_config(binary_to_list(FilePath));
add_config(FilePath) ->
    gen_server:call(?SERVER, {add_config, FilePath}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(file_info, {
        file_path       :: list(),
        last_modified=0 :: file:date_time() | 0
}).

init(_) ->
    ets:new(fmon, [named_table, bag, {keypos, 1}, private]),
    List = case mconfig:config_path() of 
        error ->
            [];
        {ok, L} ->
            L
    end,
    AllPaths = lists:foldl(fun(Path, Pre) ->
            Pre ++ Path
        end, [], List),    
    FIList = lists:map(fun([FilePath]) ->
        #file_info{
            file_path = FilePath,
            last_modified = filelib:last_modified(FilePath)
        }
    end, AllPaths),

    erlang:send_after(self(), mconfig_lib:delay(), scan),

    {ok, FIList}.

handle_call({add_config, FilePath}, _From, State) ->
    IsExisted = lists:foldl(fun(#file_info{file_path=FilePath}, _Pre) ->
                true;
            (_, Pre) ->
                Pre
        end, false, State),
    case IsExisted of 
        true ->
            {reply, has_existed, State};
        false ->
            case filelib:last_modified(FilePath) of 
                error ->
                    {reply, not_existed, State};
                LastModified ->
                    {reply, ok, [#file_info{file_path=FilePath, last_modified=LastModified} | State]}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(scan, State) ->
    NewState = lists:map(fun(#file_info{file_path=Path, last_modified=Time}=FileInfo) ->
                case filelib:last_modified(Path) of 
                    %%file has been removed
                    error ->
                        lager:error("File Has Been Removed. File Path: ~p~n", [FileInfo]),
                        FileInfo;
                    %%file not been modified
                    Time ->
                        FileInfo;
                    %%file has been modifed
                    NewTime ->
                        gen_server:call(mconfig_server, {file_changed, Path}),
                        FileInfo#file_info{last_modified = NewTime}
                end

        end, State),
    erlang:send_after(self(), mconfig_lib:delay(), scan),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

sub_configs(FilePath) ->
    case file:consult(FilePath) of 
        {ok, Terms} ->
            iterate_terms(Terms, []);
        {error, Reason} ->
            lager:error("Read File Info Error. File Path: ~p~n", [FilePath]),
            []
    end.

iterate_terms([], Result) ->
    Result;
iterate_terms([T|Tail], Result) ->
    iterate_terms(Tail, search_sub_file(T, Result)).

%%keep files order for the reason that the back override the front
search_sub_file([], Result) ->
    lists:reverse(Result);
search_sub_file([Str|Tail], Result) io_lib:printable_unicode_list(Str) ->
    search_sub_file(Tail. [Str | Result]).
