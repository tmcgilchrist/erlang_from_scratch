-module(erlio_store).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,
         handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%% =========================================================================================
%% API functions
%% =========================================================================================
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, erlio_store}, ?MODULE, [], []).

%% =========================================================================================
%% gen_server Callbacks
%% =========================================================================================
init([]) ->
    {ok, #state{}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
