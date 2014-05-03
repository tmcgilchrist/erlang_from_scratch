-module(erlio_store).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, lookup_link/1, link_exists/1]).

%% gen_server callbacks
-export([init/1, handle_call/3,
         handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-define(TABLE, links).

%% =========================================================================================
%% API functions
%% =========================================================================================
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, erlio_store}, ?MODULE, [], []).

lookup_link(Id) ->
    gen_server:call(erlio_store, {lookup_link, Id}).

link_exists(Id) ->
    gen_server:call(erlio_store, {link_exists, Id}).

%% =========================================================================================
%% gen_server Callbacks
%% =========================================================================================
init([]) ->
    %% Create table and setup some seed data
    _ = create_table(),
    _ = seed_links(),

    {ok, #state{}}.
handle_call({lookup_link, Id}, _From, State) ->
    case ets:lookup(?TABLE, Id) of
        [] ->
            Reply = {not_found},
            {reply, Reply, State};
        [{_Key, Link}] ->
            Reply = {ok, Link},
            {reply, Reply, State}
    end;
handle_call({link_exists, Id}, _From, State) ->
    case ets:lookup(?TABLE, Id) of
        [] ->
            {reply, false, State};
        [{_Key, _Link}] ->
            {reply, true, State}
    end;
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

%% =========================================================================================
%% private functions
%% =========================================================================================
create_table() ->
    ets:new(?TABLE, [public,
                    ordered_set,
                    named_table,
                    {read_concurrency, true},
                    {write_concurrency, true}]).

seed_links() ->
    ets:insert(?TABLE, [
                       {"0",
                        [{url, <<"http://erlang.com">>},
                         {hits, <<"0">>}]},
                       {"1",
                        [{url, <<"http://www.openbsd.org">>},
                         {hits, <<"0">>}]}
                       ]).
