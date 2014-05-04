-module(erlio_short_resource).

%% webmachine callbacks
-export([init/1, resource_exists/2, previously_existed/2, moved_temporarily/2]).

%% API
-export([routes/0]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

%% =========================================================================================
%% API functions
%% =========================================================================================

routes() ->
    [{['*'], ?MODULE, []}].

%% =========================================================================================
%% webmachine Callbacks
%% =========================================================================================

init([]) ->
    {ok, #context{}}.

resource_exists(ReqData, State) ->
    {false, ReqData, State }.

previously_existed(ReqData, State) ->
    {true, ReqData, State}.

moved_temporarily(_ReqData, State) ->
    {{halt, 302}, "link", State}.
