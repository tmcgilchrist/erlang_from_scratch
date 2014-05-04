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
    Key = get_key(ReqData),
    {erlio_store:link_exists(Key), ReqData, State}.

moved_temporarily(ReqData, State) ->
    Key = get_key(ReqData),
    {ok, Link} = erlio_store:lookup_link(Key),
    Url = binary_to_list(proplists:get_value(url, Link)),
    {{halt, 302},
     wrq:set_resp_header("Location", Url, ReqData),
     State}.

%% Extract key from request path
get_key(ReqData) ->
    binary_to_list(iolist_to_binary(remove_slash(wrq:path(ReqData)))).

%% Remove slash from path
remove_slash(Path) ->
    re:replace(Path, "^\/", "").
