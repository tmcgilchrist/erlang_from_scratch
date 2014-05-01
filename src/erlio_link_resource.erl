-module(erlio_link_resource).

%% Webmachine exports
-export([init/1, content_types_provided/2, allowed_methods/2, to_json/2, resource_exists/2]).

-export([routes/0]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {link}).

init([]) ->
    {ok, #context{}}.

routes() ->
    [{["link", link_id], ?MODULE, []}].

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    Id = wrq:path_info(link_id, ReqData),
    Response = link_exists(Id),
    {Response, ReqData, Context}.

to_json(ReqData, Context) ->
    Id = wrq:path_info(link_id, ReqData),
    {ok, Link} = lookup_link(Id),
    Response = mochijson2:encode({struct, Link}),
    {Response, ReqData, Context}.

lookup_link(Id) ->
    case ets:lookup(links, Id) of
        [] ->
            not_found;
        [{_Key, Link}] ->
            {ok, Link}
    end.

link_exists(Id) ->
    case lookup_link(Id) of
        not_found ->
            false;
        _ ->
            true
    end.
