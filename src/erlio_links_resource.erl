-module(erlio_links_resource).

%% webmachine callbacks
-export([init/1, allowed_methods/2, post_is_create/2, content_types_accepted/2,
         allow_missing_post/2, create_path/2, from_json/2]).

%% API
-export([routes/0]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

%% =========================================================================================
%% API functions
%% =========================================================================================

routes() ->
    [{["links"], ?MODULE, []}].

%% =========================================================================================
%% webmachine Callbacks
%% =========================================================================================

init([]) ->
    {ok, #context{}}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

create_path(ReqData, Context) ->
    _Attributes = wrq:req_body(ReqData),
    Resource = "",
    {Resource, ReqData, Context}.

from_json(ReqData, Context) ->
    {true, ReqData, Context}.

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.
