-module(erlio_assets_resource).

-export([init/1,
         routes/0,
         to_resource/2,
         allowed_methods/2,
         generate_etag/2,
         last_modified/2,
         resource_exists/2,
         previously_existed/2,
         moved_temporarily/2,
         content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

-record(context, {filename,
                  fileinfo,
                  rendered,
                  token}).

%% @doc Initialize the resource.
-spec init([]) -> {ok, #context{}}.
init([]) ->
    {ok, #context{}}.

%% @doc Return the routes this module should respond to.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [
     {["javascripts"], ?MODULE, []},
     {["stylesheets"], ?MODULE, []},
     {["images"], ?MODULE, []}
     %% {['*'], ?MODULE, []}            %% Accept anything, but we restrict in identify_resource to only index.html
    ].

%% @doc Handle serving of the single page application.
-spec allowed_methods(wrq:reqdata(), #context{}) ->
    {list(), wrq:reqdata(), #context{}}.
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

%% @doc Generates an etag for the asset being served.
-spec generate_etag(wrq:reqdata(), #context{}) ->
                           {list(), wrq:reqdata(), #context{}}.
generate_etag(ReqData, #context{filename=template,
                                rendered=Rendered}=Context) ->
    {mochihex:to_hex(erlang:phash2(Rendered)), ReqData, Context};
generate_etag(ReqData, #context{fileinfo=FileInfo}=Context) ->
    {mochihex:to_hex(erlang:phash2(FileInfo)), ReqData, Context}.

%% @doc Determines the time the asset was last modified
-spec last_modified(wrq:reqdata(), #context{}) ->
                           {undefined | calendar:datetime(),
                            wrq:reqdata(), #context{}}.
last_modified(ReqData, #context{filename=template}=Context) ->
    {undefined, ReqData, Context};
last_modified(ReqData, #context{fileinfo={ok, #file_info{mtime=MTime}}}=Context) ->
    {MTime, ReqData, Context}.

%% @doc Given a series of request tokens, normalize to priv dir file.
-spec normalize_filepath(list()) -> list().
normalize_filepath(Filepath) ->
    {ok, App} = application:get_application(?MODULE),
    filename:join([priv_dir(App), "www"] ++ Filepath).

%% @doc Return a context which determines if we serve up the index or a
%%      particular file
-spec identify_resource(wrq:reqdata(), #context{}) ->
    {boolean(), #context{}}.
identify_resource(ReqData, #context{filename=undefined}=Context) ->
    case wrq:disp_path(ReqData) of
        [] ->
            Filename = normalize_filepath(["index.html"]),
            FileInfo = file:read_file_info(Filename),
            {true, Context#context{filename=Filename,
                                   fileinfo=FileInfo}};
        _ ->
            Tokens = wrq:path_tokens(ReqData),
            Filename = normalize_filepath(Tokens),
            FileInfo = file:read_file_info(Filename),
            {true, Context#context{filename=Filename,
                                   fileinfo=FileInfo}}
    end;
identify_resource(_ReqData, Context) ->
    {true, Context}.

%% @doc If the file exists, allow it through, otherwise assume true if
%%      they are asking for the application template.
-spec resource_exists(wrq:reqdata(), #context{}) ->
    {boolean(), wrq:reqdata(), #context{}}.
resource_exists(ReqData, Context) ->
    case identify_resource(ReqData, Context) of
        {true, NewContext=#context{filename=template}} ->
            {true, ReqData, NewContext};
        {true, NewContext=#context{filename=Filename}} ->
            case filelib:is_regular(Filename) of
                true ->
                    {true, ReqData, NewContext};
                _ ->
                    {false, ReqData, NewContext}
            end
    end.

-spec previously_existed(wrq:reqdata(), #context{}) ->
    {boolean(), wrq:reqdata(), #context{}}.
previously_existed(ReqData, State) ->
    Key = get_key(ReqData),
    {erlio_store:link_exists(Key), ReqData, State}.

-spec moved_temporarily(wrq:reqdata(), #context{}) ->
      {{halt, 302}, string(), #context{}}.
moved_temporarily(ReqData, State) ->
    Key = get_key(ReqData),
    {ok, Link} = erlio_store:lookup_link(Key),
    Url = binary_to_list(proplists:get_value(url, Link)),
    {{halt, 302},
     wrq:set_resp_header("Location", Url, ReqData),
     State}.

%% @doc Return the proper content type of the file, or default to
%%      text/html.
-spec content_types_provided(wrq:reqdata(), #context{}) ->
    {list({list(), atom()}), wrq:reqdata(), #context{}}.
content_types_provided(ReqData, Context) ->
    case identify_resource(ReqData, Context) of
        {true, NewContext=#context{filename=template}} ->
            {[{"text/html", to_resource}], ReqData, NewContext};
        {true, NewContext=#context{filename=Filename}} ->
            MimeType = webmachine_util:guess_mime(Filename),
            {[{MimeType, to_resource}], ReqData, NewContext};
        {true, NewContext} ->
            {[{"text/html", to_resource}], ReqData, NewContext}
    end.

%% @doc Return the resources content.
-spec to_resource(wrq:reqdata(), #context{}) ->
    {binary(), wrq:reqdata(), #context{}}.
to_resource(ReqData, #context{filename=template,
                              rendered=Content,
                              token=Token}=Context) ->
    {Content,
     wrq:set_resp_header("Set-Cookie",
                         "csrf_token="++Token++"; httponly", ReqData),
     Context};
to_resource(ReqData, #context{filename=Filename}=Context) ->
    {ok, Source} = file:read_file(Filename),
    {Source, ReqData, Context}.

%% @doc Extract the priv dir for the application.
-spec priv_dir(term()) -> list().
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.

%% Extract key from request path
-spec get_key(wrq:reqdata()) -> string().
get_key(ReqData) ->
    binary_to_list(iolist_to_binary(remove_slash(wrq:path(ReqData)))).

%% Remove slash from path
-spec remove_slash(string()) -> string().
remove_slash(Path) ->
    re:replace(Path, "^\/", "").
