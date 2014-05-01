-module(erlio_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    %% Setup Webmachine
    Ip =  "0.0.0.0",
    Dispatch = load_wm_resources(),
    WebConfig = [ {ip, Ip},
                  {port, 8000},
                  {log_dir, "priv/log"},
                  {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},

    Children = [Web],

    {ok, {{one_for_one, 1, 1}, Children}}.

%% Load webmachine routes directly from each resource
load_wm_resources() ->
    Resources = [erlio_assets_resource],
    lists:flatten([Module:routes() || Module <- Resources]).
