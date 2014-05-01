-module(erlio_app).

-behaviour(application).

-export([start/2,
         stop/1]).

%% @doc application start callback for erlio.
start(_Type, _StartArgs) ->
    erlio_sup:start_link().

%% @doc application stop callback for erlio.
stop(_State) ->
    ok.
