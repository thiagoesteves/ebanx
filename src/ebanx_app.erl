%%%-------------------------------------------------------------------
%% @doc ebanx_app public API
%% @end
%%%-------------------------------------------------------------------

-module(ebanx_app).

-behaviour(application).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start/2, stop/1]).

%%====================================================================
%% API functions implementation
%%====================================================================

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
            {"/",        ebanx_server, [help]},
            {"/event",   ebanx_server, [update]},
            {"/balance", ebanx_server, [get]},
            {"/reset",   ebanx_server, [reset]}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
    env => #{dispatch => Dispatch}
  }),
  ebanx_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http).

%%====================================================================
%% Internal functions
%%====================================================================
