%%%-------------------------------------------------------------------
%%% Created : 02 Nov 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%% This is the EBANX top level supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(ebanx_sup).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(BALANCE_SERVER, ebanx_bank).

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 4,
               period => 30},
  % Balance serve creation
  ChildSpecs = [#{id => ?BALANCE_SERVER,
                  start => {?BALANCE_SERVER, start_link, []},
                  restart => permanent,
                  type => worker,
                  shutdown => brutal_kill} ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
