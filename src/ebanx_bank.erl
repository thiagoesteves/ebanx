%%%-------------------------------------------------------------------
%%% Created : 02 Nov 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This file contains
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ebanx_bank).

-behaviour(gen_server).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("ebanx.hrl").
%% For LOG purposes
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% gen_server exports
-export([init/1,
         start_link/0,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

% APIs
-export([account_exist/1,
         get_balance/1,
         update/1,
         reset/0]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

-define(ACCOUNT_TOKEN, account_id).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(list()) -> {ok, map()} | ignore | {stop, term()}.
init([]) ->
  logger:set_module_level(?MODULE, info),
  %% Enable trapping exits
  process_flag(trap_exit, true),
  {ok, [] }.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call({get_balance, AccountId}, _From, State) ->
  Response = get_balance_priv(AccountId),
  {reply, Response, State};

handle_call({update, Map}, _From, State) ->
  Response = update_priv(Map),
  {reply, Response, State};

handle_call({account_exist, AccountId}, _From, State) ->
  Response = account_exist_priv(AccountId),
  {reply, Response, State};

handle_call({reset}, _From, State) ->
  %% Reset all information
  erase(),
  {reply, ok, State};

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(normal, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Public APIs
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Update all temperature information
%%
%% @end
%%--------------------------------------------------------------------
get_balance(AccountId) ->
  gen_server:call(?MODULE, {get_balance, AccountId}).

update(Map) ->
  gen_server:call(?MODULE, {update, Map}).

account_exist(AccountId) ->
  gen_server:call(?MODULE, {account_exist, AccountId}).

reset() ->
  gen_server:call(?MODULE, {reset}).

%%====================================================================
%% Internal functions
%%====================================================================

get_balance_priv(AccountId) ->
  %% Get Balance, check were already done by resource_exist
  ?LOG_INFO("Get Balance from Account: ~p", [AccountId]),
  Balance = get( {?ACCOUNT_TOKEN, AccountId} ),
  {ok, i2b(Balance)}.

update_priv(#{?OP_TYPE   := ?OP_DEPOSIT,
              ?ACC_DEST  := Dest,
              ?ACC_AMOUNT:= Amount} = _Map) ->
  ?LOG_INFO("Deposit - Dest: ~p, Amount: ~p", [Dest, Amount]),
  %% Check Account Exist, if not, create one
  FinalBalance = case get({?ACCOUNT_TOKEN, Dest}) of
    undefined -> put({?ACCOUNT_TOKEN, Dest}, Amount),
                 Amount;
    Balance   -> Val = Balance+Amount,
                 put({?ACCOUNT_TOKEN, Dest}, Val),
                 Val
  end,
  jsone:encode(
    #{?ACC_DEST => #{ <<"id">> => Dest, <<"balance">> => FinalBalance} } );

update_priv(#{?OP_TYPE    := ?OP_WITHDRAW,
              ?ACC_ORIGIN := Origin,
              ?ACC_AMOUNT := Amount} = _Map) ->
  ?LOG_INFO("Withdraw - Dest: ~p, Amount: ~p", [Origin, Amount]),
  %% Withdraw monwy, check were already done by resource_exist
  Balance = get({account_id, Origin}),
  FinalBalance = Balance-Amount,
  put({?ACCOUNT_TOKEN, Origin}, Balance-Amount),
  jsone:encode(
    #{?ACC_ORIGIN => #{ <<"id">> => Origin, <<"balance">> => FinalBalance} } );

update_priv(#{?OP_TYPE    := ?OP_TRANSFER,
              ?ACC_DEST   := Dest,
              ?ACC_AMOUNT := Amount,
              ?ACC_ORIGIN := Origin} = _Map) ->
  ?LOG_INFO("Transfer - Origin: ~p, Dest: ~p, Amount: ~p", [Origin, Dest, Amount]),
  %% Update, checks were already done by resource_exist
  O = get({?ACCOUNT_TOKEN, Origin}) - Amount,
  D = get({?ACCOUNT_TOKEN, Dest}) + Amount,
  put({?ACCOUNT_TOKEN, Origin}, O),
  put({?ACCOUNT_TOKEN, Dest},   D),
  jsone:encode(
    #{?ACC_ORIGIN => #{ <<"id">> => Origin, <<"balance">> => O},
      ?ACC_DEST   => #{ <<"id">> => Dest,   <<"balance">> => D} } ).

account_exist_priv(#{?OP_TYPE    := ?OP_TRANSFER,
                     ?ACC_DEST   := Dest,
                     ?ACC_ORIGIN := Origin} = _Map) ->
  ?LOG_INFO("Check Accounts exist for transfer: ~p ~p", [Dest, Origin]),
  %% Check Account Exist
  case { get({?ACCOUNT_TOKEN, Origin}), get({?ACCOUNT_TOKEN, Dest}) } of
    {undefined, _} -> false;
    {_, undefined} -> false;
    _   -> true
  end;

account_exist_priv(#{?OP_TYPE := ?OP_DEPOSIT} = _Map) ->
  ?LOG_INFO("No check is needed for a deposit"),
  true;

account_exist_priv(#{?OP_TYPE    := ?OP_WITHDRAW,
                     ?ACC_ORIGIN := Origin} = _Map) ->
  ?LOG_INFO("Check Accounts exist for withdraw: ~p", [Origin]),
  %% Check Account Exist
  case get({?ACCOUNT_TOKEN, Origin}) of
    undefined -> false;
    _         -> true
  end;

account_exist_priv(#{?OP_TYPE := Op} = _Map) ->
  ?LOG_INFO("Invalid Operation received: ~p", [Op]),
  false;

account_exist_priv(AccountId) ->
  ?LOG_INFO("Check Account exist: ~p", [AccountId]),
  %% Check Account Exist
  case get({?ACCOUNT_TOKEN, AccountId}) of
    undefined -> false;
    _   -> true
  end.

i2b(Int) ->
  erlang:integer_to_binary(Int).


