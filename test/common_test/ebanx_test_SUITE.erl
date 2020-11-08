%%%-------------------------------------------------------------------
%%% Created : 02 Nov 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc Test suite file
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ebanx_test_SUITE).

%%%===================================================================
%%% Includes
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ebanx.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%%%===================================================================
%%% local Defines
%%%===================================================================

-define(HELP_MSG, "<html>\n  <head>\n    <meta charset=\"utf-8\">\n    <title>REST EBANX!</title>\n  </head>\n  <body>\n    <p>REST EBANX as HTML!</p>\n  </body>\n  </html>").

-define(APP_JSON_TEXT,   "application/json").

-define(OP_TYPE_TEXT,    "type").
-define(OP_DEPOSIT_TEXT, "deposit").
-define(OP_WITHDRAW_TEXT,"withdraw").
-define(OP_TRANSFER_TEXT,"transfer").
-define(ACC_ORIGIN_TEXT, "origin").
-define(ACC_DEST_TEXT,   "destination").
-define(ACC_BALANCE_TEXT,"balance").
-define(ACC_AMOUNT_TEXT, "amount").
-define(ACC_ID_TEXT,     "id").

-define(LOCAL_URL(CMD), "http://localhost:8080" ++ CMD).

%%%===================================================================
%%% Test exports
%%%===================================================================
-export([ebanx_ok/1,
         ebanx_get_empty_ok/1,
         ebanx_get_balance_ok/1,
         ebanx_create_account_ok/1,
         ebanx_double_deposit_ok/1,
         ebanx_transfer_amount_ok/1,
         ebanx_transfer_amount_origin_error/1,
         ebanx_transfer_amount_dest_non_exist_ok/1,
         ebanx_reset_ok/1,
         ebanx_withdraw_ok/1,
         ebanx_withdraw_error/1,
         ebanx_invalid_operation_error/1,
         ebanx_invalid_json_error/1,
         ebanx_bank_full_coverage_ok/1
         ]).

all() -> [ebanx_ok,
          ebanx_get_empty_ok,
          ebanx_get_balance_ok,
          ebanx_create_account_ok,
          ebanx_double_deposit_ok,
          ebanx_transfer_amount_ok,
          ebanx_transfer_amount_origin_error,
          ebanx_transfer_amount_dest_non_exist_ok,
          ebanx_reset_ok,
          ebanx_withdraw_ok,
          ebanx_withdraw_error,
          ebanx_invalid_operation_error,
          ebanx_invalid_json_error,
          ebanx_bank_full_coverage_ok
         ].

%%%===================================================================
%%% init_per_suite:  Contains common initializations for all test
%%%                  cases in the suite
%%%===================================================================
init_per_suite(Config) ->
  inets:start(),
  Config.

%%%===================================================================
%%% end_per_suite: It is called as the final stage of the test suite
%%%                execution
%%%===================================================================
end_per_suite(_Config) ->
  inets:stop(),
  ok.

%%%===================================================================
%%% init_per_testcase: It is called before each test case in the suite.
%%%                    Contains initialization that must be done for
%%%                    each test case.
%%%
%%% @param Name    Name of the test case
%%% @param Config  Config key-value list of runtime configuration data,
%%%                which has the same value as the list returned by
%%%                init_per_suite
%%%===================================================================
init_per_testcase(_, Config) ->
  %% Start the Server
  application:ensure_all_started(ebanx),

  %% Reset
  httpc:request(post, {?LOCAL_URL("/reset"), [], ?APP_JSON_TEXT, []}, [], []),

  Config.

%%%===================================================================
%%% end_per_testcase: It is called after each test case has finished,
%%%                   enabling cleanup after init_per_testcase
%%%===================================================================
end_per_testcase(_, _Config) ->
  meck:unload(),
  ok.

%%%===================================================================
%%%           Test case functions: Waiting for OK result
%%%===================================================================

%%%===================================================================
%%% Function: ebanx_ok
%%%
%%% Description: Test the start/stop of the application
%%%===================================================================
ebanx_ok(_Config) ->
  %% Start the Server
  application:ensure_all_started(ebanx),

  %% Check the server is still running
  ?assertNotEqual( undefined, try_get_state(ebanx_sup) ),

  %% Stop Server
  application:stop(ebanx),

  %% Check the server is not running anymore
  ?assertMatch( undefined, try_get_state(ebanx_sup) ),

  ok.

%%%===================================================================
%%% Function: ebanx_get_empty_ok
%%%
%%% Description: Test the GET method with no parameters in the path
%%%===================================================================
ebanx_get_empty_ok(_Config) ->

  Request = {?LOCAL_URL("/"), []},
  {ok, {{_Version, ?HMTL_OK, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, Request, [], []),

  %% Check the retun
  ?assertEqual( ?HELP_MSG, Body ),
  ok.

%%%===================================================================
%%% Function: ebanx_get_balance_ok
%%%
%%% Description: Test the GET method requesting balance
%%%===================================================================
ebanx_get_balance_ok(_Config) ->

  %% Create Account
  Info  = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"5000">>,
                          ?ACC_AMOUNT => 300} ),
  RequestEvent = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info},
  %% Execute POST
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestEvent, [], []),

  Request =  {?LOCAL_URL("/balance?account_id=5000"), []},
  {ok, {{_Version, ?HMTL_OK, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, Request, [], []),

  %% Check the return
  ?assertEqual( "300", Body),
  ok.

%%%===================================================================
%%% Function: ebanx_create_account_ok
%%%
%%% Description: Test the POST method creating an account
%%%===================================================================
ebanx_create_account_ok(_Config) ->

  %% Check Account doesn't exist
  RequestAcc = {?LOCAL_URL("/balance?account_id=100"), []},
  {ok, {{_, HTTP_ERR, _}, _, _}} =
        httpc:request(get, RequestAcc, [], []),
  ?assertEqual( HTTP_ERR, ?HMTL_NOT_FOUND),

  %% Create account
  Info  = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"100">>,
                          ?ACC_AMOUNT => 10} ),
  RequestCreate = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestCreate, [], []),

  %% Check Account exist and balance
  RequestAcc1 = {?LOCAL_URL("/balance?account_id=100"), []},
  {ok, {{_, ?HMTL_OK, _}, _, Body}} =
        httpc:request(get, RequestAcc1, [], []),

  %% Check balance
  ?assertEqual( "10", Body),
  ok.

%%%===================================================================
%%% Function: ebanx_double_deposit_ok
%%%
%%% Description: Test the POST method creating an account and making
%%%              a double deposit
%%%===================================================================
ebanx_double_deposit_ok(_Config) ->

  %% Check Account doesn't exist
  RequestAcc = {?LOCAL_URL("/balance?account_id=50"), []},
  {ok, {{_, HTTP_ERR, _}, _, _}} =
        httpc:request(get, RequestAcc, [], []),
  ?assertEqual( HTTP_ERR, ?HMTL_NOT_FOUND),

  %% Create account
  Info  = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"50">>,
                          ?ACC_AMOUNT => 200} ),
  RequestCreate = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info},

  %% Execute double deposit
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, "{\"destination\":{\"balance\":200,\"id\":\"50\"}}"}} =
        httpc:request(post, RequestCreate, [], []),

  {ok, {{_, ?HMTL_OK_CREATED, _}, _, "{\"destination\":{\"balance\":400,\"id\":\"50\"}}"}} =
        httpc:request(post, RequestCreate, [], []),

  %% Check Account exist and balance
  Request = {?LOCAL_URL("/balance?account_id=50"), []},
  ?assertMatch( {ok, {{_, ?HMTL_OK, _}, _, "400"}} ,
                httpc:request(get, Request, [], [])),

  ok.

%%%===================================================================
%%% Function: ebanx_transfer_amount_ok
%%%
%%% Description: Test the POST method transfering money from one to another
%%%===================================================================
ebanx_transfer_amount_ok(_Config) ->

  %% Create accounts
  Info1 = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"800">>,
                          ?ACC_AMOUNT => 120} ),
  Info2 = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"300">>,
                          ?ACC_AMOUNT => 100} ),
  RequestCreate1 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info1},
  RequestCreate2 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info2},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestCreate1, [], []),
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestCreate2, [], []),

  %% Check Account exist and balances
  ?assertMatch( {ok, {{_, ?HMTL_OK, _}, _, "120"}} ,
                httpc:request(get,
                          {?LOCAL_URL("/balance?account_id=800"), []}, [], [])),

  ?assertMatch( {ok, {{_, ?HMTL_OK, _}, _, "100"}} ,
                httpc:request(get,
                          {?LOCAL_URL("/balance?account_id=300"), []}, [], [])),

  %% Execute transfer
  Info3 = jsone:encode( #{?OP_TYPE    => ?OP_TRANSFER,
                          ?ACC_ORIGIN => <<"300">>,
                          ?ACC_DEST   => <<"800">>,
                          ?ACC_AMOUNT => 95} ),
  RequestTransfer = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info3},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, "{\"destination\":{\"balance\":215,\"id\":\"800\"},\"origin\":{\"balance\":5,\"id\":\"300\"}}"}} =
        httpc:request(post, RequestTransfer, [], []),

  %% Check Account exist and balances
  ?assertMatch( {ok, {{_, ?HMTL_OK, _}, _, "215"}} ,
                httpc:request(get,
                          {?LOCAL_URL("/balance?account_id=800"), []}, [], [])),

  ?assertMatch( {ok, {{_, ?HMTL_OK, _}, _, "5"}} ,
                httpc:request(get,
                          {?LOCAL_URL("/balance?account_id=300"), []}, [], [])),

  ok.

%%%===================================================================
%%% Function: ebanx_transfer_amount_origin_error
%%%
%%% Description: Test the POST method transfering money for an invalid
%%%              origin account
%%%===================================================================
ebanx_transfer_amount_origin_error(_Config) ->

  %% Create only one account
  Info1 = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"700">>,
                          ?ACC_AMOUNT => 120} ),
  RequestCreate1 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info1},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestCreate1, [], []),

  %% Execute transfer
  Info2 = jsone:encode( #{?OP_TYPE    => ?OP_TRANSFER,
                          ?ACC_ORIGIN => <<"600">>,
                          ?ACC_DEST   => <<"700">>,
                          ?ACC_AMOUNT => 95} ),
  RequestTransfer = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info2},
  {ok, {{_, HTTP_ERR, _}, _, _}} =
        httpc:request(post, RequestTransfer, [], []),

  %% Check error
  ?assertEqual( ?HMTL_NOT_FOUND, HTTP_ERR),

  ok.

%%%===================================================================
%%% Function: ebanx_transfer_amount_dest_non_exist_ok
%%%
%%% Description: Test the POST method transfering money for an non existent
%%%              destination account
%%%===================================================================
ebanx_transfer_amount_dest_non_exist_ok(_Config) ->

  %% Create only one account
  Info1 = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"833">>,
                          ?ACC_AMOUNT => 120} ),
  RequestCreate1 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info1},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestCreate1, [], []),

  %% Execute transfer
  Info2 = jsone:encode( #{?OP_TYPE    => ?OP_TRANSFER,
                          ?ACC_ORIGIN => <<"833">>,
                          ?ACC_DEST   => <<"123">>,
                          ?ACC_AMOUNT => 95} ),
  RequestTransfer = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info2},

%% Check success
  ?assertMatch( {ok, {{_, ?HMTL_OK_CREATED, _}, _,
  "{\"destination\":{\"balance\":95,\"id\":\"123\"},\"origin\":{\"balance\":25,\"id\":\"833\"}}"}},
        httpc:request(post, RequestTransfer, [], []) ),

  ok.

%%%===================================================================
%%% Function: ebanx_reset_ok
%%%
%%% Description: Test the RESET request
%%%===================================================================
ebanx_reset_ok(_Config) ->

  %% Check Account doesn't exist
  RequestAcc = {?LOCAL_URL("/balance?account_id=505"), []},
  {ok, {{_, HTTP_ERR, _}, _, _}} =
        httpc:request(get, RequestAcc, [], []),
  ?assertEqual( HTTP_ERR, ?HMTL_NOT_FOUND),

  %% Create account
  Info  = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"505">>,
                          ?ACC_AMOUNT => 10} ),
  RequestCreate = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestCreate, [], []),

  %% Check Account exist and balance
  RequestAcc1 = {?LOCAL_URL("/balance?account_id=505"), []},
  {ok, {{_, ?HMTL_OK, _}, _, Body}} =
        httpc:request(get, RequestAcc1, [], []),

  %% Check balance
  ?assertEqual( "10", Body),

  %% Reset and check account doesn't exist anymore
  {ok, {{_, ?HMTL_OK, _}, _, _}} =
    httpc:request(post, {?LOCAL_URL("/reset"), [], ?APP_JSON_TEXT, []}, [], []),

  ?assertMatch( {ok, {{_, ?HMTL_NOT_FOUND, _}, _, _}} ,
                httpc:request(get, RequestAcc, [], [])),
  ok.

%%%===================================================================
%%% Function: ebanx_withdraw_ok
%%%
%%% Description: Test the POST method withdrawing money
%%%===================================================================
ebanx_withdraw_ok(_Config) ->

  %% Create only one account
  Info1 = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"5555">>,
                          ?ACC_AMOUNT => 120} ),
  RequestCreate1 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info1},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestCreate1, [], []),

  Request =  {?LOCAL_URL("/balance?account_id=5555"), []},
  ?assertMatch( {ok, {{_, ?HMTL_OK, _}, _, "120"}} ,
                httpc:request(get, Request, [], [])),

  %% Withdraw some money
  Info2 = jsone:encode( #{?OP_TYPE    => ?OP_WITHDRAW,
                          ?ACC_ORIGIN => <<"5555">>,
                          ?ACC_AMOUNT => 220} ),
  RequestCreate2 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info2},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, "{\"origin\":{\"balance\":-100,\"id\":\"5555\"}}"}} =
        httpc:request(post, RequestCreate2, [], []),

  ?assertMatch( {ok, {{_, ?HMTL_OK, _}, _, "-100"}} ,
                httpc:request(get, Request, [], [])),

  ok.

%%%===================================================================
%%% Function: ebanx_withdraw_error
%%%
%%% Description: Test the POST method withdrawing money from a non-existing
%%%              account
%%%===================================================================
ebanx_withdraw_error(_Config) ->

  %% Withdraw some money from a non existent account (Robbery)
  Info2 = jsone:encode( #{?OP_TYPE    => ?OP_WITHDRAW,
                          ?ACC_ORIGIN => <<"6666">>,
                          ?ACC_AMOUNT => 220} ),
  RequestCreate2 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info2},

  ?assertMatch( {ok, {{_, ?HMTL_NOT_FOUND, _}, _, _}}  ,
                httpc:request(post, RequestCreate2, [], [])),

  ok.

%%%===================================================================
%%% Function: ebanx_invalid_operation_error
%%%
%%% Description: Test the POST method with a invalid operation
%%%===================================================================
ebanx_invalid_operation_error(_Config) ->

  %% Create only one account
  Info1 = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"4444">>,
                          ?ACC_AMOUNT => 120} ),
  RequestCreate1 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info1},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestCreate1, [], []),

  Request =  {?LOCAL_URL("/balance?account_id=4444"), []},
  ?assertMatch( {ok, {{_, ?HMTL_OK, _}, _, "120"}} ,
                httpc:request(get, Request, [], [])),

  %% Try Invalid operation
  Info2 = jsone:encode( #{?OP_TYPE    => <<"Get All Money">>,
                          ?ACC_ORIGIN => <<"4444">>,
                          ?ACC_AMOUNT => 220} ),
  RequestCreate2 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info2},
  ?assertMatch( {ok, {{_, ?HMTL_NOT_FOUND, _}, _, _}} ,
                httpc:request(post, RequestCreate2, [], [])),

  ok.

%%%===================================================================
%%% Function: ebanx_invalid_json_error
%%%
%%% Description: Test the POST method with invalid json
%%%===================================================================
ebanx_invalid_json_error(_Config) ->

  %% Create only one account
  Info1 = "{\"type\":\"deposit\", \"destination\":100\", \"amount\":10}",
  RequestCreate1 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info1},
  ?assertMatch( {ok, {{_, ?HMTL_NOT_FOUND, _}, _, _}} ,
                httpc:request(post, RequestCreate1, [], [])),

  %% Create only one account
  Info2 = jsone:encode( #{?OP_TYPE    => ?OP_DEPOSIT,
                          ?ACC_DEST   => <<"2345">>,
                          ?ACC_AMOUNT => 120} ),
  RequestCreate2 = {?LOCAL_URL("/event"), [], ?APP_JSON_TEXT, Info2},
  {ok, {{_, ?HMTL_OK_CREATED, _}, _, _}} =
        httpc:request(post, RequestCreate2, [], []),

  Request =  {?LOCAL_URL("/balance?id=235"), []},
  ?assertMatch( {ok, {{_, ?HMTL_NOT_FOUND, _}, _, _}} ,
                httpc:request(get, Request, [], [])),

  ok.

%%%===================================================================
%%% Function: ebanx_bank_full_coverage_ok
%%%
%%% Description: This test will only guarantee 100% coverage for
%%%              ebanx_bank.erl
%%%===================================================================
ebanx_bank_full_coverage_ok(_Config) ->

  gen_server:cast(ebanx_bank, {none}),
  gen_server:call(ebanx_bank, {none}),
  erlang:send(ebanx_bank, {none}),
  ebanx_bank:code_change(none, none, none),
  gen_server:stop(ebanx_bank),

  ok.

%%--------------------------------------------------------------------
%% @doc This function try to get the state of a registered server
%%
%% @param Name Server name
%% @end
%%--------------------------------------------------------------------
try_get_state(Name)->
  try sys:get_state(Name) of
    S -> S
  catch
    _:_ -> undefined
  end.
