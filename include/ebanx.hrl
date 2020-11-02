%%%-------------------------------------------------------------------
%%% Created : 02 Nov 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc Definitions for the whole application
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ebanx).
-define(ebanx, true).

%%%===================================================================
%%% Global Defines
%%%===================================================================

%% Json Info from http post request
-define(OP_TYPE,     <<"type">>).
-define(OP_DEPOSIT,  <<"deposit">>).
-define(OP_WITHDRAW, <<"withdraw">>).
-define(OP_TRANSFER, <<"transfer">>).
-define(ACC_ORIGIN,  <<"origin">>).
-define(ACC_DEST,    <<"destination">>).
-define(ACC_BALANCE, <<"balance">>).
-define(ACC_AMOUNT,  <<"amount">>).
-define(ACC_ID,      <<"id">>).

% Html return values
-define(HMTL_BAD_REQUEST,   400). %% Bad Request
-define(HMTL_NOT_FOUND,     404). %% Not Found
-define(HMTL_OK,            200). %% The request is OK
-define(HMTL_OK_CREATED,    201). %% The request is OK, created
-define(HMTL_OK_NO_CONTENT, 204). %% Succcess, but is not returning any content

-endif. %% ebanx
