%%%-------------------------------------------------------------------
%%% Created : 02 Nov 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This file contains
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ebanx_server).

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

%% cowboy callbacks
-export([init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         allow_missing_post/2
        ]).

% user callbacks API
-export([provided_operation_to_html/2]).
-export([accepted_operation_to_json/2]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%% Rest Methods
-define(REST_GET,  <<"GET">>).
-define(REST_POST, <<"POST">>).

%% Content Types
-define(CONTENT_HTML, <<"text/html">>).
-define(CONTENT_JSON, <<"application/json">>).
-define(CONTENT_TEXT, <<"text/plain">>).

%%%===================================================================
%%% COWBOY system callbacks implementation
%%%===================================================================

allowed_methods(Req0, State) ->
  Methods = [?REST_GET, ?REST_POST],
  {Methods, Req0, State}.

content_types_provided(Req0, State) ->
  {[
     {?CONTENT_HTML, provided_operation_to_html}
   ], Req0, State}.

content_types_accepted(Req0, State) ->
  {[
     {?CONTENT_JSON, accepted_operation_to_json}
  ], Req0, State}.

allow_missing_post(Req0, State) ->
  {false, Req0, State}.

%%%===================================================================
%%% COWBOY local callbacks implementation
%%%===================================================================

%% Execute Operation for html request
provided_operation_to_html(Req0, #{operation := reset} = State) ->
  ?LOG_INFO("HTTP Reset Operation"),
  ok = ebanx_bank:reset(),
  {<<"">>, Req0, State};

provided_operation_to_html(Req0, #{operation := get,
                                   account_id := Id} = State) ->
  ?LOG_INFO("HTTP Get Operation for account_id: ~p", [Id]),
  {ok, Balance} = ebanx_bank:get_balance(l2b(Id)),
  {Balance, Req0, State};

provided_operation_to_html(Req0, #{operation := help} = State) ->
  ?LOG_INFO("HTTP Help Operation"),
  Body = <<"<html>
  <head>
    <meta charset=\"utf-8\">
    <title>REST EBANX!</title>
  </head>
  <body>
    <p>REST EBANX as HTML!</p>
  </body>
  </html>">>,
  {Body, Req0, State}.

%% Execute Operation for json application
accepted_operation_to_json(Req0, #{operation := update,
                                   post_map := PostMap} = State) ->
  ?LOG_INFO("HTTP Post Operation for ~p", [PostMap]),
  Res = ebanx_bank:update(PostMap),
  Req1 = cowboy_req:set_resp_body(Res, Req0),
  cowboy_req:reply(?HMTL_OK_CREATED, Req1).

%% Check resource exist
resource_exists(Req0, #{operation := reset} = State) ->
  {true, Req0, State};

resource_exists(Req0, #{operation := update} = State) ->
  %% Extract body
  {ok, [{MapBin,true}], Req1} = cowboy_req:read_urlencoded_body(Req0),
  %% Check method and json convertion
  case { cowboy_req:method(Req1), maybe_json_decode(MapBin) } of
    {?REST_POST, {ok, Map}} ->
        %% TODO: check if resource exist in case of transfer
        Status = ebanx_bank:account_exist(Map),
        ?LOG_INFO("HTTP Update Status: ~p", [Status]),
        {Status, Req1, State#{post_map := Map}};
    _ ->
        ?LOG_ERROR("HTTP Invalid method or received json"),
        {false, Req1, State}
  end;

resource_exists(Req0, #{operation := get} = State) ->
  case { cowboy_req:method(Req0), maybe_cowboy_match_qs([account_id], Req0) } of
    { ?REST_GET, #{account_id := Id} } ->
        %% TODO: check if resource exist
        Status = ebanx_bank:account_exist(Id),
        {Status, Req0, State#{account_id := b2l(Id)}};
    _ -> ?LOG_ERROR("HTTP Invalid method or received json"),
        {false, Req0, State}
  end;

resource_exists(Req0, #{operation := help} = State) ->
  {true, Req0, State}.

%%%===================================================================
%%% API
%%%===================================================================

init(Req0, Opts) ->
  logger:set_module_level(?MODULE, error),
  [Op | _] = Opts,
  State = #{operation => Op, post_map => undefined, account_id => undefined},
  {cowboy_rest, Req0, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Try to decode the query
%%
%% @param List List with options
%% @param Req request
%% @end
%%--------------------------------------------------------------------
maybe_cowboy_match_qs(List, Req) ->
  try cowboy_req:match_qs(List, Req) of
    Res -> Res
  catch
    _:_ ->
        undefined
  end.

%%--------------------------------------------------------------------
%% @doc Try to decode json string
%%
%% @param String String to be decoded
%% @end
%%--------------------------------------------------------------------
-spec maybe_json_decode(string()) -> {ok | error, map() | json}.
maybe_json_decode(Str) ->
  try jsone:decode(Str) of
    DecodedStr -> {ok, DecodedStr}
  catch
    _:_ ->
        {error, json}
  end.

b2l(Bin) ->
  erlang:binary_to_list(Bin).

l2b(Bin) ->
  erlang:list_to_binary(Bin).
