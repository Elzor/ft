-module(consumer_SUITE).

-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(RL, 500).

all() ->
  [
    {group, main}
  ].

groups() ->
  [
    {main,
      [shuffle],
      [is_prime, flow]}
  ].


%% =============================================================================
%% init
%% =============================================================================
init_per_suite(_) ->
  mock_redis_server:start(),
  application:load(ft),
  application:set_env(ft, run_consumer, true),
  application:start(ft),
  [].

%% =============================================================================
%% end
%% =============================================================================
end_per_suite(_) ->
  application:stop(ft),
  mock_redis_server:stop(),
  ok.


%% =============================================================================
%% group: main
%% =============================================================================
is_prime(_) ->
  CheckList = [
    {-1, false},
    {0, false},
    {1, false},
    {2, true},
    {3, true},
    {4, false},
    {5, true},
    {6, false},
    {7, true},
    {8, false},
    {9, false},
    {10, false},
    {11, true},
    {12, false},
    {13, true},
    {14, false},
    {15, false}
  ],
  [true = ft_consumer:is_prime(N) =:= Res || {N, Res} <- CheckList].

flow(_) ->
  {ok, _} = wait_and_check(300, 10,
    fun() ->
      {ok, Len} = mock_redis_server:q(mock, ["LLEN", "gen"]),
      true = 10 < Len,
      {ok, ResLen} = mock_redis_server:q(mock, ["HLEN", "res"]),
      true = 0 < ResLen
    end).

%%%===================================================================
%%% internal functions
%%%===================================================================
wait_and_check(_, 0, _) ->
  timeout;
wait_and_check(OperationTimeout, Times, Func) ->
  case catch Func() of
    true -> {ok, Times};
    ok -> {ok, Times};
    {error, Descr} -> {error, Descr};
    _ ->
      timer:sleep(OperationTimeout),
      wait_and_check(OperationTimeout, Times - 1, Func)
  end.