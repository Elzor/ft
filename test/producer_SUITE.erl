-module(producer_SUITE).

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
      [init, sec3]}
  ].


%% =============================================================================
%% init
%% =============================================================================
init_per_suite(_) ->
  mock_redis_server:start(),
  application:load(ft),
  application:set_env(ft, run_consumer, false),
  [].

init_per_testcase(_Testcase, Config) ->
  application:stop(ft),
  mock_redis_server:q(mock, ["DEL", "gen"]),
  Config.

%% =============================================================================
%% end
%% =============================================================================
end_per_suite(_) ->
  mock_redis_server:stop(),
  ok.


%% =============================================================================
%% group: main
%% =============================================================================
init(_) ->
  application:start(ft),
  timer:sleep(1000),
  {ok, Len} = mock_redis_server:q(mock, ["LLEN", "gen"]),
  true = check_limit(?RL, Len),
  application:start(ft),
  timer:sleep(1000),
  application:stop(ft),
  {ok, Len2} = mock_redis_server:q(mock, ["LLEN", "gen"]),
  true = check_limit(?RL * 2, Len2),
  ok.

sec3(_) ->
  application:start(ft),
  timer:sleep(1000),
  {ok, Len} = mock_redis_server:q(mock, ["LLEN", "gen"]),
  true = check_limit(?RL, Len),
  timer:sleep(1000),
  {ok, Len2} = mock_redis_server:q(mock, ["LLEN", "gen"]),
  true = check_limit(?RL * 2, Len2),
  timer:sleep(1000),
  {ok, Len3} = mock_redis_server:q(mock, ["LLEN", "gen"]),
  true = check_limit(?RL * 3, Len3),
  ok.

%%%===================================================================
%%% internal functions
%%%===================================================================
check_limit(Limit, Value) ->
  (Limit - Limit * 0.05) < Value andalso Value < (Limit + Limit * 0.05).