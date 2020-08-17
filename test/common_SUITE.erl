-module(common_SUITE).

-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    {group, app_checks}
  ].

groups() ->
  [
    {app_checks,
      [parallel, shuffle],
      [module_load, start_stop]}
  ].


%% =============================================================================
%% init
%% =============================================================================
init_per_suite(_) ->
  mock_redis_server:start(),
  [].

init_per_group(_Group, Config) ->
  [{init, true} | Config].


%% =============================================================================
%% end
%% =============================================================================
end_per_group(_Group, _Config) ->
  ok.

end_per_suite(_) ->
  mock_redis_server:stop(),
  ok.


%% =============================================================================
%% group: common_app_checks
%% =============================================================================
module_load(_) ->
  {module, ft_app} = code:load_file(ft_app).

start_stop(_) ->
  ok = application:start(ft),
  timer:sleep(150),
  {error, {already_started, ft}} = application:start(ft),
  ok = application:stop(ft).
