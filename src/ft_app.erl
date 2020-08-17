-module(ft_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  pg:start_link(),
  case application:get_env(ft, redis_module) of
    {ok, eredis_pool} -> eredis_pool:start();
    _Else -> mocking_mode
  end,
  ft_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
  ok.

