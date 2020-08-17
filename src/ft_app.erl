-module(ft_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  pg:start_link(),
  ft_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
  ok.

