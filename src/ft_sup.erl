-module(ft_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-spec start_link()
      -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([])
      -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupervisorSpecification = #{
    strategy => one_for_all,
    intensity => 10,
    period => 10
  },

  ChildShutdownTimeout = 5000,

  ChildrenSpecification = [
    #{id => consumer,
      start => {ft_consumer, start_link, [#{}]},
      restart => permanent,
      shutdown => ChildShutdownTimeout,
      type => worker,
      modules => [ft_consumer]
    },
    #{id => producer,
      start => {ft_producer, start_link, [#{}]},
      restart => permanent,
      shutdown => ChildShutdownTimeout,
      type => worker,
      modules => [ft_producer]
    }
  ],

  {ok, {SupervisorSpecification, ChildrenSpecification}}.
