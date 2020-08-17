-module(ft_producer).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(JITTER, 5).

%%--------------------------------------------------------------------
%% @doc
%% Starts producer
%% @end
%%--------------------------------------------------------------------
-spec start_link(term()) ->
  {ok, pid()}
  | ignore
  | {error, term()}.
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Params :: map()) ->
  {ok, State :: term()}
  | {ok, State :: term(), Timeout :: non_neg_integer()}
  | ignore
  | {stop, Reason :: term()}.
init(_Args) ->
  {ok, RedisModule} = application:get_env(ft, redis_module),
  {ok, RedisPool} = application:get_env(ft, redis_pool),
  {ok, RateLimit} = application:get_env(ft, rate_limit),
  {ok, N} = application:get_env(ft, n),
  {ok, QueueKey} = application:get_env(ft, queue_key),
  Interval = floor((1000 / RateLimit) * 1000), %microseconds
  case application:get_env(ft, run_producer) of
    {ok, false} -> pass;
    _ -> erlang:send(self(), gen)
  end,
  {ok, #{redis_module => RedisModule,
    redis_pool => RedisPool,
    rate_limit => RateLimit,
    interval => Interval,
    m => 2,
    n => N,
    queuekey => QueueKey}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), {Pid :: pid(), Tag :: term()}, State :: term()) ->
  {reply, Reply :: term(), State :: term()}
  | {reply, Reply :: term(), State :: term(), Timeout :: non_neg_integer()}
  | {noreply, State :: term()}
  | {noreply, State :: term(), Timeout :: non_neg_integer()}
  | {stop, Reason :: term(), Reply :: term(), State :: term()}
  | {stop, Reason :: term(), State :: term()}.
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
  {noreply, NewState :: term()}
  | {noreply, NewState :: term(), Timeout :: non_neg_integer()}
  | {noreply, NewState :: term(), hibernate}
  | {noreply, NewState :: term(), {continue, Continue :: term()}}
  | {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: term()) ->
  {noreply, State :: term()}
  | {noreply, State :: term(), Timeout :: non_neg_integer()}
  | {stop, Reason :: term(), State :: term()}.
handle_info(gen, State) ->
  gen(State),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term())
      -> term().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: term(), Extra :: term())
      -> {ok, NewState :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
gen(#{m := M, n := N,
  interval := Interval, redis_module := RedisModule,
  redis_pool := RedisPool, queuekey := QueueKey} = State) ->
  T1 = ts_micro(cur_timestamp()),
  RedisModule:q(RedisPool, ["RPUSH", QueueKey, urand(M, N)]),
  Diff = ts_micro(cur_timestamp()) - T1,
  microsleep(Interval - Diff),
  gen(State).

-ifdef('ERLANG_OTP_VERSION_17').
urand(From, To) -> mfloor(random:uniform() * To) + From.
-else.
urand(From, To) -> mfloor(rand:uniform() * To) + From.
-endif.

mfloor(X) when X < 0 ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T - 1
  end;
mfloor(X) ->
  trunc(X).


add_microsec(Micro, {Mega0, Sec0, Micro0}) ->
  Micro1 = Micro0 + Micro,
  Sec1 = Sec0 + (Micro1 div 1000000),
  Mega1 = Mega0 + (Sec1 div 1000000),
  {Mega1, (Sec1 rem 1000000), (Micro1 rem 1000000)}.

busywait_until(Target, Loops) ->
  case cur_timestamp() of
    Now when Now >= Target ->
      {Now, Loops};
    _ ->
      erlang:yield(),
      busywait_until(Target, 1 + Loops)
  end.

microsleep(MicroSec) ->
  Target = add_microsec(MicroSec, cur_timestamp()),
  AdjMsec = MicroSec - ?JITTER,
  case AdjMsec > 10000 of
    true -> timer:sleep(AdjMsec div 1000);
    false -> ok
  end,
  {Finish, Loops} = busywait_until(Target, 1),
  {timer:now_diff(Finish, Target), Loops}.

-ifdef('ERLANG_OTP_VERSION_17').
cur_timestamp() -> erlang:now().
-else.
cur_timestamp() -> erlang:timestamp().
-endif.

ts_micro({MegaSecs, Secs, Usecs}) ->
  (MegaSecs * 1000000 + Secs) * 1000000 + Usecs.