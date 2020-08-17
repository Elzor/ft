-module(ft_consumer).

-behaviour(gen_server).

%% API
-export([start_link/1, is_prime/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%--------------------------------------------------------------------
%% @doc
%% Starts consumer
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
  {ok, QueueKey} = application:get_env(ft, queue_key),
  {ok, ResultsetKey} = application:get_env(ft, result_set_key),
  case application:get_env(ft, run_consumer) of
    {ok, false} -> pass;
    _Else -> erlang:send(self(), consume)
  end,
  {ok, #{redis_module => RedisModule,
    redis_pool => RedisPool,
    queuekey => QueueKey,
    resultsetkey => ResultsetKey}}.

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
handle_info(consume, State) ->
  check(State),
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
check(#{redis_module := RedisModule, redis_pool := RedisPool,
  queuekey := QueueKey, resultsetkey := ResultsetKey} = State) ->
  case RedisModule:q(RedisPool, ["LPOP", QueueKey]) of
    {ok, BinInt} when is_binary(BinInt) ->
      Number = binary_to_integer(BinInt),
      case is_prime(Number) of
        true -> RedisModule:q(RedisPool, ["HSET", ResultsetKey, Number, Number]);
        _ -> skip
      end;
    _Else ->
      timer:sleep(100)
  end,
  check(State).

-spec divisors(number())
      -> list().
divisors(N) -> [X || X <- lists:seq(1, N), (N rem X) == 0].

-spec is_prime(number()) ->
  boolean().
is_prime(N) when N =< 0; N == 1 -> false;
is_prime(2) -> true;
is_prime(N) -> divisors(N) == [1, N].