-module(vehicle).
-behaviour(gen_server).

%% API
-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([build_init_state/3, start_in_shell_for_testing/1]).
-export([move/4]).
-define(SERVER, ?MODULE).
-record(state, {
          % static info
          % type=truck, capacity=5000, 
          id, type=van, capacity=1000, 
          speed=100, % 100 unit/second
          % dynamic state
          location, loaded_weight=0, order_ids=[],
          from=none, to=none, distance_to_destination=none
         }).

 
start_link({ Id, Loc, Kg }) ->
    State = build_init_state(Id, Loc, Kg),
    {ok, _Pid} = gen_server:start_link({local, Id}, ?MODULE, [State], []).

%%
%% @doc for test server in the shell
start_in_shell_for_testing({ Id, Loc, Kg }) ->
    State = build_init_state(Id, Loc, Kg),
    {ok, Pid} = gen_server:start_link({local, Id}, ?MODULE, [State], []),
    unlink(Pid).

%% @doc Move from one location to another, this func is non-blocking
%% But handler of move will run for a while and update state
move(Id, From, To, Distance) ->
    gen_server:cast(Id, {move, From, To, Distance}).
    
init(Args) ->
    [State] = Args,
    {ok, State}.

stop(Name) ->
    gen_server:call(Name, stop).

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({move, From, To, Distance}, State) ->
    io:format("=> Vehicle start moving from ~p to ~p (~p)~n", [From, To, Distance]),
    Speed = State#state.speed,
    timer:sleep((Distance*1000) div Speed),
    io:format("=> Vehicle ~p moved to ~p~n", [self(), To]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, _State) -> 
    io:format("~p terminated normally.~n", [self()]),
    ok;
terminate(_Reason, _State) ->
    io:format("~p terminated abnormally.~n Save state.~n", [self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

build_init_state(Id, Loc, Kg) -> #state{id=Id, location=Loc, capacity=Kg}.
