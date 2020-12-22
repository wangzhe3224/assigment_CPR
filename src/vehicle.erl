-module(vehicle).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([build_init_state/3, start_in_shell_for_testing/1, query_vehicle/1]).
-export([move/4, load/2, dropoff/2, transit/3, reserve/4]).
-define(SERVER, ?MODULE).
-record(vehicle, {
          % static info
          % type=truck, capacity=5000, 
          id, type=van, capacity=1000, 
          speed=100, % 100 unit/second
          % dynamic state
          location, loaded_weight=0, order_ids=[],
          from=none, to=none, distance_to_destination=none,
          process_pid
         }).

 
start_link({ Id, Loc, Kg }) ->
    % query database to restore state
    mnesia:start(),
    mnesia:wait_for_tables([vehicle], 20000),
    State = build_init_state(Id, Loc, Kg),
    persist_vehicle(State),
    case gen_server:start_link({local, Id}, ?MODULE, [State], []) of 
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

%%
%% @doc for test server in the shell
start_in_shell_for_testing({ Id, Loc, Kg }) ->
    mnesia:start(),
    mnesia:wait_for_tables([vehicle], 20000),
    State = build_init_state(Id, Loc, Kg),
    persist_vehicle(State),
    case gen_server:start_link({local, Id}, ?MODULE, [State], []) of 
        {ok, Pid} -> unlink(Pid), {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

%% @doc Move from one location to another, this func is non-blocking
%% But handler of move will run for a while and update state
move(Id, From, To, Distance) ->
    gen_server:cast(Id, {move, From, To, Distance}).

%% @doc reserved orders 
reserve(Id, From, To, Kg) -> 
    gen_server:call(Id, {reserve, From, To, Kg}).

%% @doc load order, update order list of the vehicle and persist it.
load(Id, OrderId) -> 
    gen_server:call(Id, {load, OrderId}).

%% @doc dropoff order, remove order id from order list
dropoff(Id, OrderId) -> 
    gen_server:call(Id, {dropoff, OrderId}).

%% @doc transit order, remove order if from order list
transit(Id, OrderId, _Loc) -> 
    gen_server:call(Id, {transit, OrderId}).
    
init(Args) ->
    [State] = Args,
    {ok, State}.

stop(Name) ->
    gen_server:call(Name, stop).

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({load, OrderId}, _From, State) ->
    Reply = manager:load(OrderId),
    OrderIds = State#vehicle.order_ids,
    NewState = State#vehicle{order_ids=[OrderId | OrderIds]},
    persist_vehicle(NewState),
    {reply, Reply, NewState};
handle_call({dropoff, OrderId}, _From, State) -> 
    Reply = manager:dropoff(OrderId),
    OrderIds = State#vehicle.order_ids,
    NewState = State#vehicle{order_ids=lists:delete(OrderId, OrderIds)},
    persist_vehicle(NewState),
    {reply, Reply, NewState};
handle_call({transit, OrderId}, _From, State) -> 
    Reply = manager:transit(OrderId),
    OrderIds = State#vehicle.order_ids,
    NewState = State#vehicle{order_ids=lists:delete(OrderId, OrderIds)},
    persist_vehicle(NewState),
    {reply, Reply, NewState};
handle_call({reserve, From, To, Kg}, _From, State) -> 
    Reply = manager:reserve(From, To, Kg),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({move, From, To, Distance}, State) ->
    io:format("=> Vehicle start moving from ~p to ~p (~p)~n", [From, To, Distance]),
    Speed = State#vehicle.speed,
    timer:sleep((Distance*1000) div Speed),
    io:format("=> Vehicle ~p moved to ~p~n", [self(), To]),
    NewState = State#vehicle{location=To},
    persist_vehicle(NewState),
    {noreply, NewState};
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

% @doc Persist Id if it is a new one, otherwise fetch from the database
build_init_state(Id, Loc, Kg) -> 
    case query_vehicle(Id) of 
        []    -> 
            State = #vehicle{id=Id, location=Loc, capacity=Kg},
            State;
        State -> 
            io:format("Vehicle ID: ~p already existed, pickup state.~n", [Id]),
            State
    end.

%% @doc do 1 query
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

% @doc either empty or a vehicle status
query_vehicle(Id) -> 
    do(qlc:q([ X || X <- mnesia:table(vehicle), X#vehicle.id =:= Id])).

% @doc write vehicle states to database
persist_vehicle(State) -> 
    % Updated = State#vehicle{process_pid=self()},
    % io:format("[DEBUG INFO] Persist vehicle state: ~p~n", [State]),
    mnesia:transaction(fun() -> 
                               mnesia:write(State)
                       end).

