-module(manager).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, ?MODULE).
-record(state, { order_queue=queue:new() }).
-record(order, {
          order_id, from, to, location, weight,
          owner_pid, reserve_status, deliver_status, vehicle_id, timestamp
         }).
% Internal
-export([
         build_init_state/0, build_order/10, build_new_order/6,
         init/1, handle_call/3, do/1, all_orders/0, put_order/4,
         unreserved_orders/1, sort_orders_by_time/1, query_orders/2,
         reserve_action_by/3, is_hub/2, transit_order/1, start_in_shell_for_testing/0,
         handle_cast/2, handle_info/2,  terminate/2, code_change/3, stop/1
        ]).
% Export APIs
-export([start_link/0, send/3, pickup/1, reserve/3, load/1, dropoff/1, 
         transit/2, lookup/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%       Public APIs        %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc start manager server, connect to database
- spec start_link() -> {ok, pid()}.
start_link() ->
    mnesia:start(),
    mnesia:wait_for_tables([order, vehicle], 20000),
    State = build_init_state(),
    {ok, _Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [State], []).

%% @doc submit order to deliver From to To with weights Kg.
- spec send(From :: string(), To :: string(), Kg :: integer()) -> {ok, OrderId :: integer()}.
send(From, To, Kg) -> 
    gen_server:call(?SERVER, {send, From, To, Kg}).

%% @doc given a location, manager will give back a list of locations that have 
%% parcels to pick up (Note results contain duplicate locations) 
- spec pickup(Loc :: string()) -> {ok, DestinationList :: list()} | {error, Reason :: term()}.
pickup(Loc) -> 
    gen_server:call(?SERVER, {pickup, Loc}).

%% @doc Reserves a list of parcels weighing a maximum of Kg kilos in total that
%% have to be picked up in a location From for delivery to destination To. 
- spec reserve(From :: string(), To :: string(), Kg :: integer()) -> {ok, list()}.
reserve(From, To, Kg) ->
    gen_server:call(?SERVER, {reserve, From, To, Kg}).

%% @doc Given order id, load an order (if the vehicle is at right location)
- spec load(Ref :: integer()) -> ok | {error, not_reserved|instance}.
load(Ref) -> 
    gen_server:call(?SERVER, {load, Ref}).

%% @doc drop off parcel when vehicle reached destination, will trigger 
%% {delivered, OrderId} message sent to order OwnerPid.
- spec dropoff(Ref :: integer()) -> ok | {error, not_picked | instance}.
dropoff(Ref) -> 
    gen_server:call(?SERVER, {dropoff, Ref}).

%% @doc put an order to a hub.
- spec transit(Ref :: integer(), Loc :: string()) -> ok | {error, not_picked | instance}.
transit(Ref, Loc) -> 
    gen_server:call(?SERVER, {transit, Ref, Loc}).

%% @doc given an order id, check order info {Ref,From,To,Kg,Loc|VehiclePid,OwnerPid}
- spec lookup(Ref :: integer()) -> {ok, term() | {error, instace}}.
lookup(Ref) -> 
    gen_server:call(?SERVER, {lookup, Ref}).

         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         %%%%%%%%%%%%%%        Callbacks         %%%%%%%%%%%%%
         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @doc inert 1 order, return order id
handle_call({send, From, To, Kg}, FromPid, State) ->
    Reply = put_order(From, To, Kg, FromPid),
    { reply, Reply, State };

%% pickup: Destinations are provided based on non-reserved parcels that 
%% have been waiting the longest to be picked up. Update order state
%% If order queue is empty, { error, no_unreserved_orders }
handle_call({pickup, Loc}, _From, State) -> 
    case Orders = unreserved_orders(Loc) of 
        [] -> { reply, {error, no_unreserved_orders}, State };
        _  ->  
            Des = lists:map(fun(Item) -> Item#order.to end, sort_orders_by_time(Orders)),
            { reply, {ok, Des}, State }
    end;

%% reserve: dequeue order queue until weights sums to Kg.
%% Mark orders as reserved by this Pid/Id
%% vehicle process pid changed won't affect Id
handle_call({reserve, From, To, Kg}, FromPid, State) -> 
    {Pid, _Ref} = FromPid,
    Id = get_pid_name(Pid),
    Orders = sort_orders_by_time(query_orders(From, To)),
    { SelecteIds, _ } = lists:foldl(fun(Elem, {Select, Sum}) -> 
                                            W = Elem#order.weight,
                                            ReserveStatus = Elem#order.reserve_status,
                                            case ReserveStatus of 
                                                reserved -> {Select, Sum};
                                                _ -> if
                                                         (W + Sum) > Kg -> {Select, Sum};
                                                         true -> {[Elem#order.order_id | Select], Sum+W}
                                                     end
                                            end
                                    end, {[], 0}, Orders),
    % mark ids with reserved
    lists:map(fun(OrderId) -> reserve_action_by(OrderId, reserve, Id) end,
              SelecteIds),
    io:format("> ~p reserved order ~p~n", [Id, SelecteIds]),
    { reply, {ok, SelecteIds}, State };

%% load: if the vehicle reserve the order, mark this order's vehicle_id
%%  conver pid to Id, otherwise the order is unlinked from vehichle
%% if the vehichle process crashed.
handle_call({load, OrderId}, FromPid, State) -> 
    {Pid, _Ref} = FromPid,
    Id = get_pid_name(Pid),
    case query_orders(OrderId) of 
        [ ] -> Reply = {error, order_not_exist};
        [O] -> if 
                   (O#order.reserve_status == reserved) -> 
                   % and (O#order.vehicle_id==FromPid) -> 
                       % mark order picked
                       pick_action_by(OrderId, pick, Id),
                       Reply = ok;
                   true -> Reply = {error, not_reserved}
               end;
        _   -> Reply = {error, unknown}
    end,
    io:format("> Load order id ~p to ~p~n", [OrderId, Id]),
    { reply, Reply, State };

%% dropoff: mark order delivered, send {delivered, OrderId} or order owner_pid.
handle_call({dropoff, OrderId}, FromPid, State) ->
    {Pid, _Ref} = FromPid,
    Id = get_pid_name(Pid),
    io:format("> ~p dropoff order ~p. Send message to owner_pid ~n", [Id, OrderId]),
    case query_orders(OrderId) of 
        [ ]   -> Reply = {error, order_not_exist};
        [O]   -> if 
                   (O#order.deliver_status == picked) -> 
                   % and (O#order.vehicle_id==FromPid) -> 
                       % mark order deliverd  
                       pick_action_by(OrderId, delivered, Id),
                       % notify order owner
                       {OwnerPid, _} = O#order.owner_pid,
                       OwnerPid ! {delivered, OrderId},
                       Reply = ok;
                   true -> Reply = {error, not_picked}
                 end;
        _    -> Reply = {error, unknown}
    end,
    { reply, Reply, State };

%% transit: mark unpicked, unreserved. Check if {Loc} is a hub from planner service
handle_call({transit, OrderId, Loc}, FromPid, State) -> 
    io:format("> ~p transit order ~p at ~p~n", [FromPid, OrderId, Loc]),
    case is_hub(Loc, planner) of 
        false -> { reply, {error, not_a_hub}, State };
        true  -> 
            case is_order_exist(OrderId) of 
                false -> { reply, {error, order_not_exist}, State };
                true  -> 
                    transit_order(OrderId),
                    { reply, ok, State }
            end
    end;

%% lookup: get general order info
handle_call({lookup, OrderId}, _From, State) ->
    io:format("> ~p check order ~p info.~n", [_From, OrderId]),
    case query_orders(OrderId) of 
        [ ]   -> Reply = {error, order_not_exist};
        [O]   -> 
            if 
                O#order.vehicle_id /= none -> 
                    Reply = {ok, {OrderId, O#order.from, O#order.to, O#order.weight, 
                                  O#order.vehicle_id, O#order.owner_pid}};
                true -> 
                    Reply = {ok, {OrderId, O#order.from, O#order.to, O#order.weight, 
                                  O#order.location, O#order.owner_pid}}
            end;
        _     -> Reply = {error, unknown}
    end,
    { reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop(_Name) ->
    gen_server:call(?SERVER, stop).

init(Args) ->
    [State] = Args,
    {ok, State}.

         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         %%%%%%%%%%%%%%     Helpers Funtions     %%%%%%%%%%%%%
         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
% @doc check if {Loc} is a hub by calling planner service
is_hub(Loc, PlannerService) -> 
    {ok, Hubs} = PlannerService:hub_list(),
    lists:member(Loc, Hubs).

% @doc build order record
build_order(OrderId, From, To, Location, Kg, OwnerPid, 
            ReserveStatus, DeliverStatus, VehicleId, Timestamp) -> 
    _Order = #order{
        order_id       = OrderId,
        from           = From,
        to             = To,
        location       = Location,
        weight         = Kg,
        owner_pid      = OwnerPid,
        reserve_status = ReserveStatus,
        deliver_status = DeliverStatus,
        vehicle_id     = VehicleId,
        timestamp      = Timestamp
    }.

%% @doc create a new order with default states
build_new_order(OrderId, From, To, Kg, OwnerPid, Datetime) ->
    build_order(OrderId, From, To, From, Kg, OwnerPid, not_reserved, not_delivered, no_vehicle_pid, Datetime).

% @doc build initial state of manager from database
build_init_state() -> 
    {ok, #state{}}.

% @doc sort order from oldest to newest
sort_orders_by_time(Orders) -> 
    lists:sort(fun(A, B) -> A#order.timestamp > B#order.timestamp end, Orders).

%% @doc do 1 query
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

% @doc get all ordes in the database
all_orders() -> 
    do(qlc:q([X || X <- mnesia:table(order)])).

is_order_exist(OrderId) -> 
    case do(qlc:q([ X || X <- mnesia:table(order), X#order.order_id =:= OrderId ])) of 
        [] -> false;
        _  -> true
    end.

% @doc create and inert 1 order, return the order id
put_order(From, To, Kg, FromPid) -> 
    Time = erlang:system_time(),
    OrderId = erlang:phash2({node(), Time}),
    Order = build_new_order(OrderId, From, To, Kg, FromPid, Time),
    mnesia:transaction(fun() -> mnesia:write(Order) end),
    {ok, OrderId}.

% @doc get unreserved orders
unreserved_orders(_Loc) -> 
    do(qlc:q([ X || X <- mnesia:table(order), X#order.reserve_status /= reserved ])).

% @doc query order {From} to {To}
query_orders(From, To) -> 
    do(qlc:q([ X || X <- mnesia:table(order), X#order.from =:= From, X#order.to =:= To ])).
query_orders(OrderId) -> 
    do(qlc:q([ X || X <- mnesia:table(order), X#order.order_id =:= OrderId ])).
    
% @doc reserved order ids, assume order id exists
reserve_action_by(OrderId, Action, By) -> 
    F = fun() -> 
                [ O ] = mnesia:read({order, OrderId}),
                case Action of 
                    reserve -> 
                        N = O#order{reserve_status=reserved, vehicle_id=By},
                        mnesia:write(N);
                    unreserve -> 
                        N = O#order{reserve_status=not_reserved, vehicle_id=none},
                        mnesia:write(N)
                end
        end,
    mnesia:transaction(F).

% @doc pick/deliver order ids, assume order id exists
pick_action_by(OrderId, Action, By) -> 
    F = fun() -> 
                [ O ] = mnesia:read({order, OrderId}),
                case Action of 
                    pick -> 
                        N = O#order{deliver_status=picked, vehicle_id=By},
                        mnesia:write(N);
                    unpick -> 
                        N = O#order{deliver_status=no_picked, vehicle_id=none},
                        mnesia:write(N);
                    delivered -> 
                        N = O#order{deliver_status=delivered, vehicle_id=By},
                        mnesia:write(N)
                end
        end,
    mnesia:transaction(F).

% @doc transit order 
transit_order(OrderId) -> 
    F = fun() -> 
                [ O ] = mnesia:read({order, OrderId}),
                N = O#order{deliver_status=undelived, 
                            reserve_status=unreserved, 
                            vehicle_id=none},
                mnesia:write(N)
        end,
    mnesia:transaction(F).

start_in_shell_for_testing() ->
    mnesia:start(),
    mnesia:wait_for_tables([order, vehicle], 20000),
    State = build_init_state(),
    {ok, _Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [State], []),
    unlink(_Pid),
    {ok, _Pid}.

% @doc given a pid, get registered_name if exist, undefined otherwise.
get_pid_name(Pid) -> 
    case process_info(Pid) of 
        [{registered_name, Id}|_] -> Id;
        _ -> undefined
    end.
