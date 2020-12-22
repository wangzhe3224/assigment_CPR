-module(manager).
-behaviour(gen_server).

-export([stop/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([build_init_state/0, start_in_shell_for_testing/0, 
         get_non_reserved_order_locations/1,
         build_new_order/5, build_order/9,
         get_reserve_order_refs/4]).
-define(SERVER, ?MODULE).
% Export APIs
-export([send/3, pickup/1, reserve/3, load/1, dropoff/1, transit/2, lookup/1]).

%% Internal state 
-record(order, {
    order_id, from, to, location, weight,
    owner_pid, reserve_status, deliver_status, vehicle_id
}).
-record(state, 
        {
          order_id=0,  % next avaiable order id
          order_queue=queue:new()
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%       Public APIs        %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc start manager server
- spec start_link() -> {ok, pid()}.
start_link() ->
    State = build_init_state(),
    % crash if match failed.
    {ok, _Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [State], []).

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

%% 
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
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

%%
%% send: increase order_id by one, push new order into order queue
handle_call({send, From, To, Kg}, FromPid, State) ->
    io:format("> Initial state ~p~n", [State]),
    #state{order_id=OrderId, order_queue=OrderQueue} = State,
    NewOrder = build_new_order(OrderId, From, To, Kg, FromPid),
    NewState = State#state{order_id=OrderId+1, order_queue=queue:in(NewOrder, OrderQueue)},
    io:format("> Received order ~p ~p ~p, assign order id ~p~n", [ From, To, Kg, OrderId ]),
    io:format("> Update states to ~p~n", [NewState]),
    io:format("> Enqueue new order ~p~n", [NewOrder]),
    { reply, {ok, OrderId}, NewState };

%%
%% pickup: Destinations are provided based on non-reserved parcels that 
%% have been waiting the longest to be picked up. Update order state
%% If order queue is empty, { error, no_orders_in_queue }
handle_call({pickup, Loc}, _From, State) -> 
    #state{order_id=_OrderId, order_queue=OrderQueue} = State,
    io:format("> Responsd pickup requirements at ~p~n", [Loc]),
    case queue:is_empty(OrderQueue) of 
        true -> { reply, {error, no_orders_in_queue}, State };
        _    -> 
            Locs = get_non_reserved_order_locations(OrderQueue),
            { reply, {ok   , Locs}, State }
    end;

%% 
%% reserve: dequeue order queue until weights sums to Kg.
%% Mark orders as reserved by this Pid/Id
handle_call({reserve, From, To, Kg}, FromPid, State) -> 
    #state{order_id=_OrderId, order_queue=OrderQueue} = State,
    ReserveIds = get_reserve_order_refs(OrderQueue, From, To, Kg),
    io:format("> Reserve orders for ~p: ~p~n", [FromPid, ReserveIds]),
    {reply, {ok, ReserveIds}, State};

%%
%% load: if the vehicle reserve the order, mark this order's vehicle_id
%% TODO: conver pid to Id, otherwise the order is unlinked from vehichle
%% if the vehichle process crashed.
handle_call({load, OrderId}, FromPid, State) -> 
    #state{order_id=_OrderId, order_queue=OrderQueue} = State,
    % find order
    io:format("> Load order id ~p to ~p~n", [OrderId, FromPid]),
    { reply, ok, State };

%%
%% dropoff: mark order delivered, send {delivered, OrderId} or order owner_pid.
handle_call({dropoff, OrderId}, _From, State) ->
    #state{order_id=_OrderId, order_queue=OrderQueue} = State,
    io:format("> [Not implement yet] ~p dropoff order ~p. Send message to owner_pid ~n", [_From, OrderId]),
    { reply, ok, State };

%%
%% transit: TODO: not sure yet.
handle_call({transit, OrderId, Loc}, _From, State) -> 
    #state{order_id=_OrderId, order_queue=OrderQueue} = State,
    io:format("> [Not implement yet] ~p transit order ~p at ~p~n", [_From, OrderId, Loc]),
    { reply, ok, State };

%% 
%% lookup: get general order info
handle_call({lookup, OrderId}, _From, State) ->
    #state{order_id=_OrderId, order_queue=OrderQueue} = State,
    io:format("> [Not implement yet] ~p check order ~p info.~n", [_From, OrderId]),
    { reply, {ok, {OrderId, some, info}}, State};

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
build_init_state() -> #state{}.

% @doc build order record
build_order(OrderId, From, To, Location, Kg, OwnerPid, 
            ReserveStatus, DeliverStatus, VehicleId) -> 
    _Order = #order{
        order_id       = OrderId,
        from           = From,
        to             = To,
        location       = Location,
        weight         = Kg,
        owner_pid      = OwnerPid,
        reserve_status = ReserveStatus,
        deliver_status = DeliverStatus,
        vehicle_id     = VehicleId
    }.

%% @doc create a new order with default states
build_new_order(OrderId, From, To, Kg, OwnerPid) ->
    build_order(OrderId, From, To, From, Kg, OwnerPid, not_reserved, not_delivered, no_vehicle_pid).

%% @doc for test server in the shell
start_in_shell_for_testing() ->
    State = build_init_state(),
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [State], []),
    unlink(Pid).

%% @doc given order queue, return locations of all non-reserved orders
get_non_reserved_order_locations(OrderQueue) -> 
    lists:map(fun(Order) -> Order#order.location end, 
              queue:to_list(queue:filter(fun(Order) -> Order#order.reserve_status/=reserved end, OrderQueue))).

%% @doc given order queue, select non-reserved orders that {From} to {To},
%% and sum of weights less than {Kg}. 
get_reserve_order_refs(OrderQueue, From, To, Kg) -> 
    Orders = lists:filter(fun(Item) -> 
                                  case Item of 
                                      {order,_,From,To,_,_,_,_,_,_} -> true;
                                      _ -> false
                                  end
                          end, queue:to_list(OrderQueue)),
    {Refs, _Total} = lists:foldr(fun(Elem, {Select, Sum})->
                                      W = Elem#order.weight,
                                      if (W + Sum) > Kg -> {Select, Sum};
                                         true -> {[Elem#order.order_id | Select], Sum+W}
                                      end
                              end,
                              {[], 0}, Orders),
    Refs.

%% @doc given order queue, mark {Id} as reserved by {Pid}
