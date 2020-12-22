-module(setup_db).
-export([start/0, setup_db/0, reset_tables/0, clear_tables/0, do/1]).
-record(vehicle, {
          id, type, capacity, speed, % 100 unit/second
          location, loaded_weight, order_ids,
          from=none, to=none, distance_to_destination=none,
          process_pid
         }).
-record(order, {
          order_id, from, to, location, weight,
          owner_pid, reserve_status, deliver_status, vehicle_id, timestamp
         }).

setup_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(order,     [{attributes, record_info(fields, order)}, {disc_copies, [node()]}]),
    mnesia:create_table(vehicle,   [{attributes, record_info(fields, vehicle)}, {disc_copies, [node()]}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([order, vehicle], 20000).

reset_tables() ->
    mnesia:clear_table(order),
    mnesia:clear_table(vehicle),
    F = fun() ->
		lists:foreach(fun mnesia:write/1, gen_test_orders())
	end,
    mnesia:transaction(F).

clear_tables() -> 
    mnesia:clear_table(order),
    mnesia:clear_table(vehicle),
    cleared_tables.

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

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

gen_test_orders() -> 
    [
     build_new_order(1, f1, t1, 100, a_pid, {{2020, 1, 1}, {16, 0, 0}}),
     build_new_order(2, f2, t2, 100, a_pid, {{2020, 1, 1}, {16, 0, 0}}),
     build_order(3, f3, t3, f3, 100, a_pid, reserved, not_picked, not_delivered, {{2020, 1, 1}, {16, 0, 0}}),
     build_new_order(4, f2, t2, 100, a_pid, {{2020, 1, 1}, {16, 0, 0}}),
     build_new_order(5, f5, t5, 100, a_pid, {{2020, 1, 1}, {16, 0, 0}}),
     build_new_order(6, f5, t6, 100, a_pid, {{2020, 1, 1}, {16, 0, 0}})
    ].
