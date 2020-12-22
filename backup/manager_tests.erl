-module(manager_tests).
-include_lib("eunit/include/eunit.hrl").
-import(manager, [build_new_order/5, build_order/9, get_non_reserved_order_locations/1]).
% TODO: better to find a way to test internal state, add a function call would help
-record(state, 
        {
          order_id=0,  % next avaiable order id
          order_queue=queue:new()
        }).

gen_test_orders() -> 
    [
     build_new_order(1, f1, t1, 100, a_pid),
     build_new_order(2, f2, t2, 100, a_pid),
     build_order(3, f3, t3, f3, 100, a_pid, reserved, not_picked, not_delivered),
     build_new_order(4, f2, t2, 100, a_pid),
     build_new_order(5, f5, t5, 100, a_pid),
     build_new_order(6, f5, t6, 100, a_pid)
    ].

get_non_reserved_order_locations_test_() -> 
    OrderQueue = queue:from_list(gen_test_orders()),
    Locs = get_non_reserved_order_locations(OrderQueue),
    ?_assertEqual([f1, f2, f2, f5, f5], Locs).

get_reserve_order_refs_test_() -> 
    OrderQueue = queue:from_list(gen_test_orders()),
    ?_assertEqual([2], manager:get_reserve_order_refs(OrderQueue, f2, t2, 100)),
    ?_assertEqual([2, 4], manager:get_reserve_order_refs(OrderQueue, f2, t2, 400)).

handle_call_pickup_test_() -> 
    OrderQueue = queue:from_list(gen_test_orders()),
    Loc = f1, _From = a_pid,
    State = manager:build_init_state(),
    NewState = State#state{order_queue=OrderQueue},
    {reply, {ok, Locs}, S} = manager:handle_call({pickup, Loc}, _From, NewState),
    ?_assertEqual([f1, f2, f2, f5, f5], Locs).
