-module(cases_test).
-include_lib("eunit/include/eunit.hrl").
-export([demo_run/0]).

clear_db_test() -> 
    setup_db:setup_db(),
    setup_db:clear_tables().

demo_run() -> 
    % setup clean database
    io:format("> Setup clean database to run demo. ~n"),
    clear_db_test(),
    % start full tree
    {ok, _Pid} = top_sup:start_link("locations.txt"),
    % add some vehicles
    {ok, V1} = vehicle_sup:add_vehichle(vehicle_sup, { "Oxford", 1000 }),
    {ok, V2} = vehicle_sup:add_vehichle(vehicle_sup, { "Oxford", 5000 }),
    % submit some orders
    manager:send("Oxford", "Leeds", 1000),
    manager:send("Oxford", "Leeds", 2000),
    manager:send("Oxford", "Brimingham", 950),
    manager:send("Oxford", "Liverpool", 950),

    % 1. call pickup 
    {ok, Route} = manager:pickup("Oxford"),
    io:format("~n> Call manager:pickup/1 get a route assigned: ~p~n", [Route]),

    % 2. vehicles reserve orders 
    io:format("~n> Vehicles reserved orders.~n"),
    {ok, OrderIds} = vehicle:reserve(V1, "Oxford", "Leeds", 1000),
    {ok, OrderIds2} = vehicle:reserve(V2, "Oxford", "Leeds", 5000),
    io:format("~n> Vehicles load orders.~n"),
    lists:map(fun(OrderId) -> vehicle:load(V1, OrderId) end, OrderIds),
    lists:map(fun(OrderId) -> vehicle:load(V2, OrderId) end, OrderIds2),
    {ok, Routes} = planner:route("Oxford", ["Leeds"]),
    io:format("~n> Getting route ~p~n", [Routes]),
    [From, To] = Routes,
    % vehicle need to calcualte the distance?
    io:format("~n> Vehicle moving ....~n"),
    vehicle:move(V1, From, To, 103),
    vehicle:move(V2, From, To, 103),
    lists:map(fun(OrderId) -> vehicle:dropoff(V1, OrderId) end, OrderIds),
    lists:map(fun(OrderId) -> vehicle:dropoff(V2, OrderId) end, OrderIds2),
    io:format("~n> Vehicles drop off orders~n"),
    {ok, [Hub | _]} = planner:hub("Leeds"),
    io:format("~n> End round, vehicles go back to hub ~p~n", [Hub]),
    vehicle:move(V1, To, Hub, 103),
    vehicle:move(V2, To, Hub, 103),

    demo_finished.


%% APIs demos 
%% Note: below functions should be run in a fresh Erlang shell.
%%
top_sup_test() -> 
    {ok, Pid} = top_sup:start_link("locations.txt"),
    io:format("> Top sup Pid    : ~p~n", [Pid]),
    io:format("> Vehicle Sup Pid: ~p~n", [whereis(vehicle_sup)]),
    io:format("> Planner Pid    : ~p~n", [whereis(planner)]),
    io:format("> Manager Pid    : ~p~n", [whereis(manager)]),
    io:format("! Kill Manager process ... ~n"),
    exit(whereis(manager), kill),
    timer:sleep(1000),
    io:format("> Manager Pid    : ~p~n", [whereis(manager)]),
    io:format("! Kill Planner process ... ~n"),
    exit(whereis(planner), kill),
    timer:sleep(1000),
    io:format("> Planner Pid    : ~p~n", [whereis(planner)]),
    io:format("! Kill Vehicle Sup process ... ~n"),
    exit(whereis(vehicle_sup), kill),
    timer:sleep(1000),
    io:format("> Vehicle Sup Pid    : ~p~n", [whereis(vehicle_sup)]),
    top_sup_test_done.

planner_api_test() -> 
    {ok, _Pid1}      = planner:start_link("locations.txt"),
    {ok, CityList}   = planner:route("Oxford", ["Liverpool"]),
    {error, invalid} = planner:route("Oxford", ["Liver"]),
    {ok, HubList}    = planner:hub("Sheffield"),
    ?assertEqual(["Oxford", "Birmingham"], HubList),
    ?assertEqual(["Oxford","Leeds","Manchester","Liverpool"], CityList),
    planner_test_done.

manager_api_test() -> 
    % good cases
    % {ok, _Pid2} = manager:start_link(),
    {ok, _Pid2}     = manager:start_in_shell_for_testing(),
    {ok, OrderId}   = manager:send("Oxford", "Leeds", 100),
    {ok, Locations} = manager:pickup("Oxford"),
    {ok, Reserves}  = manager:reserve("Oxford", "Leeds", 200),
    ok              = manager:load(OrderId),
    ok              = manager:dropoff(OrderId),
    {ok, OrderInfo} = manager:lookup(OrderId),

    io:format("> Locations: ~p~n", [Locations]),
    io:format("> Reserves : ~p~n", [Reserves]),
    io:format("> Orderinfo: ~p~n", [OrderInfo]),

    manager_test_done.

vehicle_api_test() -> 

    {ok, _Pid1} = vehicle:start_in_shell_for_testing({ v1, location1, 1000 }),
    % move from location 1 to location 2
    vehicle:move(v1, location1, location2, 100),
    {ok, _RefList} = vehicle:reserve(v1, "Oxford", "Leeds", 200),

    vehicle_api_test_done.

vehicle_sup_test() -> 

    {ok, SupPid} = vehicle_sup:start_in_shell_for_testing(),
    {ok, Id1}    = vehicle_sup:add_vehichle(SupPid, {location2, 1000}),
    {ok, Id2}    = vehicle_sup:add_vehichle(SupPid, {location3, 1000}),
    % ok           = vehicle_sup:stop(SupPid),
    io:format("> Testing Vehicle process carshes and recovery~n"),
    io:format("> Current Vehicles: ~p~n~n~n", [supervisor:which_children(vehicle_sup)]),
    exit(whereis(Id1), kill),
    timer:sleep(2000),
    io:format("~n> Current Vehicles after ~p crashed:~n ~p~n",
              [Id2, supervisor:which_children(vehicle_sup)]),
    io:format("~n> Note that Pid of ~p has changed. But the state is mantained. ~n", [Id2]),
    io:format("> Remove ~p~n", [Id1]),
    ok           = vehicle_sup:remove_vehichle(SupPid, Id1),

    vehicle_sup_test_done.

