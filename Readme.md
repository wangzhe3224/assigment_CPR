# Assignment

## Components

- Simulation config file
- locations.txt
- planner.erl
    - start_link(FileName) -> {ok, Pid}.
    - route(From, ToList) -> {ok, CityList} | {error, invalid}.
    - hub(City) -> {ok, HubList | {error, invalid}}.
- manager.erl
    * start_link() -> {ok, Pid}.
    * send(From, To, Kg) -> {ok, OrderId}. Once delivered, send response.
    * pickup(Loc)
    * reserve(From, To, Kg) -> {ok, RefList}.
    * load(Ref)
    * dropoff(Ref)
    * transit(Ref, Loc)
    * lookup(Ref)
- vehicle.erl
    * maintian some states.
    * this is a client of manager and planner?
- vehichle_sup.erl
    * add/remove vehichle.
    * if a vehicle termineted abnormally, restart it at the closet hub.
- top_sup.erl
    * manage vehicle_sup, manager, and planner. 
    * Choose a supervision strategy that ensures the system is not left in an
      inconsistent state and allows the other components to recover.

## `planner.erl`

- Build state from file, using map state to store it. `maps` module is useful to
  retrive states.

Need a weighted graph to plan the route. 

```erlang
% preprocess staes
{ok, A} = States.
A = planner:build_init_state("locations.txt").
{ok, Paths} = maps:find(distances, A).
{ok, Towns} = maps:find(towns, A).
Vs = lists:map(fun(X)->{Town, _}=X,Town end, Towns).
Es = lists:append(lists:map(fun(X)->{A1, A2, _}=X,[{A1,A2},{A2,A1}] end, Paths)).
% build graph
G = digraph:new().
lists:map(fun(V)->digraph:add_vertex(G, V)end, Vs).
lists:map(fun(Edge)->{E1, E2}=Edge,digraph:add_edge(G,E1,E2)end, Es).
```

## `manager.erl`

- Can get table from planner. 

States:
- [ { OrderId, Kg, From, To, 
      Loc, VehiclePid, OwnerPid,
      ReserveStatus, deliverStatus, QueueTime, 
      } ]
- [ { Location, [OrderId] } ]

Orders:
[ order_id, from, to, location, owner_pid, reserve_status, deliver_status,
  vehicle_pid ]

## `vehicle.erl`

Need time to move from 1 city to another city. 

States:
- { Type, Capacity, LoadedWeight, 
    Destination, OrderIds,
    CurrentLoc
   }
