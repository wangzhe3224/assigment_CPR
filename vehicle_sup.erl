-module(vehicle_sup).
-behaviour(supervisor).
-export([start_in_shell_for_testing/0, start_link/0, init/1]).
-export([add_vehichle/2, stop/1, remove_vehichle/2]).

%% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc stop vehicle supervisor. terminate all the linked vehichles.
stop(SupPid) -> 
    exit(SupPid, stop),
    ok.

%% @doc add a new vehichle at Loc with capacity Kg?
add_vehichle(SupPid, {Id, Loc, Kg}) -> 
    supervisor:start_child(SupPid, [ {Id, Loc, Kg} ]).

%% @doc remove vehichle from tree
remove_vehichle(SupPid, Id) -> 
    supervisor:terminate_child(SupPid, whereis(Id)).

%% Internals
start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, []),
    unlink(Pid),
    {ok, Pid}.

init(_Arg) -> 
    {ok, {
       {simple_one_for_one, 20, 100000},
       [
        {vehicletype, {vehicle, start_link, []},
         permanent, infinity, worker, [vehicle]}
       ]
      }}.

