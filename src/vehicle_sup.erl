-module(vehicle_sup).
-behaviour(supervisor).
-export([start_in_shell_for_testing/0, start_link/0, init/1]).
-export([add_vehichle/2, stop/1, remove_vehichle/2]).

%% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) -> 
    {ok, {
       {one_for_one, 20, 100000},
       []
      }}.


%% @doc stop vehicle supervisor. terminate all the linked vehichles.
stop(SupPid) -> 
    exit(SupPid, stop),
    ok.

%% @doc add a new vehichle at Loc with capacity Kg.
add_vehichle(SupPid, {Loc, Kg}) ->
    % generate a unique id if id is not provided
    Id = list_to_atom(erlang:ref_to_list(make_ref())),
    add_vehichle_by_id(SupPid, {Id, Loc, Kg}).
add_vehichle_by_id(SupPid, {Id, Loc, Kg}) -> 
    Spec = {Id, {vehicle, start_link, [{Id, Loc, Kg}]},
            permanent, infinity, worker, [vehicle]},
    supervisor:start_child(SupPid, Spec),
    {ok, Id}.

%% @doc remove vehichle from tree
remove_vehichle(SupPid, Id) -> 
    supervisor:terminate_child(SupPid, Id),
    supervisor:delete_child(SupPid, Id).

%% Internals
start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, []),
    unlink(Pid),
    {ok, Pid}.

