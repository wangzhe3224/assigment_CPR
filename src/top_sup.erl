-module(top_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-export([start_in_shell_for_testinglink/1]).

start_link(Args) -> 
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(FileName) -> 
    {ok, {
       {one_for_one, 5, 10000}, 
       [
        {
         planner,
         {planner, start_link, [FileName]},
         permanent, 10000, worker, [planner]
        },
        {
         manager,
         {manager, start_link, []},
         permanent, 10000, worker, [manager]
        },
        {
         vehicle_sup,
         {vehicle_sup, start_link, []},
         permanent, 10000, supervisor, [vehicle_sup]
        }
       ]
    }}.

start_in_shell_for_testinglink(Args) -> 
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, Args),
    unlink(Pid),
    {ok, Pid}.
