-module(planner).
-behaviour(gen_server).

%% API
% -export([start/1, stop/1, start_link/1]).
-export([stop/1, start_link/1, route/2, hub/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([build_init_state/1, start_in_shell_for_testing/0]).
-define(SERVER, ?MODULE).


% APIs
- spec route(From :: pid(), ToList :: list()) -> {ok, CityList :: list()} | {error, invalid}.
route(From, ToList) ->
    gen_server:call(?SERVER, {route, From, ToList}).

- spec hub(City :: string()) -> {ok, HubList :: list()} | {error, invalid}.
hub(City) -> 
    gen_server:call(?SERVER, {hub, City}).

%% @doc start planer server
- spec start_link(term()) -> {ok, pid()}.
start_link(FileName) ->
    State = build_init_state(FileName),
    % crash if match failed.
    {ok, _Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [State], []).

stop(Name) ->
    gen_server:call(Name, stop).

init(Args) ->
    {ok, Args}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

%% route: get route cities
handle_call({route, _FromC, ToList}, _From, State) ->
    % TODO: smart algo
    {reply, {ok, ToList}, State};
 
%% hub: get nearest hub(s)
handle_call({hub, City}, _From, State) -> 
    % TODO: smart algo
    {reply, {ok, [City]}, State};

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

% Helpers
% @doc consume locations.txt and build a graph in order to calculate route.
build_init_state(FileName) -> 
    {ok, Data} = file:consult(FileName),
    A = maps:from_list(Data),
    {ok, Paths} = maps:find(distances, A),
    {ok, Towns} = maps:find(towns, A),
    Vs = lists:map(fun(X)->{Town, _}=X,Town end, Towns),
    Es = lists:append(lists:map(fun(X)->{A1, A2, _}=X,[{A1,A2},{A2,A1}] end, Paths)),
    % build graph
    G = digraph:new(),
    lists:map(fun(V)->digraph:add_vertex(G, V)end, Vs),
    lists:map(fun(Edge)->{E1, E2}=Edge,digraph:add_edge(G,E1,E2)end, Es),
    maps:put(graph, G, A).

start_in_shell_for_testing() ->
    State = build_init_state("locations.txt"),
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [State], []),
    unlink(Pid).

