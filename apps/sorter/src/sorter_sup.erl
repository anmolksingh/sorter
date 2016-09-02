-module(sorter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	LoadGenerator = ?CHILD(sorter_load_generator,worker),
	%Stage1 = ?CHILD(sorter_stage_1,worker),
	Stage1WorkerSup = ?CHILD(sorter_stage_1_worker_sup,supervisor),
	Stage2 = ?CHILD(sorter_stage_2,worker),
	Stage3 = ?CHILD(sorter_stage_3, worker),
	Destination = ?CHILD(sorter_destination, worker),
	Stats = ?CHILD(sorter_stats, worker),
	Childs = [LoadGenerator, Stage1WorkerSup, Stage2, Stage3, Destination,Stats],	
    {ok, { {one_for_one, 5, 10}, Childs} }.
