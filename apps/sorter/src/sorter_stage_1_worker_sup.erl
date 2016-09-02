%%
%% @author Anmol Kumar Singh <anmol@dasudian.com>
%%
-module(sorter_stage_1_worker_sup).
-behaviour(supervisor).
%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).
-define(CHILD(I, Type,Name), {Name, {I, start_link, [Name]}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
     Workers = stats_workers(10,[]),
     {ok, { {one_for_one, 5, 10}, Workers}}.

stats_workers(0,Childs)->
    lager:info("All the workers are started"),
    Childs;
stats_workers(N,Childs)->
    Name = list_to_atom("sorter_stage_1_worker_" ++ integer_to_list(N)),
    Worker = ?CHILD(sorter_stage_1, worker,Name),
    Children = [Worker] ++ Childs,
    stats_workers(N-1,Children).
