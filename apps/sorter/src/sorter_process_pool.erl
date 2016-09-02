%%--------------------------------------------------------------------
%% @author Anmol Kumar Singh <anmolsingh008@gmail.com>
%%%-----------------------------------------------------------------------------
-module(sorter_process_pool).
-export([take_member/1]).
-define(WORKERS, 10).

take_member(stage_1_worker)->
	return_process(stage_1_worker,0);
take_member(Else)->
	lager:error("process Pool:~p undefined",[Else]).
	
return_process(stage_1_worker,N) when N < ?WORKERS->
    Random = erlang:phash(os:timestamp(),?WORKERS),
    try
        case erlang:whereis(erlang:list_to_atom("sorter_stage_1_worker_" ++ integer_to_list(Random))) of
            undefined ->
                return_process(stage_1_worker,N+1);
            Server->
                Server
        end
    catch
        _:_ ->
            return_process(stage_1_worker,N+1)
    end;
return_process(_,_N)->
    lager:error("sorter_process_pool, Failed to fetch process from pool, givinig up").
