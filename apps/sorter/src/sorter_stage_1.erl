-module(sorter_stage_1).
-compile(export_all).
-behavior(gen_server).
-include("sorter.hrl").

start_link(Name)->
	gen_server:start_link({local,Name},?MODULE,[],[{timeout,30000}]).

init([])->
	{ok,[]}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({receive_ball, Id}, State) ->
	Res = update_stage(Id),
	lager:info("stage1 proc ~p ,ball received",[self()]),
	to_stage2(Res,Id),
    {noreply, State}.
    
handle_info(_Event, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_stage(Id)->
	try
		case ets:lookup(?BALLS,Id) of
			[Ball]->
				ets:insert(?BALLS,Ball#balls{stage1={s1,?NOW}}),
				ok;
			[]->
				lager:warning("stage1, Ball with unregisterd Id:~p received",[Id]),
				warning
		end
	catch
		_:_ ->
			lager:error("stage1, Ball with Id:~p cannot be sent to stage2",[Id]),
			error
	end.

to_stage2(error,_)->
	ok;
to_stage2(warning,Id)->
	?STAGE_2 ! {receive_ball, Id};
to_stage2(ok,Id)->
	?STAGE_2 ! {receive_ball, Id}.
