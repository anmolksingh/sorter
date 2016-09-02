-module(sorter_stage_2).
-compile(export_all).
-behavior(gen_server).
-include("sorter.hrl").
-define(SERVER,?MODULE).

start_link()->
	gen_server:start_link({local,?SERVER},?MODULE,[],[{timeout,30000}]).

init([])->
	{ok,[]}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Event, State) ->
    {noreply, State}.
    
handle_info({receive_ball, Id}, State) ->
	T1 = ?NOW,
	detect_color(Id),
	TimeDiff = timer:now_diff(?NOW,T1),
	if
		TimeDiff < 100 ->
			lager:info("stage2, Redetect the color of the ball"),
			detect_color(Id);
	 	true  ->
			ok
	end, 
	timer:sleep(1500),
	to_stage3(Id),
    {noreply, State}.

terminate(_Reason, State) ->
    State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

detect_color(Id)->
	try
		Color = erlang:phash(Id,4),
		update_db(Id,Color)
	catch
		_:_ ->
			lager:error("stage2, unable to detect color for Ball id:~p",[Id])
	end.

update_db(Id,Color)->
	case ets:lookup(?BALLS,Id) of
		[Ball]->
			ets:insert(?BALLS,Ball#balls{stage2={s2,?NOW},color=Color}),
			lager:info("stage2, color detected for Ball");
		[]->
			lager:warning("stage2, Ball with unregisterd Id:~p received",[Id])
	end.

to_stage3(Id)->
	?STAGE_3 ! {receive_ball, Id}.
