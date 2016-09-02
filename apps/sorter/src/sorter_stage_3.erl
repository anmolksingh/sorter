-module(sorter_stage_3).
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
	Dest = get_destination(Id),
	Res = update_db(Id,Dest),
	timer:sleep(1500),
	to_destination(Res,Id,Dest),
    {noreply, State}.

terminate(_Reason, State) ->
    State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_destination(Id)->
	try
		case ets:lookup(?BALLS,Id) of
			[Ball]->
				Ball#balls.color;
			[] ->
				lager:warning("stage3, Ball with unregisterd Id:~p received",[Id]),
				warning
		end
	catch
		_:_ ->
			lager:error("stage3, unable to get the color for Ball id:~p",[Id]),
			error
	end.

update_db(Id,Dest)->
	try
		case ets:lookup(?BALLS,Id) of
			[Ball]->
				ets:insert(?BALLS,Ball#balls{stage3={s3,?NOW,Dest}}),
				lager:info("stage3, destination found for Ball");
			[]->
				lager:warning("stage3, Ball with unregisterd Id:~p received, dropped",[Id]),
				drop
		end
	catch
		_:_ ->
			lager:error("satge3, ball with id:~p cannot be processed, dropped",[Id]),
			drop
	end.

to_destination(drop,_Id,_Dest)->
	ok;
to_destination(ok,Id, Dest)->
	?DESTINATION ! {receive_ball, Id, Dest}.
