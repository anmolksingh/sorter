-module(sorter_destination).
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
    
handle_info({receive_ball, Id, Dest}, State) when Dest == 1; Dest ==2; Dest ==3; Dest ==4->
	update_db(Id,Dest),
    {noreply, State};
handle_info({receive_ball, Id, Dest},State)->
	lager:warning("destination, ball:~p with unknown destination:~p arrived, dropped",[Id,Dest]),
	{noreply,State}.

terminate(_Reason, State) ->
    State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_db(Id,Dest)->
	try
		case ets:lookup(?BALLS,Id) of
			[Ball]->
				ets:insert(?BALLS,Ball#balls{destination={dest,?NOW,Dest}}),
				lager:info("destination, Ball reached its destination:~p",[Dest]);
			[]->
				lager:warning("destination, Ball with unregisterd Id:~p received, dropped",[Id])
		end
	catch
		_:_ ->
			lager:error("satge3, ball with id:~p cannot be processed, dropped",[Id])
	end.
