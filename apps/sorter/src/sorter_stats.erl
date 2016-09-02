%%--------------------------------------------------------------------
%% @author Anmol Kumar Singh <anmol@dasudian.com>
%%%-----------------------------------------------------------------------------
-module(sorter_stats).
-include_lib("stdlib/include/ms_transform.hrl").
-include("sorter.hrl").
-behaviour(gen_server).


-define(TABLE, stats_picker).
-define(SERVER, stats_picker_server).
-define(STATS_DEFINED,[collect]).

-record(stats_picker, {
        name,
        ref,
        callback,
        interval,
        status,
        lastrun,
        nextrun}).

-export([start_link/0]).
-compile(export_all).

start_link()->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [],[{timeout,30000}]).

add_stat_picker(Name, Callback, Interval)->
	add_stat_picker(Name, Callback, Interval, false).

add_stat_picker(Name, {M,F,A}, Interval, Starttime)->
	Verify_Args = [test], %% This is to make sure that user knows which function it is calling. 
	try M:F(Verify_Args) of
	test_ok ->
		%gen_server:call(?SERVER, {create_picker, Name, {M,F,A}, Interval, Starttime})
		create_picker(Name, {M,F,A}, Interval, Starttime)
	catch
	 _:_Why ->
		lager:error("callback Function in not test verified")
	end.

get_status(Name)->
	case ets:lookup(?TABLE, Name) of
	[R] ->
		R#stats_picker.status;
	_ ->
		undefined
	end.

deactivate(Name)->
    case ets:lookup(?TABLE, Name) of
    [R]->
        NewR = R#stats_picker{status = inactive},
        ets:insert(?TABLE, NewR),
        get_status(Name);
    _ ->
        undefined
    end.
remove_stat_picker(Name)->
    case ets:lookup(?TABLE, Name) of
        [R]->
            deactivate(Name),
            TRef = R#stats_picker.ref,
            erlang:cancel_timer(TRef),
            ets:delete(?TABLE, Name);
        [] ->
            not_present
    end.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================
init([])->
	ets:new(?TABLE, [{keypos, #stats_picker.name}, set, named_table, public]),
	load_stats(),
	{ok, []}.

handle_call(_Request, _From, State)->
	{reply, ok, State}.

handle_cast(_Request, State)->
	{noreply, State}.

handle_info({stat_ticker,Stats,{M,F,A}}, State)->
	case lists:member(Stats, ?STATS_DEFINED) of
		true ->
			check_for_specialFlag(Stats),
			case check_status(Stats) of
				active ->
					spawn(M,F,[A]);	
				_Else ->
					igonre
			end,
			{noreply, State};
		false ->
			lager:error("Received message from: ~p, no handler defined, removing stat picker entry",[Stats]),
        	remove_stat_picker(Stats),	
			{noreply, State}
	end;

handle_info(Info, State)->
	CurrentTime = calendar:local_time(),
	lager:error("Received ~p at time: ~p",[Info, CurrentTime]),
	{noreply, State}.
	
terminate(_Reason, State)->
        State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%% start_time: {{2015,11,26},{19,14,42}}
%% Interval: in seconds
create_picker(Name, Callback, Interval, Starttime)->
lager:info("Trying to create stats picker:~p, Callack:~p, Interval:~p, Stattime:~p",[Name, Callback, Interval, Starttime]),
try
case ets:lookup(?TABLE, Name) of
	[]->
		Msg = {stat_ticker, Name, Callback},
		if 
			(Starttime =/= false)->
				NewStartTime = check_time(Starttime, Interval),
				NextRun = calendar:datetime_to_gregorian_seconds(NewStartTime),
				CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
				SendAfter = NextRun - CurrentTime, 
				if(SendAfter < 0) -> 
					lager:error("Invalid Start time"),
					{error,invalid_start_time};
				true ->
					ok
				end;
			true ->
				SendAfter = Interval,
				CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
                NextRun = Interval + CurrentTime
		end,
		TRef = erlang:send_after(SendAfter*1000, ?SERVER, Msg),
		NewR = #stats_picker{name = Name, interval = Interval, callback = Callback, ref = TRef, status = active, nextrun = NextRun},
		ets:insert(?TABLE, NewR),
		lager:info("Stats-picker: ~p Name created successfully",[Name]),
		ok;
	_ ->
		lager:error("Stat picker with name:~p already exist",[Name]),
		ok
end
catch
	E1:E2 ->
		lager:error("Unable to create stats-Picker, failed with reason ~p:~p",[E1,E2])
end.

load_stats()->
	Stats = application:get_env(?APP, stats, []),
	load(Stats).

load([])->
	ok;
load([{Name, {M,F,A}, Interval, StartTime}|Tail]) ->
    add_stat_picker(Name, {M,F,A}, Interval, StartTime),	
	load(Tail);
load([{Name, {M,F,A}, Interval}|Tail]) ->
    add_stat_picker(Name, {M,F,A}, Interval),	
	load(Tail).

check_time({{Y,M,D},T}, Interval)->
	NextRun = calendar:datetime_to_gregorian_seconds({{Y,M,D},T}),
	CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	SendAfter = NextRun - CurrentTime, 
	if(SendAfter < 0) -> 
		NewStartTime = addInterval({{Y,M,D},T}, Interval),
		check_time(NewStartTime, Interval);
	true ->
		{{Y,M,D},T}	
	end.

addInterval({{Y,M,D},T}, daily)->
	NextDay = {Y,M,D+1},
	case calendar:valid_date(NextDay) of
		true ->
			{{Y,M,D+1},T};
		false ->
			NextMonth = {Y,M+1,1},
			case calendar:valid_date(NextMonth) of
				true -> 
					{{Y,M+1,1},T};
				false ->
					{{Y+1,1,1},T}
			end
	end;

addInterval({{Y,M,D},T}, Interval)->
	Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	StartTime = calendar:datetime_to_gregorian_seconds({{Y,M,D},T}),
	if(StartTime + Interval > Now)->
		NewStartTime = StartTime + Interval,
		calendar:gregorian_seconds_to_datetime(NewStartTime);
	true ->
		SD = calendar:gregorian_seconds_to_datetime(StartTime + Interval),
		addInterval(SD, Interval)
	end.
	
check_for_specialFlag(Name)->
	case ets:lookup(?TABLE, Name) of
	[R] ->
		Ref = R#stats_picker.ref,
		erlang:cancel_timer(Ref),
		Interval = R#stats_picker.interval,
		CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
		Callback = R#stats_picker.callback,
		Msg = {stat_ticker, Name, Callback},
		NextRun = CurrentTime +  Interval,
		NewTimer = erlang:send_after(Interval*1000, ?SERVER, Msg),
		NewR = R#stats_picker{nextrun = NextRun, lastrun = CurrentTime, ref = NewTimer},
		ets:insert(?TABLE, NewR);
	_ ->
		undefined
	end.

check_status(Name) ->
	case get_status(Name) of
		{suspend, Suspend}->
			CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
			if 
				(CurrentTime >= Suspend)->
					[R] = ets:lookup(?TABLE, Name),
					NewR = R#stats_picker{status = active},
					ets:insert(?TABLE, NewR),
					active;
				true ->
					suspend
			end;
		Status ->
			Status
	end.

collect([])->
	Data = ets:tab2list(?BALLS),
	Balls = length(Data),
	Stage1 = length([S1||{balls,_ID,_COLOR,S1,_S2,_S3,_Dest}<-Data, S1 =/= undefined]),
	Stage2 = length([S2||{balls,_ID,_COLOR,_S1,S2,_S3,_Dest}<-Data, S2 =/= undefined]),
	Stage3 = length([S3||{balls,_ID,_COLOR,_S1,_S2,S3,_Dest}<-Data, S3 =/= undefined]),
	Dest = length([Dest||{balls,_ID,_COLOR,_S1,_S2,_S3,Dest}<-Data, Dest =/= undefined]),
	lager:info("STATS                                                     BALLS:~p  STAGE-1:~p  STAGE-2:~p  STAGE-3:~p  STAGE-4:~p",[Balls,Stage1,Stage2,Stage3,Dest]),
	ok;
collect([test])->
	test_ok.
