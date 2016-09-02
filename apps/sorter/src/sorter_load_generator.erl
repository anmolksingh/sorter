-module(sorter_load_generator).
-compile(export_all).
-behavior(gen_server).
-include("sorter.hrl").
-define(SERVER,?MODULE).
-define(STAGE_1, sorter_process_pool:take_member(stage_1_worker)).

start_link()->
	gen_server:start_link({local,?SERVER},?MODULE,[],[{timeout,30000}]).

start()->
	spawn(?MODULE,start,[1000,20]).

start(_,0)->
	ok;
start(Interval,Balls)->
	create_ball(),
	timer:sleep(Interval),
	start(Interval,Balls-1).
	
create_ball()->
	case gen_server:call(?SERVER,create) of
		error ->
			lager:error("Failed to create a ball");
		Id ->
			to_stage1(Id)
	end.

to_stage1(Id)->
	gen_server:cast(?STAGE_1,{receive_ball,Id}).

init([])->
	ets:new(?BALLS, [{keypos, #balls.id}, set, named_table, public]),
	{ok,[]}.

handle_call(create,_From,State)->
	Reply = ball(),
	{reply,Reply,State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.
    
handle_info(_info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

ball()->
	try
		{ok,Id}=flake_server:id(16),
		ets:insert(?BALLS,#balls{id=Id}),
		Id
	catch
		_:_ ->
			error
	end.
