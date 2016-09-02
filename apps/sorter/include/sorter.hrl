-define(APP,sorter).
-define(BALLS,balls).
-define(STAGE_2,sorter_stage_2).
-define(STAGE_3,sorter_stage_3).
-define(DESTINATION, sorter_destination).
-define(NOW, os:timestamp()).

-record(balls,{
	id,
	color=undefined,
	stage1=undefined,
	stage2=undefined,
	stage3=undefined,
	destination=undefined
	}).
