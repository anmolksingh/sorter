# sorter

## ARCHITECTURE
Sorter application has below stages
-----------------------------------   
LOAD-GENERATOR
RECEIVER (STAGE-1)
COLOR DETECTOR (STAGE-2)
DESTINATION IDENTIFIER (STAGE-3)
DESTINATION (STAGE-4)
STATS COLLECTOR

- load_generator: This is implemented as gen-server and generates the load, balls in this case each with a unique ID. The balls are forwarded to stage-
- stage_1: Stage-1 has pool of workers. Load-generator process choose any of the available stage_1 workers. Stage-1 checks if the ball received is present in the ets table or not and forwards the ball to stage-2.
- stage-2: Stage-2 is implemented as gen-server. Ball is received and its color is detected and is stored in the ETS table. (here actually the color is assigned to every ball randomly) Ball is then forwarded to stage-3.
- stage-3: Stage-3 is implemented as gen-server. Ball is received and is checked in ETS table for its destination based on the color of the ball recorded in the previous step. Ball is then forwarded to the destination.
- stage-4: This is the destination stage, implemented as gen-server. Ball is received and is checked if present in ETS table or not.

```
NOTE:
- All the stages are independent of each other and are separately supervised.
- For simplicity ball color and ball destination is choosen as 1,2,3,4.
- COLOR DETECTER detects the color (actually assigns the random color to the ball) and if this happens in less than 100 mili seconds then it again detects the color. This has no significance here but
in actuall scenario a sensor can scan the item multiple times to make sure the item is read correctly.
```

## DATA STRUCTURE

The application uses the below record definition:
```
-record(balls,{
    id,
    color,
    stage1,
    stage2,
    stage3,
    destination
    }).
```

On every stage, ETS table is updated which records below information at various stages:
1. load_generator: creates the ball id store it in ETS.
```
{id=id1, color=undefined, stage1=undefined, stage2=undefined, stage3=undefined, destination=undefined}
```
2. Stage_1: stores the time at which ball is received at stage-1
```
{id=id1, color=undefined, stage1={s1,now()}, stage2=undefined, stage3=undefined, destination=undefined}
```

3. stage-2: stores the time at which ball is received at stage-2 and the detected color.
```
{id=id1, color=red, stage1={s1,now()}, stage2={s2,now()}, stage3=undefined, destination=undefined}
```
4. stage-3: stores the time at which ball is received at stage-3 and the destination.
```
{id=id1, color=red, stage1={s1,now()}, stage2={s2,now()}, stage3={s3,now(),red}, destination=undefined}
```
5. destination: stores the time at which ball is received at the destination and the actual destination.
```
{id=id1, color=red, stage1={s1,now()}, stage2={s2,now()}, stage3={s3,now(),red}, destination={dest,now(),red}}
```

NOTE: for the simplicity purpose color and destination are choosen as same (numbers from 1,2,3,4)

## STATISTICS
The application uses a seperate gen_server process to collect the stats. In this case the ETS table is read after every 5 seconds and information related to the number of balls in each stage 
is printed on the console itself (for simplicity purpose).
```
01:54:35.185 STATS BALLS:1  STAGE-1:0  STAGE-2:0  STAGE-3:0  STAGE-4:0
01:54:40.186 STATS BALLS:7  STAGE-1:4  STAGE-2:3  STAGE-3:2  STAGE-4:1
```

NOTE: The stats info would be printing continuously on the shell.
To stop the stats, run below in the shell
```
sorter_stats:remove_stat_picker(collect).
```

To Start the Stats again
```
sorter_stats:load_stats().
```

## HOW TO RUN
- git clone the repository
- make rel
- ./rel/sorter/bin/sorter console
- Inside the shell

```
  sorter_load_generator:start(). %% This will generate 100 balls each after every one sec
  or
  sorter_load_generator:start(IntervalinMs, NumberOfBalls).
```