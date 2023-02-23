troll
=====

An OTP application

Build
-----

    $ rebar3 compile

Using
-----

```
1> TF = fun({trace,_,call,{M=lists,F=seq,A=[1,_]}}, _) ->
             start_trace;
           ({trace,_,call,{M=lists,F=seq,A=[2,_]}}, _) ->
             print_trace;
           ({trace,_,call,{M=lists,F=seq,A=[3,_]}}, _) ->
             end_trace;
           ({trace,_,call,{M,F,A}}, _) ->
             end_trace;
           (_,_) ->
             none
        end.
#Fun<erl_eval.43.65746770>

2> troll_retro:start([{lists,seq}],[{some_key,{TF,undefined}}]).
{ok,<0.151.0>}

3> lists:seq(1,2).
[1,2]

4> lists:seq(2,3).
[<0.140.0>] call   lists:seq(2, 3)
[<0.140.0>] return lists:seq(…) -> [1,2]
[<0.140.0>] call   lists:seq(1, 2)
[2,3]

5> lists:seq(3,4).
[<0.140.0>] call   lists:seq(3, 4)
[<0.140.0>] return lists:seq(…) -> [2,3]
[3,4]

6> lists:seq(3,4).
[3,4]

7> lists:seq(2,4).
[2,3,4]
```
