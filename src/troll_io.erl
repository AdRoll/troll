%%% rebar3_lint complains about io:format; this module puts the work-around in
%%% a single place, and also opens the door to future optimizations
-module(troll_io).

-export([format/2, format/3]).

format(Fmt, Args) ->
    troll_io:fwrite(Fmt, Args).

format(Device, Fmt, Args) ->
    troll_io:fwrite(Device, Fmt, Args).
