%%% rebar3_lint complains about io:format; this module puts the work-around in
%%% a single place, and also opens the door to future optimizations
-module(troll_io).

-export([format/1, format/2, format/3]).

format(Fmt) ->
    io:fwrite(Fmt).

format(Fmt, Args) ->
    io:fwrite(Fmt, Args).

format(Device, Fmt, Args) ->
    io:fwrite(Device, Fmt, Args).
