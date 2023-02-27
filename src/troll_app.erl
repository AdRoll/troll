%%%-------------------------------------------------------------------
%% @doc troll public API
%% @end
%%%-------------------------------------------------------------------

-module(troll_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    troll_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
