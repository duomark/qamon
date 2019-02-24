-module(qamon_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    qamon_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
