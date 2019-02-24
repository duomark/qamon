-module(qamon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
-spec init({}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({}) ->
  {ok, {#{strategy => rest_for_one},
        [
         #{id => qamon_dynamic_sup, start => {qamon_dyn_sup, start_link, []}},
         #{id => qamon_api,         start => {qamon_api,     start_link, []}}
        ]
       }
  }.
