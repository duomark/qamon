-module(qamon_dynamic_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_poller/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

-spec add_poller(atom(), module()) -> {ok, pid()}
                                    | {error, already_present | {already_started, pid()}}.
add_poller(Name, Model)
  when is_atom(Name),  Name  =/= undefined,
       is_atom(Model), Model =/= undefined ->
  supervisor:start_child(?MODULE, qamon_poller:supervisor_spec(Name), Model).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
-spec init({}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({}) ->
  {ok, {#{strategy => one_for_one}, []}}.
