-module(qamon_poller_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_poller/2, add_poller/4]).

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
  supervisor:start_child(?MODULE, qamon_poller:supervisor_spec(Name, Model)).

-spec add_poller(atom(), module(), pos_integer(), pos_integer()) -> {ok, pid()}
                                    | {error, already_present | {already_started, pid()}}.
add_poller(Name, Model, Max_History, Interval_Seconds)
  when is_atom(Name),  Name  =/= undefined,
       is_atom(Model), Model =/= undefined,
       is_integer(Max_History), Max_History > 0,
       is_integer(Interval_Seconds), Interval_Seconds > 0 ->
  supervisor:start_child(?MODULE, qamon_poller:supervisor_spec(Name, Model, Max_History, Interval_Seconds)).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
-spec init({}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({}) ->
  {ok, {#{strategy => one_for_one},
        [
        ]
       }
  }.
