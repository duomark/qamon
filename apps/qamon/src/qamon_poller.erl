-module(qamon_poller).

-behaviour(gen_server).

%% API
-export([start_link/3, start_link/4,
         supervisor_spec/2, supervisor_spec/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-define(MIN_SECONDS, 10).
-define(MAX_HISTORY, 10).

-record(history, {
                  timestamp = erlang:monotonic_time() :: integer(),
                  valid     = undefined               :: boolean() | undefined
                 }).

-record(state, {
                created     = erlang:monotonic_time()    :: integer(),
                name        = undefined                  :: atom(),
                model       = undefined                  :: module(),
                valid       = false                      :: boolean(),
                history     = queue:new()                :: queue:queue(),
                max_history = ?MAX_HISTORY               :: pos_integer(),
                frequency   = undefined                  :: pos_integer() | undefined,
                timer_id    = undefined                  :: timer:tref()  | undefined
               }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(atom(), module(), pos_integer()) -> {ok, pid()}.
start_link(Name, Model, Max_History)
  when is_atom(Name),  Name  =/= undefined,
       is_atom(Model), Model =/= undefined,
       is_integer(Max_History), Max_History > 0 ->
  start_link(Name, Model, Max_History, ?MIN_SECONDS).

-spec start_link(atom(), module(), pos_integer(), pos_integer()) -> {ok, pid()}.
start_link(Name, Model, Max_History, Interval_Seconds)
  when is_atom(Name),  Name  =/= undefined,
       is_atom(Model), Model =/= undefined,
       is_integer(Max_History), Max_History > 0,
       is_integer(Interval_Seconds), Interval_Seconds > 0 ->
  gen_server:start_link({local, ?SERVER}, ?MODULE,
                        {Name, Model, Max_History, Interval_Seconds}, []).

-spec supervisor_spec(atom(), module()) -> supervisor:child_spec().
supervisor_spec(Name, Model)
  when is_atom(Name),  Name  =/= undefined,
       is_atom(Model), Model =/= undefined ->
  supervisor_spec(Name, Model, ?MAX_HISTORY, ?MIN_SECONDS).

-spec supervisor_spec(atom(), module(), pos_integer(), pos_integer())
                     -> supervisor:child_spec().
supervisor_spec(Name, Model, Max_History, Interval_Seconds)
  when is_atom(Name),  Name  =/= undefined,
       is_atom(Model), Model =/= undefined,
       is_integer(Max_History), Max_History > 0,
       is_integer(Interval_Seconds), Interval_Seconds >= 0 ->
    #{id => Name, start => {?MODULE, start_link, [Name, Model, Max_History, Interval_Seconds]}}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init({atom(), module(), pos_integer(), pos_integer()}) -> {ok, #state{}}.
init({Name, Model, Max_History, Interval_Seconds})
  when is_atom(Name),  Name  =/= undefined,
       is_atom(Model), Model =/= undefined,
       is_integer(Max_History), Max_History > 0 ->
  Frequency  = timer:seconds(Interval_Seconds),
  {ok, Tref} = timer:send_interval(Frequency, validate),
  {ok, #state{name=Name, model=Model, max_history=Max_History,
              frequency=Frequency, timer_id = Tref}}.

%%% Respond to timer callback
handle_info(validate, #state{model=Model, valid=Last_Valid} = State) ->
  case Model:is_valid() of
    Last_Valid -> {noreply, State};
    New_Valid  -> {noreply, add_history(State, New_Valid)}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

%%% Unused callbacks
handle_call(_Request, _From, State)  -> {reply, ok, State}.
handle_cast(_Request, State)         -> {noreply,   State}.
code_change(_OldVsn,  State, _Extra) -> {ok,        State}.
format_status(_Opt,  Status)         -> Status.


%%%===================================================================
%%% Support functions
%%%===================================================================
add_history(#state{history=History, max_history=Max_History} = State, New_Valid) ->
  Log_Entry = #history{valid=New_Valid},
  Over_Max  = queue:len(History) >= Max_History,
  New_History = push_history(Over_Max, History, Max_History, Log_Entry),
  State#state{valid=New_Valid, history=New_History}.

push_history(Over_Max, History, Max_History, Log_Entry) ->
  Q1 = queue:in_r(Log_Entry, History),
  case Over_Max of
    false -> Q1;
    true  -> Num_To_Remove = Max_History - queue:len(Q1),
             pop_items(Q1, Num_To_Remove)
  end.

pop_items(Q, 0) -> Q;
pop_items(Q, N) when N > 0 ->
  {_, Q2} = queue:out_r(Q),
  pop_items(Q2, N-1).
