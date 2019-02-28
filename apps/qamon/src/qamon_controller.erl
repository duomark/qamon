-module(qamon_controller).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init({}) -> {ok, #state{}}.
init({}) ->
  {ok, #state{}}.

handle_info(_Info, State)  -> {noreply, State}.

%%% Unused callbacks
handle_call   (_Request, _From, State)  -> {reply, ok, State}.
handle_cast   (_Request, State)         -> {noreply,   State}.
code_change   (_OldVsn,  State, _Extra) -> {ok,        State}.
terminate     (_Reason, _State)         -> ok.
format_status (_Opt,  Status)           -> Status.


%%%===================================================================
%%% Support functions
%%%===================================================================
