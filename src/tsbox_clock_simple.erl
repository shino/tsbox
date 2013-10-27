-module(tsbox_clock_simple).

-behaviour(gen_server).
-behaviour(tsbox_clock).

%% API
-export([start_link/0, stop/0]).
-export([current/0, compare/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {tick = 1 :: pos_integer()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

current() ->
    gen_server:call(?SERVER, current).

compare(Ts, Ts) ->
    identical;
compare(Ts1, Ts2) when Ts2 < Ts1 ->
    descendant;
compare(Ts1, Ts2) when Ts1 < Ts2 ->
    ascendant.

init([]) ->
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(current, _From, #state{tick = Tick} = State) ->
    Reply = {ok, Tick},
    {reply, Reply, State#state{tick = Tick + 1}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
