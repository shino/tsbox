-module(tsbox).

-export([new/1, write/5, read/4, set_committed/3]).

-export_type([history/0]).

-record(history, {
          events = [] :: [event()]
         }).
-record(event, {
          writer :: tsbox_clock:ts(),    % clock when writer begin tx
          committed :: tsbox_clock:ts(), % clock when write commit tx
          value :: term(),
          hints = [] :: hints()
         }).

-type history() :: #history{}.
-type event() :: #event{}.

-type hints() :: [hint()].
-type hint() :: aborted.

-type clock() :: atom() | pid().
-type clog() :: atom() | pid().

-type reader() :: tsbox_clock:ts().
-type writer() :: tsbox_clock:ts().

-include_lib("eunit/include/eunit.hrl").

new(_Opts) ->
    #history{}.

-spec write(writer(), term(), history(), clock(), clog()) ->
                   UpdatedHistory::history().
write(WriterXid, Value, #history{events = Events} = Box, _ClockServer, _ClogStore) ->
    Box#history{events = [#event{writer = WriterXid, value = Value} | Events]}.

-type get_result() :: vacant | {value, Value::term()} | {error, Reason::term()}.
-spec read(reader(), history(), clock(), clog()) ->
                 {get_result(), UpdatedHistory::history()}.
read(ReaderXid, #history{events = Events} = _Box, ClockServer, ClogStore) ->
    read_visible(ReaderXid, Events, ClockServer, ClogStore).

set_committed(WriterXid, Committed, #history{events=Events} = Box) ->
    Box#history{events = set_committed(WriterXid, Committed, Events, [])}.

set_committed(_WriterXid, _Committed, [], Stacked) ->
    lists:reverse(Stacked);
set_committed(WriterXid, Committed, [#event{writer = WriterXid} = Event | Rest], Stacked) ->
    lists:reverse(Stacked, [Event#event{committed=Committed} | Rest]);
set_committed(WriterXid, Committed, [Event | Rest], Stacked) ->
    set_committed(WriterXid, Committed, Rest, [Event | Stacked]).

%% TODO: clog should be checked
%% TODO: handling of '$deleted'
read_visible(_ReaderXid, [], _ClockServer, _ClogStore) ->
    vacant;
read_visible(_ReaderXid, [#event{committed = undefined} = _Event | _Rest],
            _ClockServer, _ClogStore) ->
    %% Query to clog server and update event
    {error, 'NOT_IMPLEMENTED_YET'};
read_visible(ReaderXid, [#event{committed = Committed} = Event | Rest],
            ClockServer, ClogStore) ->
    ?debugFmt("Event: ~p", [Event]),
    case ClockServer:future(Committed, ReaderXid) of
        ReaderXid ->
            {value, Event#event.value};
        _ ->
            read_visible(ReaderXid, Rest, ClockServer, ClogStore)
    end.
