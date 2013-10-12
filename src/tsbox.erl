-module(tsbox).

-export([new/1, put/5, get/4]).

-export_type([history/0]).

-record(history, {
          track = [] :: list(step())
         }).
-record(step, {
          xid :: tsbox_clock:ts(),
          value :: term(),
          hints = [] :: list(term())
         }).

-type history() :: #history{}.
-type step() :: #step{}.

new(_Opts) ->
    #history{}.

put(Xid, Value, #history{track = Track} = Box, _ClockServer, _ClogStore) ->
    Box#history{track = [#step{xid = Xid, value = Value} | Track]}.

get(_Xid, #history{track = Track} = _Box, _ClockServer, _ClogStore) ->
    case Track of
        [] ->
            vacant;
        [#step{value = Value} = _Step | _] ->
            {value, Value}
    end.
