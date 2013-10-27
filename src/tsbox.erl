-module(tsbox).

-export([new/1, put/5, get/4]).

-export_type([history/0]).

-record(history, {
          events = [] :: list(event())
         }).
-record(event, {
          xid :: tsbox_clock:ts(),
          value :: term(),
          hints = [] :: list(term())
         }).

-type history() :: #history{}.
-type event() :: #event{}.

-include_lib("eunit/include/eunit.hrl").

new(_Opts) ->
    #history{}.

put(Xid, Value, #history{events = Events} = Box, _ClockServer, _ClogStore) ->
    Box#history{events = [#event{xid = Xid, value = Value} | Events]}.

get(Xid, #history{events = Events} = _Box, ClockServer, ClogStore) ->
    get_visible(Xid, Events, ClockServer, ClogStore).

%% FIXME: clog should be checked
%% FIXME: no treatment of 'concurrent' yet
get_visible(_GetXid, [], _ClockServer, _ClogStore) ->
    vacant;
get_visible(GetXid, [#event{xid = PutXid} = Event | Rest],
            ClockServer, ClogStore) ->
    ?debugFmt("Event: ~p", [Event]),
    case ClockServer:compare(GetXid, PutXid) of
        identical ->
            {value, Event#event.value};
        descendant ->
            {value, Event#event.value};
        ascendant ->
            get_visible(GetXid, Rest, ClockServer, ClogStore)
    end.
