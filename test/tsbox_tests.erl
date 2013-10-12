-module(tsbox_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CLOCK_SERVER, tsbox_clock_ss).
-define(CLOG_STORE,   tsbox_clog_ss).

new_test_() ->
    [
     fun() ->
             Box = tsbox:new([]),
             Xid = 1,
             ?assertEqual(vacant, tsbox:get(Xid, Box, ?CLOCK_SERVER, ?CLOG_STORE))
     end
    ].

put_get_test_() ->
    [
     fun() ->
             Box1 = tsbox:new([]),
             Xid = 1,
             Box2 = tsbox:put(Xid, first, Box1, ?CLOCK_SERVER, ?CLOG_STORE),
             ?assertEqual({value, first},
                          tsbox:get(Xid, Box2, ?CLOCK_SERVER, ?CLOG_STORE))
     end
    ].
