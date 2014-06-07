-module(tsbox_tests).

-export([setup/0, cleanup/1]).

-include_lib("eunit/include/eunit.hrl").

-define(CLOCK_SERVER, tsbox_clock_simple).
-define(CLOG_STORE,   tsbox_clog_NOT_IMPLEMENTED_YET).

setup() ->
    {ok, Pid} = tsbox_clock_simple:start_link(),
    Pid.

cleanup(_ClockPid) ->
    tsbox_clock_simple:stop(),
    ok.

new_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Create new box and get, results in vacant",
       fun() ->
               Box = tsbox:new([]),
               Xid = tsbox_clock_simple:current(),
               ?assertEqual(vacant, tsbox:read(Xid, Box, ?CLOCK_SERVER, ?CLOG_STORE))
       end}
     ]}.

fake_commit(Writer, Box) ->
    {ok, Committed} = ?CLOCK_SERVER:current(),
    tsbox:set_committed(Writer, Committed, Box).

put_get_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Write -> read in single tx",
       fun() ->
               Box1 = tsbox:new([]),
               io:format(user, "Box1: ~p~n", [Box1]),
               {ok, Writer} = ?CLOCK_SERVER:current(),
               Box2 = tsbox:write(Writer, first, Box1, ?CLOCK_SERVER, ?CLOG_STORE),
               io:format(user, "Box2: ~p~n", [Box2]),
               Box3 = fake_commit(Writer, Box2),
               io:format(user, "Box3: ~p~n", [Box3]),

               {ok, Reader} = ?CLOCK_SERVER:current(),
               ?assertEqual({value, first},
                            tsbox:read(Reader, Box3, ?CLOCK_SERVER, ?CLOG_STORE))
       end},
      {"Read by ascendant tx, can NOT be retrieved",
       fun() ->
               Box1 = tsbox:new([]),
               {ok, Reader} = ?CLOCK_SERVER:current(),
               {ok, Writer} = ?CLOCK_SERVER:current(),
               Box2 = tsbox:write(Writer, first, Box1, ?CLOCK_SERVER, ?CLOG_STORE),
               Box3 = fake_commit(Writer, Box2),
               ?assertEqual(vacant,
                            tsbox:read(Reader, Box3, ?CLOCK_SERVER, ?CLOG_STORE))
       end},
      {"Get by descendant tx, can be retrieved",
       fun() ->
               Box1 = tsbox:new([]),
               {ok, Writer} = ?CLOCK_SERVER:current(),
               Box2 = tsbox:write(Writer, first, Box1, ?CLOCK_SERVER, ?CLOG_STORE),
               Box3 = fake_commit(Writer, Box2),
               {ok, Reader} = ?CLOCK_SERVER:current(),
               ?assertEqual({value, first},
                            tsbox:read(Reader, Box3, ?CLOCK_SERVER, ?CLOG_STORE))
       end},
      {"Two events, different tx sees different values",
       fun() ->
               Box1 = tsbox:new([]),
               {ok, ReaderLongTimeAgo} = ?CLOCK_SERVER:current(),
               {ok, Writer1} = ?CLOCK_SERVER:current(),
               Box2 = tsbox:write(Writer1, first, Box1, ?CLOCK_SERVER, ?CLOG_STORE),
               Box3 = fake_commit(Writer1, Box2),
               {ok, ReaderIntermidiate} = ?CLOCK_SERVER:current(),
               {ok, Writer2} = ?CLOCK_SERVER:current(),
               Box4 = tsbox:write(Writer2, second, Box3, ?CLOCK_SERVER, ?CLOG_STORE),
               Box5 = fake_commit(Writer2, Box4),
               {ok, ReaderTheVeryEnd} = ?CLOCK_SERVER:current(),
               [?assertEqual(Expected,
                             tsbox:read(Reader, Box5, ?CLOCK_SERVER, ?CLOG_STORE))
                || {Reader, Expected} <- [{ReaderLongTimeAgo,  vacant},
                                          {ReaderIntermidiate, {value, first}},
                                          {ReaderTheVeryEnd,   {value, second}}]]
       end}
     ]}.
