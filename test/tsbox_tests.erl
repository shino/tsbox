-module(tsbox_tests).

-export([setup/0, cleanup/1]).

-include_lib("eunit/include/eunit.hrl").

-define(CLOCK_SERVER, tsbox_clock_simple).
-define(CLOG_STORE,   tsbox_clog_ss).

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
               ?assertEqual(vacant, tsbox:get(Xid, Box, ?CLOCK_SERVER, ?CLOG_STORE))
       end}
     ]}.

put_get_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Put and get in single tx",
       fun() ->
               Box1 = tsbox:new([]),
               {ok, Xid} = ?CLOCK_SERVER:current(),
               Box2 = tsbox:put(Xid, first, Box1, ?CLOCK_SERVER, ?CLOG_STORE),
               ?assertEqual({value, first},
                            tsbox:get(Xid, Box2, ?CLOCK_SERVER, ?CLOG_STORE))
       end},
      {"Get by ascendant tx, can NOT be retrieved",
       fun() ->
               Box1 = tsbox:new([]),
               {ok, GetTx} = ?CLOCK_SERVER:current(),
               {ok, PutTx} = ?CLOCK_SERVER:current(),
               Box2 = tsbox:put(PutTx, first, Box1, ?CLOCK_SERVER, ?CLOG_STORE),
               ?assertEqual(vacant,
                            tsbox:get(GetTx, Box2, ?CLOCK_SERVER, ?CLOG_STORE))
       end},
      {"Get by descendant tx, can be retrieved",
       fun() ->
               Box1 = tsbox:new([]),
               {ok, PutTx} = ?CLOCK_SERVER:current(),
               Box2 = tsbox:put(PutTx, first, Box1, ?CLOCK_SERVER, ?CLOG_STORE),
               {ok, GetTx} = ?CLOCK_SERVER:current(),
               ?assertEqual({value, first},
                            tsbox:get(GetTx, Box2, ?CLOCK_SERVER, ?CLOG_STORE))
       end},
      {"Two events, different tx sees different values",
       fun() ->
               Box1 = tsbox:new([]),
               {ok, GetLongTimeAgo} = ?CLOCK_SERVER:current(),
               {ok, PutTx} = ?CLOCK_SERVER:current(),
               Box2 = tsbox:put(PutTx, first, Box1, ?CLOCK_SERVER, ?CLOG_STORE),
               {ok, GetIntermidiate} = ?CLOCK_SERVER:current(),
               {ok, PutMoreTx} = ?CLOCK_SERVER:current(),
               Box3 = tsbox:put(PutMoreTx, second, Box2, ?CLOCK_SERVER, ?CLOG_STORE),
               {ok, GetTheVeryEnd} = ?CLOCK_SERVER:current(),
               [?assertEqual(Expected,
                             tsbox:get(GetTx, Box3, ?CLOCK_SERVER, ?CLOG_STORE))
                || {GetTx, Expected} <- [{GetLongTimeAgo,  vacant},
                                         {GetIntermidiate, {value, first}},
                                         {GetTheVeryEnd,   {value, second}}]]
       end}
     ]}.
