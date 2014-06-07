-module(tsbox_clock).

-export_type([ts/0]). % timestamp

%% Opaque, -opaque is better?
-opaque ts() :: term().

-callback current() ->
    ts().

-callback future(Ts1::ts(), Ts2::ts()) ->
    ts()       | % One which is absolute future
    identical  | % Both are identical
    concurrent.  % Two are concurrent

-callback compare(Ts1::ts(), Ts2::ts()) ->
    relation().

-type relation() :: decsendant
                  | ascendant
                  | concurrent
                  | identical.
