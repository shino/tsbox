-module(tsbox_clock).

-export_type([ts/0]). % timestamp

-type relation() :: decsendant
                  | ascendant
                  | concurrent
                  | identical.

%% Opaque, -opaque is better?
-type ts() :: term().

-callback current() ->
    ts().

-callback compare(Ts1::ts(), Ts2::ts()) ->
    relation().
