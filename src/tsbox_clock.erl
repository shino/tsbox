-module(tsbox_clock).

-export_type([ts/0]).

%% Opaque, -opaque is better?
-type ts() :: term().
