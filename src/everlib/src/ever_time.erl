-module(ever_time).

-compile(export_all).


%% seconds
now() ->
    {A, B, _} = erlang:now(),
    A * 1000000 + B.

%% milliseconds
now2() ->
    {A, B, C} = erlang:now(),
    A * 1000000000 + B*1000 + C div 1000.

now3() ->
    {_A, B, C} = erlang:now(),
    B*1000 + C div 1000.

%% microseconds
now_microseconds() ->
    {A, B, C} = erlang:now(),
    A * 1000000000 + B*1000 + C.

now_nanosecond() ->
    {A, B, C} = erlang:now(),
    A * 1000000000000 + B*1000000 + C.
