-module(epgsql_metrics).

-export([histogram_timed_notify/1]).
-export([notify_histogram/1]).
-export([notify_histogram/2]).


histogram_timed_notify(Timer) ->
    case folsom_metrics:safely_histogram_timed_notify(Timer) of
        {error, Name, nonexistent_metric} ->
            folsom_metrics:new_histogram(Name),
            folsom_metrics:safely_histogram_timed_notify(Timer);
        ok ->
            ok
    end.

notify_histogram(Event) ->
    notify(new_histogram, Event).
notify_histogram(Name, Value) ->
    notify(new_histogram, Name, Value).

notify(Fun, {Name, Value}) ->
    notify(Fun, Name, Value).

notify(Fun, Name, Value) ->
    case folsom_metrics:safely_notify(Name, Value) of
        {error, Name, nonexistent_metric} ->
            folsom_metrics:Fun(Name),
            folsom_metrics:safely_notify(Name, Value);
        ok ->
            ok
    end.
