-module(epgsql_metrics).

-export([histogram_timed_notify/1]).


histogram_timed_notify({Name, _} = Metric) ->
     try
         ok = folsom_metrics:histogram_timed_notify(Metric)
     catch _:_ ->
             folsom_metrics:new_histogram(Name),
             folsom_metrics:safely_histogram_timed_notify(Metric)
     end.
