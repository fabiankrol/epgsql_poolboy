-type error_return() :: running
                      | restarting
                      | not_found
                      | simple_one_for_one.

-type stop_return() :: {error, error_return()}.
