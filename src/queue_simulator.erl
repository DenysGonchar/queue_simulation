-module(queue_simulator).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    simulation:run(#{ %% assuming one tick is 100 ms
                      incoming_rate => 26, % 26 per 100 ms (per tick)
                      queue_size => 500,
                      % reneging_timeout => infinity,
                      reneging_timeout => 10, % in ticks, 1.0 s
                      processing_time => 23, % in ticks, 2.3 s
                      number_of_jobs => 500,
                      number_of_ticks => 60000 % 6000 s
                    }),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

