-module(simulation).
-export([run/1]).

run(SimParams) ->
    DefaultParams = #{
        %% assuming one tick is 100 ms
        incoming_rate => 10, % 10 per 100 ms (per tick)
        queue_size => unlimited,
        reneging_timeout => infinity,
        processing_time => 1, % in ticks, 100 ms
        number_of_jobs => 10,
        number_of_ticks => 60000 % 6000 s
    },
    Params = maps:merge(DefaultParams, SimParams),
    run_simulation(Params).

run_simulation(Params) ->
    io:format("~nsimulation params ~p~n~n", [Params]),
    #{queue_size := QueueSize, reneging_timeout := RenegingTimeout,
      processing_time := ProcessingTime, number_of_jobs := NJ,
      incoming_rate := Rate, number_of_ticks := NT} = Params,

    QueueName = queue,
    queue_sim:start(QueueName, QueueSize, RenegingTimeout),
    Jobs = [element(2, job_sim:start(QueueName, ProcessingTime)) || _ <- lists:seq(1,NJ)],
    simulate_ticks(NT, Rate, QueueName, Jobs),
    QueueState = queue_sim:get_state(QueueName),
    io:format("~nqueue state ~p~n~n", [QueueState]),
    ok.

simulate_ticks(0, _Rate, _QueueName, _Jobs) ->
    io:format("~n"),
    ok;
simulate_ticks(N, Rate, QueueName, Jobs) ->
    ((N rem 1000) =:= 0) andalso io:format("."),
    queue_sim:tick(QueueName),
    queue_sim:push(QueueName, Rate),
    [job_sim:tick(J) || J <- Jobs],
    simulate_ticks(N-1, Rate, QueueName, Jobs).
