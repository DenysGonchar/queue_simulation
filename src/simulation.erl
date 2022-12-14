-module(simulation).
-export([run/1]).

run(SimParams) ->
    DefaultParams = #{
        %% assuming one tick is 100 ms
        incoming_rate => 11, % 11 per 100 ms (per tick)
        queue_size => 150,
        % reneging_timeout => infinity,
        reneging_timeout => 13, % in ticks, 1.3 s
        processing_time => 12, % in ticks, 1.2 s
        number_of_jobs => 140,
        number_of_ticks => 60000 % 6000 s
    },
    Params = maps:merge(DefaultParams, SimParams),
    run_simulation(Params).

run_simulation(Params) ->
    #{queue_size := QueueSize, reneging_timeout := RenegingTimeout} = Params,
    QueueName = queue,
    queue_sim:start(QueueName, QueueSize, RenegingTimeout),
    #{processing_time := ProcessingTime, number_of_jobs := NJ} = Params,
    Jobs = [element(2, job_sim:start(QueueName, ProcessingTime)) || _ <- lists:seq(1,NJ)],
    #{incoming_rate := Rate, number_of_ticks := NT} = Params,
    simulate_ticks(NT, Rate, QueueName, Jobs),
    QueueState = queue_sim:get_state(QueueName),
    io:format("~nsimulation params ~p~n", [Params]),
    io:format("~nqueue state ~p~n", [QueueState]),
    ok.

simulate_ticks(0, _Rate, _QueueName, _Jobs) ->
    ok;
simulate_ticks(N, Rate, QueueName, Jobs) ->
    % io:format("~nsim iteration ~p~n", [N]),
    queue_sim:tick(QueueName),
    queue_sim:push(QueueName, Rate),
    [job_sim:tick(J) || J <- Jobs],
    simulate_ticks(N-1, Rate, QueueName, Jobs).
