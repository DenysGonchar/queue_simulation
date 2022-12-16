-module(queue_simulator).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("~nArgs: ~p~n", [Args]),
    case process_args(Args) of
        {ok, SimulationParams} ->
            simulation:run(SimulationParams);
        {error, Message} ->
            io:format("~nERROR: ~s~n", [Message]),
            io:format("~nRun 'queue_simulator --help' for a complete"
                      " list of supported options.~n~n");
        print_help ->
            print_help_message()
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

process_args(Args) ->
    process_args(Args, #{}).

process_args([], Params) ->
    {ok, Params};
process_args(["--help" | _], _) ->
    print_help;
process_args(["--" ++ ParameterName | TailArgs], Params) ->
    case add_param(ParameterName, TailArgs, Params) of
        {ok, NewParams, NewTailArgs} ->
            process_args(NewTailArgs, NewParams);
        {error, Message} ->
            {error, "--" ++ ParameterName ++ ": " ++ Message}
    end;
process_args([InvalidArg|_],_) ->
    {error, InvalidArg ++ ": invalid argument"}.

add_param(ParameterName, TailArgs, Params) ->
    case is_known_parameter(ParameterName) of
        {ok, Parameter} ->
            case convert_value(Parameter, TailArgs) of
                {ok, Value, NewTailArgs} -> 
                    {ok, Params#{Parameter => Value}, NewTailArgs};
                Error -> Error
            end;
        Error -> Error
    end.

is_known_parameter(ParameterName)->
    ValidParameters = [incoming_rate, number_of_jobs, reneging_timeout,
                       processing_time, queue_size, number_of_ticks],
    try list_to_existing_atom(ParameterName) of
        Parameter ->
            case lists:member(Parameter, ValidParameters) of
                true -> {ok, Parameter};
                false -> {error, "unknown argument"}
            end

    catch 
        _:_ -> {error, "unknown argument"}
    end.


convert_value(_Parameter, ["--" ++ _ | _]) ->
    %% so far we do not support any arguments without value
    {error, "missing value"};
convert_value(_Parameter, []) ->
    %% so far we do not support any arguments without value
    {error, "missing value"};
convert_value(_Parameter, [Value | TailArgs]) ->
    %% for now all of the arguments expect one integer value
    try list_to_integer(Value) of
        IntValue -> {ok, IntValue, TailArgs}
    catch
        error:badarg -> {error, "invalid value '" ++ Value ++ "'"}
    end.

print_help_message() -> 
    io:format("~nqueue_simulator options:~n"
              "  --incoming_rate <positive integer>    - input rate (requests per tick)~n"
              "  --number_of_jobs <positive integer>   - number of jobs processing requests from the queue~n"
              "  --processing_time <positive integer>  - number of ticks required for processing 1 request~n"
              "                                          by one job~n"
              "  --queue_size <positive integer>       - maximum amount of requests that can be stored in~n"
              "                                          the queue. if the queue is full, requests are balked.~n"
              "                                          by default, the queue size is unlimited.~n"
              "  --reneging_timeout <positive integer> - number of ticks a request can wait in the queue~n"
              "                                          before reneging. if the request is not consumed~n"
              "                                          for processing (by any job) within that number of~n"
              "                                          ticks, it's dropped from the queue. by default, the~n"
              "                                          timeout is set to infinity~n"
              "  --number_of_ticks <positive integer>  - number of ticks to simulate~n~n"
             ).
