-module(job_sim).

-behaviour(gen_server).
-export([start/2, 
         tick/1]).

-export([init/1,
         handle_cast/2,
         handle_call/3]).

-record(job_state, {
            queue_name, 
            processing_time = 0,
            remained_processing = 0
        }).

start(QueueName, ProcessingTime) ->
    gen_server:start(?MODULE, [QueueName, ProcessingTime], []).

tick(Name) ->
    gen_server:call(Name, tick, infinity).

%%------------------------------------------------------------------------------------

init([QueueName, ProcessingTime]) ->
    {ok, #job_state{queue_name = QueueName, processing_time = ProcessingTime}}.

handle_call(tick, _From, State) ->
    #job_state{remained_processing = RemainedProcessing, queue_name = QueueName,
               processing_time = ProcessingTime} = State,
    if 
        RemainedProcessing =< 1 ->
            case queue_sim:pull(QueueName) of
                {ok, _} ->
                    {reply, ok, State#job_state{remained_processing = ProcessingTime}};
                _ ->
                    {reply, ok, State#job_state{remained_processing = 0}}
            end;
        true ->
            {reply, ok, State#job_state{remained_processing = RemainedProcessing - 1}}
    end;
handle_call(Request, _From, State) ->
    {reply, not_implemented, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

