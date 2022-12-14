-module(queue_sim).

-behaviour(gen_server).
-export([start/3, 
         tick/1,
         push/2,
         pull/1,
         get_state/1]).

-export([init/1,
        handle_cast/2,
        handle_call/3]).

-record(queue_state, {
            name,
            current_tick = 0,
            queue_size = 0, %% the maximum number of items in the queue
            reneging_timeout = infinity, %% infinity or positive integer
            max_queued = 0, %% the number of maximum queued items
            queued_items = [], %% items queue, new items are added at the beginning of the list
            balked_items = [], %% all balked items
            reneged_items = [], %% all reneged items
            pulled_items = [], %% all pulled items
            max_waiting_time = 0 %%  max waiting time from queueing to pulling
        }).

start(Name, QueueSize, RenegingTimeout) ->
    Params = #{queue_size => QueueSize, name => Name, 
               reneging_timeout => RenegingTimeout},
    gen_server:start({local, Name}, ?MODULE, Params, []).

tick(Name) ->
    gen_server:call(Name, tick, infinity).

push(Name, N) ->
    gen_server:call(Name, {push, N}, infinity).

pull(Name) ->
    gen_server:call(Name, pull, infinity).

get_state(Name) ->
    gen_server:call(Name, get_state, infinity).
%%------------------------------------------------------------------------------------

init(#{queue_size := QueueSize, name := Name, reneging_timeout := RenegingTimeOut}) ->
    {ok, #queue_state{name = Name, queue_size = QueueSize,
                      reneging_timeout = RenegingTimeOut}}.


handle_call(pull, _From, State) ->
    #queue_state{current_tick = CurrentTick, queued_items = QueuedItems,
                 pulled_items = PulledItems, max_waiting_time = MaxWaitingTime} = State,
    case QueuedItems of 
        [] -> {reply, {error, no_item}, State};
        _ ->
            I = lists:last(QueuedItems),
            NewQueuedItems = QueuedItems -- [I],
            #{queueing_time := T} = I,
            NewI = I#{pulled_time => CurrentTick},
            NewPulledItems = [NewI | PulledItems],
            NewMaxWaitingTime = max(MaxWaitingTime, CurrentTick - T),
            NewState = State#queue_state{queued_items = NewQueuedItems,
                                         pulled_items = NewPulledItems,
                                         max_waiting_time = NewMaxWaitingTime},
            {reply, {ok, NewI}, NewState}
    end;
handle_call(get_state, _From, State) ->
    #queue_state{pulled_items = PulledItems, queued_items = QueuedItems,
                 reneged_items=RenegedItems, balked_items = BalkedItems} = State,
    Fields = record_info(fields, queue_state),
    TraceableState = State#queue_state{pulled_items = length(PulledItems),
                                       queued_items = length(QueuedItems),
                                       reneged_items = length(RenegedItems),
                                       balked_items = length(BalkedItems)},
    ProplistState = lists:zip(Fields, tl(tuple_to_list(TraceableState))),
    {reply, maps:from_list(ProplistState), State};
handle_call({push, N}, _From, State) ->
    #queue_state{current_tick = CurrentTick, queued_items = QueuedItems,
                 balked_items = BalkedItems, max_queued = MaxQueued, 
                 queue_size = Size} = State,
    BalkedN = max(Size, (N + length(QueuedItems))) - Size,
    QueuedN = N - BalkedN,
    % io:format("queued_items ~p~n", [length(QueuedItems)]),
    % io:format("adding balked ~p~n", [BalkedN]),
    % io:format("adding queued ~p~n", [QueuedN]),
    Balked = [#{balking_time => CurrentTick} || _ <- lists:seq(1, BalkedN)],
    Queued = [#{queueing_time => CurrentTick}|| _ <- lists:seq(1, QueuedN)],
    NewQueuedItems = Queued ++ QueuedItems,
    NewBalkedItems = Balked ++ BalkedItems,
    NewMaxQueued = max(MaxQueued, length(NewQueuedItems)),
    NewState = State#queue_state{queued_items = NewQueuedItems,
                                 balked_items = NewBalkedItems,
                                 max_queued = NewMaxQueued},
     {reply, ok, NewState};
handle_call(tick, _From, State) ->
    #queue_state{current_tick = CurrentTick, queued_items = QueuedItems,
                 reneged_items = RenegedItems, reneging_timeout = RenegingTimeout} = State,
    NewCurrentTick = CurrentTick + 1,
    {NewQueuedItems, NewRenegedItems} = renege_items(NewCurrentTick, RenegingTimeout,
                                                     QueuedItems, RenegedItems),
    NewState = State#queue_state{current_tick = NewCurrentTick,
                                 reneged_items = NewRenegedItems,
                                 queued_items = NewQueuedItems},
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, not_implemented, State}.



handle_cast(_Request, State) ->
    {noreply, State}.


renege_items(CurrentTick, RenegingTimeout, QueuedItems, RenegedItems) ->
    {StillQueuedItems, NewRenegedItems} = lists:foldl(
        fun(#{queueing_time := T} = I,{Q,R}) ->
            if
                CurrentTick - T > RenegingTimeout ->
                    %% The comparison is correct if RenegingTimeout
                    %% is an atom (e.g. 'infinity'), an atom is always
                    %% greater than a number
                    {Q,[I#{reneging_time => CurrentTick}|R]};
                true -> 
                    {[I|Q],R}
            end
        end, {[],RenegedItems},QueuedItems),
    {lists:reverse(StillQueuedItems), NewRenegedItems}.
