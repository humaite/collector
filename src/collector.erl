%%%===================================================================
%%% @doc A quick and dirty extended collector metrics interface.
%%%
%%% == Custom Prometheus Registry ==
%%%
%%% Exposing BEAM metrics can be risky. To avoid exposing confidential
%%% data, it is possible to use a custom registry.
%%%
%%% ```
%%% collector:start_link(#{ registry => custom_registry }).
%%% '''
%%%
%%% The metrics will be stored in `/tmp/collector.prom'.
%%%
%%% @end
%%%===================================================================
-module(collector).
-export([
	 registered_processes/0,
	 process_info/1,
	 process_info/2,
	 processes_info/0,
	 processes_info/1,
	 processes_info/2,
	 prometheus_labels/0,
	 prometheus_labels/1,
	 prometheus_init/0,
	 prometheus_init/1,
	 prometheus_deregister/0,
	 prometheus_deregister/1,
	 prometheus_apply/0,
	 prometheus_apply/1,
	 prometheus_gauges/0,
	 prometheus_gauges/1,
	 start_link/0,
	 start_link/1,
	 stop/0
]).
-compile({no_auto_import,[process_info/2]}).

%%--------------------------------------------------------------------
%% @doc returns the list of processes with their registered name.
%% @end
%%--------------------------------------------------------------------
registered_processes() ->
    [ {X, erlang:whereis(X)} || X <-  erlang:registered() ].

%%--------------------------------------------------------------------
%% @doc convert a pid term to binary.
%% @end
%%--------------------------------------------------------------------
pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

%%--------------------------------------------------------------------
%% @doc list processes info.
%% @see processes_info/1
%% @end
%%--------------------------------------------------------------------
processes_info() ->
    processes_info(#{}).

%%--------------------------------------------------------------------
%% @doc list processes info.
%% @see processes_info/2
%% @end
%%--------------------------------------------------------------------
processes_info(Opts) ->
    processes_info(erlang:processes(), Opts).

%%--------------------------------------------------------------------
%% @doc list extended processes info.
%% @end
%%--------------------------------------------------------------------
processes_info(Pids, Opts) ->
    lists:foldr(
      fun(P, A) -> [process_info(P,Opts)|A] end,
      [],
      Pids).

%%--------------------------------------------------------------------
%% @doc display extended process info for a running process.
%% @see process_info/2
%% @end
%%--------------------------------------------------------------------
process_info(Pid) ->
    process_info(Pid, #{}).

%%--------------------------------------------------------------------
%% @doc display extended process info for a running process.
%% @end
%%--------------------------------------------------------------------
process_info(Pid, Opts)
  when is_pid(Pid), is_map(Opts) ->
    case erlang:process_info(Pid) of
	Info when is_list(Info) ->
	    process_info(Pid, Info, [{pid, pid_to_binary(Pid)}], Opts);
	undefined -> 
	    Buffer = [{pid, pid_to_binary(Pid)}, {status, undefined}],
	    process_info_final(Pid, Buffer, Opts)
    end;
process_info(RegisteredName, Opts) 
  when is_atom(RegisteredName), RegisteredName =/= undefined ->
    Pid = erlang:whereis(RegisteredName),
    process_info(Pid, Opts).
    
%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
process_info(Pid, [], Buffer, Opts) ->
    process_info_registered(Pid, Buffer, Opts);
process_info(Pid, [{current_function, {M, F, A}}|Rest], Buffer, Opts) ->
    NewBuffer = [ {current_module, M}
		, {current_function, F}
		, {current_arity, A}
		| Buffer],
    process_info(Pid, Rest, NewBuffer, Opts);
process_info(Pid, [{initial_call,{M, F, A}}|Rest], Buffer, Opts) ->
    NewBuffer = [ {initial_module, M}
		, {initial_function, F}
		, {initial_arity, A}
		| Buffer],
    process_info(Pid, Rest, NewBuffer, Opts);
process_info(Pid, [S = {status, _}|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, [S|Buffer], Opts);
process_info(Pid, [Q = {message_queue_len, _}|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, [Q|Buffer], Opts);
process_info(Pid, [{links, L}|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, [{links, length(L)}|Buffer], Opts);
process_info(Pid, [T = {trap_exit, _}|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, [T|Buffer], Opts);
process_info(Pid, [E = {error_handler, _}|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, [E|Buffer], Opts);
process_info(Pid, [{group_leader, G}|Rest], Buffer, Opts) ->
    NewBuffer = [ {group_leader, pid_to_binary(G)}
		| Buffer],
    process_info(Pid, Rest, NewBuffer, Opts);
process_info(Pid, [T = {total_heap_size, _}|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, [T|Buffer], Opts);
process_info(Pid, [T = {heap_size, _}|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, [T|Buffer], Opts);
process_info(Pid, [T = {stack_size, _}|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, [T|Buffer], Opts);
process_info(Pid, [R = {reductions, _}|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, [R|Buffer], Opts);
process_info(Pid, [{garbage_collection, G}|Rest], Buffer, Opts) ->
    NewBuffer = process_info_garbage_collection(G, [], Opts) ++ Buffer,
    process_info(Pid, Rest, NewBuffer, Opts);	
process_info(Pid, [_|Rest], Buffer, Opts) ->
    process_info(Pid, Rest, Buffer, Opts).

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
process_info_garbage_collection([], Buffer, _Opts) -> Buffer;
process_info_garbage_collection([X = {min_bin_vheap_size,_}|Rest], Buffer, Opts) ->
    process_info_garbage_collection(Rest, [X|Buffer], Opts);
process_info_garbage_collection([X = {min_heap_size, _}|Rest], Buffer, Opts) ->
    process_info_garbage_collection(Rest, [X|Buffer], Opts);
process_info_garbage_collection([X = {fullsweep_after,_}|Rest], Buffer, Opts) ->
    process_info_garbage_collection(Rest, [X|Buffer], Opts);
process_info_garbage_collection([X = {minor_gcs, _}|Rest], Buffer, Opts) ->
    process_info_garbage_collection(Rest, [X|Buffer], Opts);
process_info_garbage_collection([_|Rest], Buffer, Opts) ->
    process_info_garbage_collection(Rest, Buffer, Opts).

%%--------------------------------------------------------------------
%% @hidden
%%
%% add the registered name if it exists.
%%
%%--------------------------------------------------------------------
process_info_registered(Pid, Buffer, Opts = #{ without_registered_name := true }) ->
    process_info_extra(Pid, Buffer, Opts);
process_info_registered(Pid, Buffer, Opts) ->
    RP = registered_processes(),
    Search = [ X || X = {_Name, P} <- RP, P =:= Pid ],
    case Search of
	[{Name, Pid}] ->
	    process_info_extra(Pid, [{registered_name, Name}|Buffer], Opts);
	_ ->
	    process_info_extra(Pid, [{registered_name, undefined}|Buffer], Opts)
    end.

%%--------------------------------------------------------------------
%% @hidden
%% 
%% not all information are present at the same place, this function
%% appends them from other places.
%%
%%--------------------------------------------------------------------
process_info_extra(Pid, Buffer, Opts = #{ without_extra := true }) ->
    process_info_final(Pid, Buffer, Opts);
process_info_extra(Pid, Buffer, Opts) ->
    NewBuffer = [ {async_dist, case try_process_info(Pid, async_dist) of
				   {async_dist, A} -> A;
				   _ -> undefined
			       end}
		, {catchlevel, case try_process_info(Pid, catchlevel) of
				   {catchlevel, C} -> C;
				   _ -> undefined
			       end}
		% , {label, proc_lib:get_label(Pid)}
		, {memory, case try_process_info(Pid, memory) of
			       {memory, M} -> M;
			       _ -> -1
			   end}
		, {monitors, case try_process_info(Pid, monitored_by) of
				 {monitored_by, M} -> length(M);
				 _ -> -1
			     end}
		, {parent, case try_process_info(Pid, parent) of
			       {parent, Parent} when is_pid(Parent) -> pid_to_binary(Parent);
			       _ -> undefined
			   end}
		| Buffer ],
    process_info_final(Pid, NewBuffer, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% wrapper around erlang:process_info/2
%%--------------------------------------------------------------------
try_process_info(Pid, Term) ->
    try
	erlang:process_info(Pid, Term)
    of
	undefined -> undefined;
	Result -> Result
    catch
	_ -> undefined
    end.	     

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
process_info_final(_Pid, Buffer, #{ as_map := true }) ->
    maps:from_list(Buffer);
process_info_final(_Pid, Buffer, _Opts) ->
    Buffer.

%%--------------------------------------------------------------------
%% @doc default prometheus labels used by the collector.
%% @end
%%--------------------------------------------------------------------
prometheus_labels() ->
    [ pid
    , registered_name
    , status
    , parent
    , group_leader
    ].

%%--------------------------------------------------------------------
%% @doc retrieve the correct labels at the right place from extended
%% info.
%% @end
%% --------------------------------------------------------------------
prometheus_labels(Info) when is_list(Info) ->
    prometheus_labels(maps:from_list(Info));
prometheus_labels(#{ status := undefined }) ->
    [];
prometheus_labels(Info) ->
    [ maps:get(X, Info) || X <- prometheus_labels() ].

%%--------------------------------------------------------------------
%% @doc
%% @see prometheus_apply/1
%% @end
%%--------------------------------------------------------------------
prometheus_apply() ->
    prometheus_apply(#{}).

%%--------------------------------------------------------------------
%% @doc apply prometheus functions.
%% @end
%%--------------------------------------------------------------------
prometheus_apply(Opts) ->
    Gauges = prometheus_gauges(Opts),
    [ erlang:apply(M, F, A) || {M, F, A} <- Gauges ].

%%--------------------------------------------------------------------
%% @doc
%% @see prometheus_gauges/1
%% @end
%%--------------------------------------------------------------------
prometheus_gauges() ->
    prometheus_gauges(#{}).

%%--------------------------------------------------------------------
%% @doc returns MFA tuples to update prometheus gauges.
%% @end
%%--------------------------------------------------------------------
prometheus_gauges(Opts) ->
    Filter = maps:get(filter, Opts, fun(_) -> true end),
    InfosRaw = processes_info(#{ as_map => true }),
    Infos = lists:filter(Filter, InfosRaw),
    lists:flatten(
      [ prometheus_gauges(N, Infos, K, Opts) || 
	  {N, K} <- [ {erlang_custom_process_memory, memory}
		    , {erlang_custom_process_stack_size, stack_size}
		    , {erlang_custom_process_message_queue, message_queue_len}
		    , {erlang_custom_process_reductions, reductions}
		    , {erlang_custom_process_heap_size, heap_size}
		    % , {erlang_custom_process_links, links}
		    % , {erlang_custom_monitors, monitors}
		    ]
      ]).
    
%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
prometheus_gauges(Name, Infos, ValueKey, Opts) ->
    prometheus_gauges(Name, Infos, ValueKey, [], Opts).
prometheus_gauges(_Name, [], _ValueKey, Buffer, _Opts) ->
    Buffer;
prometheus_gauges(Name, [Info|Rest], ValueKey, Buffer, Opts) ->
    NewBuffer = [ prometheus_gauge(Name, Info, ValueKey, Opts)
		| Buffer
		],
    prometheus_gauges(Name, Rest, ValueKey, NewBuffer, Opts).

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
prometheus_gauge(_Name, #{ status := undefined }, _ValueKey, _Opts) ->
    {error, undefined};
prometheus_gauge(Name, Info, ValueKey, Opts) ->
    Registry = maps:get(registry, Opts, default),
    Labels = prometheus_labels(Info),
    Value = maps:get(ValueKey, Info),
    {prometheus_gauge, set, [Registry, Name, Labels, Value]}.

%%--------------------------------------------------------------------
%% @doc init prometheus application with custom metrics name.
%% @end
%%--------------------------------------------------------------------
prometheus_init() ->
    prometheus_init(#{}).

prometheus_init(Opts) ->
    Registry = maps:get(registry, Opts, default),
    Gauges =
	[ [{name, erlang_custom_process_memory}
	  ,{labels, prometheus_labels()}
	  ,{help, "extra process memory metrics"}
	  ,{registry, Registry}
	  ]
	, [{name, erlang_custom_process_stack_size}
	  ,{labels, prometheus_labels()}
	  ,{help, "extra process stack size metrics"}
	  ,{registry, Registry}
	  ]
	, [{name, erlang_custom_process_message_queue}
	  ,{labels, prometheus_labels()}
	  ,{help, "extra process message queue metrics"}
	  ,{registry, Registry}
	  ]
	, [{name , erlang_custom_process_reductions}
	  ,{labels , prometheus_labels()}
	  ,{help , "extra process reductions metrics"}
	  ,{registry, Registry}
	  ]
	, [{name , erlang_custom_process_heap_size}
	  ,{labels , prometheus_labels()}
	  ,{help , "extra process heap size metrics"}
	  ,{registry, Registry}
	  ]
	% , [{ name , erlang_custom_process_links}
	%   ,{labels , prometheus_labels()}
	%   ,{help , "extra process links metrics"}
	%   ]
	% , [{ name , erlang_custom_monitors}
	%   ,{labels , prometheus_labels()}
	%   ,{help , "extra process monitors metrics"}
	%  ]
	],
    [ erlang:apply(prometheus_gauge, new, [A]) ||
	A <- Gauges
    ].

%%--------------------------------------------------------------------
%% @doc deregister custom prometheus metrics.
%% @end
%%--------------------------------------------------------------------
prometheus_deregister() ->
    prometheus_deregister(#{}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
prometheus_deregister(Opts) ->
    Registry = maps:get(registry, Opts, default),
    [ prometheus_gauge:deregister(Registry, Name) || 
	Name <-[ erlang_custom_process_memory
	       , erlang_custom_process_stack_size
	       , erlang_custom_process_message_queue
	       , erlang_custom_process_reductions
	       , erlang_custom_process_heap_size
	       ]
    ].
    
%%--------------------------------------------------------------------
%% @doc start a small linked processes to update prometheus gauges.
%% @see start_link/1
%% @end
%% --------------------------------------------------------------------
start_link() ->
    start_link(#{}).

%%--------------------------------------------------------------------
%% @doc start a small linked processes to update prometheus gauges.
%% @end
%%--------------------------------------------------------------------
start_link(Opts) ->
    {ok, spawn_link(fun() -> prometheus_process_init(Opts) end)}.

%%--------------------------------------------------------------------
%% @doc stop collector process.
%% @end
%%--------------------------------------------------------------------
stop() ->
    ?MODULE ! stop.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
prometheus_process_init(Opts) ->
    Interval = maps:get(interval, Opts, 60_000),
    erlang:register(?MODULE, self()),
    erlang:process_flag(trap_exit, true),
    _ = prometheus_init(Opts),
    _ = prometheus_apply(Opts),
    {ok, Ref} = timer:send_interval(Interval, tick),
    prometheus_process_loop(Opts, Ref).

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
prometheus_process_loop(Opts, Ref) ->
    Interval = maps:get(interval, Opts, 60_000),
    IntervalDelay = Interval*3,
    receive
	tick ->
	    prometheus_process_action(Opts),
	    prometheus_process_loop(Opts, Ref);
	_ ->
	    timer:cancel(Ref)
    after
	IntervalDelay ->
	    timer:cancel(Ref)
    end.

prometheus_process_action(Opts = #{ registry := Registry })
  when Registry =/= default ->
    _ = prometheus_deregister(Opts),
    _ = prometheus_init(Opts),
    _ = prometheus_apply(Opts),
    Prom = prometheus_text_format:format(Registry),
    file:write_file("/tmp/collector.prom", Prom);
prometheus_process_action(Opts) ->
    _ = prometheus_deregister(Opts),
    _ = prometheus_init(Opts),
    _ = prometheus_apply(Opts).
	
    
