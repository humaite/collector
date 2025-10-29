%%%===================================================================
%%% @doc
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
	 prometheus_reset/0,
	 prometheus_apply/0,
	 prometheus_gauges/0,
	 start_link/0,
	 stop/0
]).
-compile({no_auto_import,[process_info/2]}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
registered_processes() ->
    [ {X, erlang:whereis(X)} || X <-  erlang:registered() ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
processes_info() ->
    processes_info(#{}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
processes_info(Opts) ->
    processes_info(erlang:processes(), Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
processes_info(Pids, Opts) ->
    lists:foldr(
      fun(P, A) -> [process_info(P,Opts)|A] end,
      [],
      Pids).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
process_info(Pid) ->
    process_info(Pid, #{}).

%%--------------------------------------------------------------------
%%
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
%%
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
%%
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
%%
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
%%
%%--------------------------------------------------------------------
process_info_final(_Pid, Buffer, #{ as_map := true }) ->
    maps:from_list(Buffer);
process_info_final(_Pid, Buffer, _Opts) ->
    Buffer.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
prometheus_labels() ->
    [ pid
    , registered_name
    , status
    , parent
    , group_leader
    ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
prometheus_labels(Info) when is_list(Info) ->
    prometheus_labels(maps:from_list(Info));
prometheus_labels(#{ status := undefined }) ->
    [];
prometheus_labels(Info) ->
    [ maps:get(X, Info) || X <- prometheus_labels() ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
prometheus_apply() ->
    Gauges = prometheus_gauges(),
    _ = prometheus_reset(),
    [ erlang:apply(M, F, A) || {M, F, A} <- Gauges ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
prometheus_gauges() ->
    Infos = processes_info(#{ as_map => true }),
    lists:flatten(
      [ prometheus_gauges(N, Infos, K) || 
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
%%
%%--------------------------------------------------------------------
prometheus_gauges(Name, Infos, ValueKey) ->
    prometheus_gauges(Name, Infos, ValueKey, []).
prometheus_gauges(_Name, [], _ValueKey, Buffer) ->
    Buffer;
prometheus_gauges(Name, [Info|Rest], ValueKey, Buffer) ->
    NewBuffer = [ prometheus_gauge(Name, Info, ValueKey)
		| Buffer
		],
    prometheus_gauges(Name, Rest, ValueKey, NewBuffer).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
prometheus_gauge(_Name, #{ status := undefined }, _ValueKey) ->
    {error, undefined};
prometheus_gauge(Name, Info, ValueKey) ->
    Labels = prometheus_labels(Info),
    Value = maps:get(ValueKey, Info),
    {prometheus_gauge, set, [Name, Labels, Value]}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
prometheus_init() ->
    Gauges =
	[ [{name, erlang_custom_process_memory}
	  ,{labels, prometheus_labels()}
	  ,{help, "extra process memory metrics"}
	  ]
	, [{name, erlang_custom_process_stack_size}
	  ,{labels, prometheus_labels()}
	  ,{help, "extra process stack size metrics"}
	  ]
	, [{name, erlang_custom_process_message_queue}
	  ,{labels, prometheus_labels()}
	  ,{help, "extra process message queue metrics"}
	  ]
	, [{name , erlang_custom_process_reductions}
	  ,{labels , prometheus_labels()}
	  ,{help , "extra process reductions metrics"}
	  ]
	, [{name , erlang_custom_process_heap_size}
	  ,{labels , prometheus_labels()}
	  ,{help , "extra process heap size metrics"}
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
%%
%%--------------------------------------------------------------------
prometheus_reset() ->
    Labels = prometheus_labels(),
    prometheus_gauge:reset(erlang_custom_process_memory, Labels),
    prometheus_gauge:reset(erlang_custom_process_stack_size, Labels),
    prometheus_gauge:reset(erlang_custom_process_message_queue, Labels),
    prometheus_gauge:reset(erlang_custom_process_reductions, Labels),
    prometheus_gauge:reset(erlang_custom_process_heap_size, Labels).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link() ->
    {ok, spawn_link(fun prometheus_process_init/0)}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
stop() ->
    ?MODULE ! stop.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
prometheus_process_init() ->
    erlang:register(?MODULE, self()),
    erlang:process_flag(trap_exit, true),
    _ = prometheus_apply(),
    {ok, Ref} = timer:send_interval(60_000, tick),
    prometheus_process_loop(Ref).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
prometheus_process_loop(Ref) ->
    receive
	tick ->
	    _ = prometheus_apply(),
	    prometheus_process_loop(Ref);
	_ ->
	    timer:cancel(Ref)
    end.
	    
