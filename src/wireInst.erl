-module(wireInst).
-export([create/2, init/2, get_VI/1, get_simVI/1, get_local_VI/1, set_parent/2, get_parent/1]).


create(Host, WireTyp_Pid) -> {ok, spawn(?MODULE, init, [Host, WireTyp_Pid])}.

init(Host, WireTyp_Pid) -> 
	{ok, State} = resource_type:get_initial_state(WireTyp_Pid, self(), []),
	survivor:entry({ wireInst_created, State }),
	loop(Host, State, WireTyp_Pid).
	
get_VI(WireInst_Pid) ->
	{ok, VI} = msg:get(WireInst_Pid, get_VI, {{make_ref(), real}, self()}),
	VI.

get_simVI(WireInst_Pid) ->
	{ok, VI} = msg:get(WireInst_Pid, get_VI, {{make_ref(), sim}, self()}),
	VI.

get_local_VI(WireInst_Pid) ->
	{ok, VI} = msg:get(WireInst_Pid, get_local_VI),
	VI.
	
set_parent(WireInst_Pid, Parent) ->
	{ok, ok} = msg:set_ack(WireInst_Pid, set_parent, Parent).
	
get_parent(WireInst_Pid) ->
	{ok, Parent} = msg:get(WireInst_Pid, get_parent),
	Parent.

loop(Host, State, WireTyp_Pid) -> 
	receive
		{get_connectors, ReplyFn} ->
			{ok,C_List} = resource_type:get_connections_list(WireTyp_Pid, State), 
			ReplyFn(C_List),
			loop(Host, State, WireTyp_Pid);
		{get_locations, ReplyFn} ->
			{ok, List} = resource_type:get_locations_list(WireTyp_Pid, State),
			ReplyFn(List),
			loop(Host, State, WireTyp_Pid);
		{get_type, ReplyFn} -> 
			ReplyFn(WireTyp_Pid),
			loop(Host, State, WireTyp_Pid);
		{get_ops, ReplyFn} ->
			ReplyFn([]),
			loop(Host, State, WireTyp_Pid);
		{set_parent, ParentInst, ReplyFn} ->
			{ok, NewState} = msg:set_ack(WireTyp_Pid, set_parent, {State, ParentInst}),
			ReplyFn(ok),
			loop(Host, NewState, WireTyp_Pid);
		{get_parent, ReplyFn} ->
			{ok, Parent} = msg:get(WireTyp_Pid, get_parent, State),
			ReplyFn(Parent),
			loop(Host, State, WireTyp_Pid);
		{get_VI, {Ref, Asker}, ReplyFn} ->
			{ok, NewState} = msg:set_ack(WireTyp_Pid, get_VI, {State, Ref, Asker, ReplyFn}),
			loop(Host, NewState, WireTyp_Pid);
		{get_local_VI, ReplyFn} ->
			{ok, VI} = msg:set_ack(WireTyp_Pid, get_local_VI, State),
			ReplyFn(VI),
			loop(Host, State, WireTyp_Pid);
		{tick, _DMsTime, ReplyFn} -> %no need for tick in wires
			ReplyFn(ok),
			loop(Host, State, WireTyp_Pid)
	end.
	
