-module(switchInst).
-export([create/4, init/4, set_on_off/2]).


create(Host, SwitchTyp_Pid, WireInst_Pid, RWFunc) -> {ok, spawn(?MODULE, init, [Host, SwitchTyp_Pid, WireInst_Pid, RWFunc])}.

init(Host, SwitchTyp_Pid, WireInst_Pid, RWFunc) -> 
	{ok, State} = resource_type:get_initial_state(SwitchTyp_Pid, self(), [WireInst_Pid, RWFunc]),
	
	wireInst:set_parent(WireInst_Pid, self()),
	survivor:entry({ switchInst_created, State }),
	loop(Host, State, SwitchTyp_Pid, WireInst_Pid).
	
set_on_off(MeterInst, OnOff)->
	{ok, ok} = msg:set_ack(MeterInst, set_on_off, OnOff).

loop(Host, State, SwitchTyp_Pid, WireInst_Pid) -> 
	receive
		{get_type, ReplyFn} -> 
			ReplyFn(SwitchTyp_Pid),
			loop(Host, State, SwitchTyp_Pid, WireInst_Pid);
		{get_VI, {Ref, CIndexFrom}, ReplyFn} ->
			{ok, {V1, I1}} = msg:get(SwitchTyp_Pid, get_VI, {State, Ref, CIndexFrom}),
			ReplyFn({V1, I1}),
			loop(Host, State, SwitchTyp_Pid, WireInst_Pid);
		{set_on_off, OnOff, ReplyFn} ->
			{ok, NewState} = msg:get(SwitchTyp_Pid, set_on_off, {State, OnOff}),
			ReplyFn(ok),
			loop(Host, NewState, SwitchTyp_Pid, WireInst_Pid);
		{tick, DMsTime, ReplyFn} ->
			{ok, NewState} = msg:get(SwitchTyp_Pid, tick, {State, DMsTime}),
			ReplyFn(ok),
			loop(Host, NewState, SwitchTyp_Pid, WireInst_Pid);
		OtherMessage ->
			WireInst_Pid ! OtherMessage,
			loop(Host, State, SwitchTyp_Pid, WireInst_Pid)
	end.
	
