-module(meterInst).
-export([create/3, init/3, set_measured/2]).


create(Host, MeterTyp_Pid, WireInst_Pid) -> {ok, spawn(?MODULE, init, [Host, MeterTyp_Pid, WireInst_Pid])}.

init(Host, MeterTyp_Pid, WireInst_Pid) -> 
	{ok, State} = resource_type:get_initial_state(MeterTyp_Pid, self(), WireInst_Pid),
	
	wireInst:set_parent(WireInst_Pid, self()),
	survivor:entry({ meterIInst_created, State }),
	loop(Host, State, MeterTyp_Pid, WireInst_Pid).
	
set_measured(MeterInst, Measured)->
	{ok, ok} = msg:set_ack(MeterInst, set_measured, Measured).

loop(Host, State, MeterTyp_Pid, WireInst_Pid) -> 
	receive
		{get_type, ReplyFn} -> 
			ReplyFn(MeterTyp_Pid),
			loop(Host, State, MeterTyp_Pid, WireInst_Pid);
		{get_VI, {Ref, CIndexFrom}, ReplyFn} ->
			{ok, {V1, I1}} = msg:get(MeterTyp_Pid, get_VI, {State, Ref, CIndexFrom}),
			ReplyFn({V1, I1}),
			loop(Host, State, MeterTyp_Pid, WireInst_Pid);
		{set_measured, Measured, ReplyFn} ->
			{ok, NewState} = msg:set_ack(MeterTyp_Pid, set_measured, {State, Measured}),
			ReplyFn(ok),
			loop(Host, NewState, MeterTyp_Pid, WireInst_Pid);
		{tick, DMsTime, ReplyFn} ->
			{ok, NewState} = msg:get(MeterTyp_Pid, tick, {State, DMsTime}),
			ReplyFn(ok),
			loop(Host, NewState, MeterTyp_Pid, WireInst_Pid);
		OtherMessage ->
			WireInst_Pid ! OtherMessage,
			loop(Host, State, MeterTyp_Pid, WireInst_Pid)
	end.
	
