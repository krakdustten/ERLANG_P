-module(batInst).
-export([create/3, init/3]).


create(Host, BatTyp_Pid, WireInst_Pid) -> {ok, spawn(?MODULE, init, [Host, BatTyp_Pid, WireInst_Pid])}.

init(Host, BatTyp_Pid, WireInst_Pid) -> 
	{ok, State} = resource_type:get_initial_state(BatTyp_Pid, self(), WireInst_Pid),
	
	wireInst:set_parent(WireInst_Pid, self()),
	survivor:entry({ batInst_created, State }),
	loop(Host, State, BatTyp_Pid, WireInst_Pid).

loop(Host, State, BatTyp_Pid, WireInst_Pid) -> 
	receive
		{get_type, ReplyFn} -> 
			ReplyFn(BatTyp_Pid),
			loop(Host, State, BatTyp_Pid, WireInst_Pid);
		{get_VI, {Ref, CIndexFrom}, ReplyFn} ->
			{ok, {V1, I1}} = msg:get(BatTyp_Pid, get_VI, {State, Ref, CIndexFrom}),
			ReplyFn({V1, I1}),
			loop(Host, State, BatTyp_Pid, WireInst_Pid);
		{tick, MsTime, ReplyFn} ->
			{ok, NewState} = msg:get(BatTyp_Pid, tick, {State, MsTime}),
			ReplyFn(ok),
			loop(Host, NewState, BatTyp_Pid, WireInst_Pid);
		OtherMessage ->
			WireInst_Pid ! OtherMessage,
			loop(Host, State, BatTyp_Pid, WireInst_Pid)
	end.
	
