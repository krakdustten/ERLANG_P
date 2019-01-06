-module(ccInst).
-export([create/3, init/3]).


create(Host, CcTyp_Pid, WireInst_Pid) -> {ok, spawn(?MODULE, init, [Host, CcTyp_Pid, WireInst_Pid])}.

init(Host, CcTyp_Pid, WireInst_Pid) -> 
	{ok, State} = resource_type:get_initial_state(CcTyp_Pid, self(), WireInst_Pid),
	
	wireInst:set_parent(WireInst_Pid, self()),
	survivor:entry({ ccInst_created, State }),
	loop(Host, State, CcTyp_Pid, WireInst_Pid).

loop(Host, State, CcTyp_Pid, WireInst_Pid) -> 
	receive
		{get_type, ReplyFn} -> 
			ReplyFn(CcTyp_Pid),
			loop(Host, State, CcTyp_Pid, WireInst_Pid);
		{get_VI, {Ref, CIndexFrom}, ReplyFn} ->
			{ok, {V1, I1}} = msg:get(CcTyp_Pid, get_VI, {State, Ref, CIndexFrom}),
			ReplyFn({V1, I1}),
			loop(Host, State, CcTyp_Pid, WireInst_Pid);
		{get_VI_function, ReplyFn} ->
			{ok, VIFunc} = msg:get(CcTyp_Pid, get_VI_function, State),
			ReplyFn(VIFunc),
			loop(Host, State, CcTyp_Pid, WireInst_Pid);
		{tick, DMsTime, ReplyFn} ->
			{ok, NewState} = msg:get(CcTyp_Pid, tick, {State, DMsTime}),
			ReplyFn(ok),
			loop(Host, NewState, CcTyp_Pid, WireInst_Pid);
		OtherMessage ->
			WireInst_Pid ! OtherMessage,
			loop(Host, State, CcTyp_Pid, WireInst_Pid)
	end.
	
