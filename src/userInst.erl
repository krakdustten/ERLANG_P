-module(userInst).
-export([create/3, init/3]).


create(Host, UserTyp_Pid, WireInst_Pid) -> {ok, spawn(?MODULE, init, [Host, UserTyp_Pid, WireInst_Pid])}.

init(Host, UserTyp_Pid, WireInst_Pid) -> 
	{ok, State} = resource_type:get_initial_state(UserTyp_Pid, self(), WireInst_Pid),
	
	wireInst:set_parent(WireInst_Pid, self()),
	survivor:entry({ userInst_created, State }),
	loop(Host, State, UserTyp_Pid, WireInst_Pid).

loop(Host, State, UserTyp_Pid, WireInst_Pid) -> 
	receive
		{get_type, ReplyFn} -> 
			ReplyFn(UserTyp_Pid),
			loop(Host, State, UserTyp_Pid, WireInst_Pid);
		{get_VI, {Ref, CIndexFrom}, ReplyFn} ->
			{ok, {V1, I1}} = msg:get(UserTyp_Pid, get_VI, {State, Ref, CIndexFrom}),
			ReplyFn({V1, I1}),
			loop(Host, State, UserTyp_Pid, WireInst_Pid);
		{tick, DMsTime, ReplyFn} ->
			{ok, NewState} = msg:get(UserTyp_Pid, tick, {State, DMsTime}),
			ReplyFn(ok),
			loop(Host, NewState, UserTyp_Pid, WireInst_Pid);
		OtherMessage ->
			WireInst_Pid ! OtherMessage,
			loop(Host, State, UserTyp_Pid, WireInst_Pid)
	end.
	
