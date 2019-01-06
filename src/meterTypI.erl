-module(meterTypI).
-export([create/0, init/0]).

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(meterTypI_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, WireInst_Pid], ReplyFn} ->
			{ok, ok} = msg:set_ack(WireInst_Pid, set_parent, ResInst_Pid),
			ReplyFn(#{resInst => ResInst_Pid, wireInst => WireInst_Pid,
			current => 0}), 
			loop();
		{get_VI, {State, {_Ref, RealSim}, CIndexFrom}, ReplyFn} ->
			#{current := I} = State,
			if
			RealSim == sim ->
				ReplyFn({nan, nan});
			CIndexFrom == 1 ->
				ReplyFn({0, I});
			true ->
				ReplyFn({0, -I})
			end,
			loop();
		{set_measured, {State, Measured}, ReplyFn} ->
			NewState = State#{current := Measured},
			ReplyFn(NewState),
			loop();
		{tick, {State, _DMsTime}, ReplyFn} ->
			ReplyFn(State),
			loop()
	end. 
