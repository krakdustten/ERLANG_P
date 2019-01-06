-module(meterTypV).
-export([create/0, init/0]).

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(meterTypV_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, WireInst_Pid], ReplyFn} ->
			{ok, ok} = msg:set_ack(WireInst_Pid, set_parent, ResInst_Pid),
			ReplyFn(#{resInst => ResInst_Pid, wireInst => WireInst_Pid,
			voltage => 0}), 
			loop();
		{get_VI, {State, {_Ref, RealSim}, CIndexFrom}, ReplyFn} ->
			#{voltage := V} = State,
			if
			RealSim == sim ->
				ReplyFn({nan, nan});
			CIndexFrom == 1 ->
				ReplyFn({V, 0});
			true ->
				ReplyFn({-V, 0})
			end,
			loop();
		{set_measured, {State, Measured}, ReplyFn} ->
			NewState = State#{voltage := Measured},
			ReplyFn(NewState),
			loop();
		{tick, {State, _DMsTime}, ReplyFn} ->
			ReplyFn(State),
			loop()
	end. 
