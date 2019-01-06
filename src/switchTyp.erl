-module(switchTyp).
-export([create/0, init/0]).

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(switchTyp_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, [WireInst_Pid, RWFunc]], ReplyFn} ->
			{ok, ok} = msg:set_ack(WireInst_Pid, set_parent, ResInst_Pid),
			ReplyFn(#{resInst => ResInst_Pid, wireInst => WireInst_Pid,
			realWorldFunc => RWFunc, switchOnOff => 0}), 
			RWFunc(0),
			loop();
		{get_VI, {State, {_Ref, _RealSim}, _CIndexFrom}, ReplyFn} ->
			#{switchOnOff := SwitchOnOff} = State,
			if
			SwitchOnOff >= 1 ->
				ReplyFn({nan, nan});
			true ->
				ReplyFn({zero, zero})
			end,
			loop();
		{set_on_off, {State, OnOff}, ReplyFn} ->
			#{realWorldFunc := RWFunc} = State,
			NewState = State#{switchOnOff := OnOff},
			ReplyFn(NewState),
			RWFunc(OnOff),
			loop();
		{tick, {State, _DMsTime}, ReplyFn} ->
			ReplyFn(State),
			loop()
	end. 
