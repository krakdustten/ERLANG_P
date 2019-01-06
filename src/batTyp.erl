-module(batTyp).
-export([create/0, init/0]).

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(batTyp_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, WireInst_Pid], ReplyFn} ->
			{ok, ok} = msg:set_ack(WireInst_Pid, set_parent, ResInst_Pid),
			ReplyFn(#{resInst => ResInst_Pid, wireInst => WireInst_Pid,
			charge => 0}), 
			loop();
		{get_VI, {State, {_Ref, RealSim}, _CIndexFrom}, ReplyFn} ->
			#{charge := Charge} = State,
			if
			RealSim == real ->
				ReplyFn({nan, nan});
			Charge > 0 ->
				ReplyFn({12, 0});
			true ->
				ReplyFn({0, 0})
			end,
			loop();
		{tick, {State, DMsTime}, ReplyFn} ->
			#{wireInst := WireInst_Pid, charge := Charge} = State,
			survivor:entry({charge, Charge}),
			{V, I} = wireInst:get_VI(WireInst_Pid),
			if
			(I < 0) -> %Charge
				NewCharge = (V / 12.0) * (-I) * (DMsTime/3600000.0) * 0.85 + Charge;
			(I > 0) and (Charge > 0) -> %Discharge
				NewCharge = -((V / 12.0) * I * (DMsTime/3600000.0)) + Charge;
			true ->
				NewCharge = Charge
			end,
			ReplyFn(State#{charge := NewCharge}),
			loop()
	end. 
