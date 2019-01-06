-module(userTyp).
-export([create/0, init/0]).

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(userTyp_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, WireInst_Pid], ReplyFn} ->
			{ok, ok} = msg:set_ack(WireInst_Pid, set_parent, ResInst_Pid),
			ReplyFn(#{resInst => ResInst_Pid, wireInst => WireInst_Pid}), 
			loop();
		{get_VI, {State, {_Ref, RealSim}, _CIndexFrom}, ReplyFn} ->
			#{wireInst := WireInst_Pid} = State,
			if
			RealSim == real ->
				ReplyFn({nan, nan});
			true ->
				{V, _} = wireInst:get_simVI(WireInst_Pid),
				if
				V > 6 ->
					ReplyFn({0, 0.2});
				true -> 
					ReplyFn({0, 0})
				end
			end,
			loop();
		{tick, {State, _DMsTime}, ReplyFn} ->
			ReplyFn(State),
			loop()
	end. 
