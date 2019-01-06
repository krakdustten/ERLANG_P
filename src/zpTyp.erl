-module(zpTyp).
-export([create/0, init/0]).

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(zpTyp_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, WireInst_Pid], ReplyFn} ->
			{ok, ok} = msg:set_ack(WireInst_Pid, set_parent, ResInst_Pid),
			ReplyFn(#{resInst => ResInst_Pid, wireInst => WireInst_Pid}), 
			loop();
		{get_VI, {_State, {_Ref, RealSim}, _CIndexFrom}, ReplyFn} ->
			if
			RealSim == real ->
				ReplyFn({nan, nan});
			true ->
				{_,{Hour,Min,_}} = erlang:localtime(),
				Time = Hour + Min / 60.0,
				if
				(Time > 7) and (Time < 21) ->
					RelativePower = 0.5 - (math:cos((Time - 7) * (math:pi() / 7)) / 2);
				true->
					RelativePower = 0
				end,
				TruePower = RelativePower * 20, %W of the solar panel
				if
				RelativePower < 0.2 ->
					Volt = RelativePower * 140,
					ReplyFn({Volt, TruePower / Volt});
				true ->
					ReplyFn({28, TruePower / 28})
				end
			end,
			loop();
		{tick, {State, _DMsTime}, ReplyFn} ->
			ReplyFn(State),
			loop()
	end. 
