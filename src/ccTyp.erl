-module(ccTyp).
-export([create/0, init/0]).

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(ccTyp_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, WireInst_Pid], ReplyFn} ->
			{ok, ok} = msg:set_ack(WireInst_Pid, set_parent, ResInst_Pid),
			ReplyFn(#{resInst => ResInst_Pid, wireInst => WireInst_Pid}), 
			loop();
		{get_VI, {_State, {_Ref, _RealSim}, _CIndexFrom}, ReplyFn} ->
			ReplyFn({nan, nan}),
			loop();
		{get_VI_function, _State, ReplyFn} ->
			ReplyFn(fun(VI, IO) -> viFunc(VI, IO) end),
			loop();
		{tick, {State, _DMsTime}, ReplyFn} ->
			ReplyFn(State),
			loop()
	end. 

viFunc({VIN, IIN}, out) ->
	if
	VIN >= 12 ->
		{12, IIN * VIN / 12};
	true ->
		{VIN, IIN}
	end;
viFunc({VOUT, IOUT}, in) ->
	{VOUT + 1, IOUT -  0.01}.