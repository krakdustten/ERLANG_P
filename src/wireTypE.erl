-module(wireTypE).
-export([create/0, init/0]). % More to be added later. 

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(wireTypE_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, TypeOptions], ReplyFn} ->
			Location = location:create(ResInst_Pid, emptySpace),
			C1 = connector:create(ResInst_Pid, simpleEnd),
			ReplyFn(#{resInst => ResInst_Pid, chambers => [Location], 
					cList => [C1], typeOptions => TypeOptions,
					parent => null}), 
			loop();
		{connections_list, State , ReplyFn} -> 
			#{cList := C_List} = State, ReplyFn(C_List), 
			loop();
		{locations_list, State, ReplyFn} -> 
			#{chambers := L_List} = State, ReplyFn(L_List),
			loop();
		{set_parent, {State, ParentInst}, ReplyFn} ->
			ReplyFn(State#{parent := ParentInst}),
			loop();
		{get_parent, State, ReplyFn} ->
			#{parent := Parent} = State,
			ReplyFn(Parent),
			loop();
		{get_VI, {State, _Ref, _Asker, ReplyFn2}, ReplyFn} -> 
			ReplyFn2({0, 0}),
			ReplyFn(State),
			loop();
		{get_local_VI, _State, ReplyFn} -> 
			ReplyFn({0, 0}),
			loop()
	end. 
