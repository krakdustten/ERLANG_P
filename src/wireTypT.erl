-module(wireTypT).
-export([create/0, init/0]). % More to be added later. 

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(wireTypT_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, TypeOptions], ReplyFn} ->
			Location = location:create(ResInst_Pid, emptySpace),
			C1 = connector:create(ResInst_Pid, simpleWireT),
			C2 = connector:create(ResInst_Pid, simpleWireT),
			C3 = connector:create(ResInst_Pid, simpleWireT),
			ReplyFn(#{resInst => ResInst_Pid, chambers => [Location], 
					cList => [C1, C2, C3], typeOptions => TypeOptions,
					parent => null, refStore => []}), 
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
		{get_VI, {State, Ref, Asker, ReplyFn2}, ReplyFn} -> 
			#{cList := C_List, refStore := RefStore, resInst := Self} = State,
			NewRefStore = funcVI:autoCalculateVI({0, 0}, RefStore, Ref, C_List, Asker, Self, ReplyFn2),
			NewState = State#{refStore := NewRefStore},
			ReplyFn(NewState),
			loop();
		{get_local_VI, _State, ReplyFn} -> 
			ReplyFn({0, 0}),
			loop()
	end. 
