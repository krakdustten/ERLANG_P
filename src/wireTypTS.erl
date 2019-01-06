-module(wireTypTS).
-export([create/0, init/0]). % More to be added later. 

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(wireTypTS_created),
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, TypeOptions], ReplyFn} ->
			Location = location:create(ResInst_Pid, emptySpace),
			CIN = connector:create(ResInst_Pid, simpleWireTS),
			COUT = connector:create(ResInst_Pid, simpleWireTS),
			CREF = connector:create(ResInst_Pid, simpleWireTS),
			ReplyFn(#{resInst => ResInst_Pid, chambers => [Location], 
					cList => [CIN, COUT, CREF], typeOptions => TypeOptions,
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
			#{cList := C_List, refStore := RefStore, parent := Parent, resInst := Self} = State,
			if
			is_atom(Parent) ->
				NewRefStore = funcVI:autoCalculateVI({0, 0}, RefStore, Ref, C_List, Asker, Self, ReplyFn2);
			true ->
				NewRefStore = funcVI:autoCalculateVIFunc(RefStore, Ref, C_List, Asker, Self, ReplyFn2, Parent)
			end,
			NewState = State#{refStore := NewRefStore},
			ReplyFn(NewState),
			loop();
		{get_local_VI, _State, ReplyFn} -> 
			ReplyFn({0, 0}),
			loop()
	end. 
