-module(funcVI).
-export([autoCalculateVI/7, autoCalculateVIFunc/7, ask_other_wires_int/6, ask_other_wires_func_int/6]).

autoCalculateVI({V, I}, RefStore, {Ref, RealSim}, C_List, Asker, Self, ReplyFn) ->
	{HadBefore, NewRefStore} = handleRefence(RefStore, Ref),
	if	
		HadBefore ->
			ReplyFn({0, 0});
		Asker == Self ->
			ReplyFn({0, 0});
		true ->
			ask_other_wires(C_List, {Ref, RealSim}, ReplyFn, {V, I}, Asker, Self)
	end,
	NewRefStore.
	
autoCalculateVIFunc(RefStore, {Ref, RealSim}, C_List, Asker, Self, ReplyFn, Parent) ->
	{HadBefore, NewRefStore} = handleRefence(RefStore, Ref),
	if	
		HadBefore ->
			ReplyFn({0, 0});
		Asker == Self ->
			ReplyFn({0, 0});
		true ->
			ask_other_wires_func(C_List, {Ref, RealSim}, Asker, Self, ReplyFn, Parent)
	end,
	NewRefStore.

handleRefence(RefStore1, Ref) ->
	{RefStore, HadBefore} = outDateRemAndFinder(RefStore1, Ref),
	if
		HadBefore ->
			RefStore2 = RefStore;
		true->
			RefStore2 = [{Ref, erlang:monotonic_time(milli_seconds)} | RefStore]
	end,
	{HadBefore, RefStore2}.

outDateRemAndFinder(RefStore, RefS)->
	outDateRemAndFinder(RefStore, [], RefS, false).
outDateRemAndFinder([], RefOut, _RefS, Found)->
	{RefOut, Found};
outDateRemAndFinder([{Ref, Time} | T] , RefOut, RefS, Found)->
	CheckTime = erlang:monotonic_time(milli_seconds) - 5000,
	if
		Time > CheckTime ->
			if
				RefS == Ref -> 
					outDateRemAndFinder(T, [{Ref, Time} | RefOut], RefS, true);
				true ->
					outDateRemAndFinder(T, [{Ref, Time} | RefOut], RefS, Found)
			end;
		true ->
			outDateRemAndFinder(T, RefOut, RefS, Found)
	end.
	
	
ask_other_wires(C_List, Ref, ReplyFn, VI, Asker, Self)->
	spawn_link(?MODULE, ask_other_wires_int, [C_List, Ref, ReplyFn, VI, Asker, Self]).
		
ask_other_wires_int(C, Ref, ReplyFn, VI, Asker, Self) ->	
	{VIList, SenderC} = convertCtoVI(C, [VI], Ref, Asker, Self),
	VIc = calculateVI(VIList, SenderC),
	ReplyFn(VIc),
	ok.
	
ask_other_wires_func(C_List, Ref, Asker, Self, ReplyFn, Parent)->
	spawn_link(?MODULE, ask_other_wires_func_int, [C_List, Ref, Asker, Self, ReplyFn, Parent]).
	
ask_other_wires_func_int(C, Ref, Asker, Self, ReplyFn, Parent) ->	
	{VIList, SenderC} = convertCtoVI(C, [], Ref, Asker, Self),
	{ok, VIFunc} = msg:get(Parent, get_VI_function),
	VIc = calculateVIFunc(VIList, SenderC, VIFunc),
	ReplyFn(VIc),
	ok.
	
convertCtoVI(C, VI, Ref, Asker, Self) ->
	convertCtoVI(C, VI, Ref, Asker, Self, 1, 0).
convertCtoVI([], VI, _Ref, _Asker, _Self, _Count, SenderC)->
	{VI, SenderC};
convertCtoVI([C | T], VI, Ref, Asker, Self, Count, SenderC)->
	{ok, CA} = connector:get_connected(C),
	{ok, Res} = connector:get_ResInst(CA),
	{ok, Par} = msg:get(Res, get_parent),
	if
	(Asker == Res) or (Asker == Par) ->
		convertCtoVI(T, [{0, 0} | VI], Ref, Asker, Self, Count + 1, Count);
	true ->
		if
		is_atom(Par) ->
			{ok, VI1} = msg:get(Res, get_VI, {Ref, Self});
		true ->
			ParCIndex = get_device_C_index(Res, CA),
			{ok, {V1, I1}} = msg:get(Par, get_VI, {Ref, ParCIndex}),
			if
			is_atom(V1) or is_atom(I1) ->
				if
				(V1 == nan) or (I1 == nan) ->
					{ok, {V2, I2}} = msg:get(Res, get_VI, {Ref, Self}),
					if
						V1 == nan ->
							V = V2;
						true -> 
							V = 0
					end,
					if
						I1 == nan ->
							I = I2;
						true -> 
							I = 0
					end,
					VI1 = {V, I};
				true ->
					VI1 = {0, 0}
				end;
			true ->
				{ok, {V2, _}} = msg:get(Res, get_VI, {Ref, Self}),
				VI1 = {V1 + V2, I1}
			end
		end,
		convertCtoVI(T, [VI1 | VI], Ref, Asker, Self, Count + 1, SenderC)
	end.
	
get_device_C_index(Res, C) ->
	{ok, C_list} = resource_instance:list_connectors(Res),
	get_C_index(C_list, C, 1, 0).

get_C_index([], _CC, _, Index) ->
	Index;
get_C_index([C | T], CC, Count, Index) ->
	if
	C == CC ->
		get_C_index(T, CC, Count + 1, Count);
	true ->
		get_C_index(T, CC, Count + 1, Index)
	end.
	

calculateVI([{V1, I1}, {V0, I0}], _SenderC)-> %type E
	{V0 + V1, I0 + I1};	
	
calculateVI([{V2, I2}, {V1, I1}, {V0, I0}], _SenderC)-> %type S
	{V0 + V1 + V2, maxA([I0, I1, I2])};	
	
calculateVI([{V3, I3}, {V2, I2}, {V1, I1}, {_, _}], _SenderC)-> %type T
	I = I1 + I2 + I3,
	{maxA([V1, V2, V3]), I}.

calculateVIFunc([{_VREF, _IREF}, {VOUT, IOUT}, {VIN, IIN}], SenderC, VIFunc)-> %type TS
	if
		SenderC == 2 -> %OUT asked
			{V, I} = VIFunc({VIN, IIN}, out);
		SenderC == 1 -> %IN asked
			{V, I} = VIFunc({VOUT, IOUT}, in);
		SenderC == 3 -> %REF asked
			{V, I} = {0, 0};
		true -> 
			{V, I} = {0, 0}
	end,
	{V, I}.
	
maxA([D | T])->
	maxA(T, D).
maxA([], Max)->
	Max;
maxA([D | T], Max)->
	if
		abs(D) > abs(Max) ->
			maxA(T, D);
		true ->
			maxA(T, Max)
	end.

