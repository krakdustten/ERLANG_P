-module(digitalTwin).
-export([create/2, get_VI/2, get_simVI/2, set_measured/3, set_on_off/3, tick/2]).
-export([init/2]).


%   +---------+ B          C  +---------+ E          F  +---------+ G          H           K          N          O  +---------+ P          Q  +---------+ R          S  +---------+
%   | ZP      | ##  W_ZP   ## | CC      | ##  W_CC   ## | A1      | ##  W_A1   ##  TBAT    ##  TV1    ##  W_TV1  ## | SW      | ##  W_SW   ##+| A2      |-##  W_A2   ## | US      |
%   | WZP     +-21---------21-+ TCC     +-31---------21-+ WA1     +-21---------21----+-----31----+----31---------21-+ WSW     +-21---------21-+ WA2     +-21---------21-+ WUS     |
%   |         | ##         ## |         | ##         ## |         | ##         ##    |     ##    |    ##         ## |         | ##         ## |         | ##         ## |         |
%   +----+----+               +----+----+               +---------+                  |           |                  +---------+               +---------+               +----+----+
%        |                         |                                              I #2#       L #2#                                                                          |
%       #1#	A                     #2# D                                             #1#         #1#                                                                         #2# T
%       #1#                       #1#                                                | +         | +                                                                        #1#
%        |                         |                                            +----+----+ +----+----+                                                                      |
%        * EZP                     * ECC                                        | BAT     | | V1      |                                                                      * EUS
%                                                                               | WBAT    | | WV1     |
%                                                                               |         | |         |
%                                                                               +----+----+ +----+----+
%                                                                                    | -         | -
%                                                                                 J #2#       M #2#
%                                                                                   #1#         #1#
%                                                                                    |           |
%                                                                                    * EBAT      * EV1

create(Host, RWSwitchFunc) -> {ok, spawn(?MODULE, init, [Host, RWSwitchFunc])}.

get_VI(DT_PID, Name) -> 
	{ok, VI} = msg:get(DT_PID, get_VI, Name),
	VI.
	
get_simVI(DT_PID, Name) ->
	{ok, VI} = msg:get(DT_PID, get_simVI, Name),
	VI.

set_measured(DT_PID, Name, Measured) ->
	msg:get(DT_PID, set_measured, {Name, Measured}).

set_on_off(DT_PID, Name, OnOff) ->
	msg:get(DT_PID, set_on_off, {Name, OnOff}).

tick(DT_PID, DMsTime) ->
	msg:get(DT_PID, tick, DMsTime).

init(Host, RWSwitchFunc)->
	survivor:entry({ digitalTwin_created}),
	{ok, WireTypeSPID} = wireTypS:create(),
	{ok, WireTTypePID} = wireTypT:create(),
	{ok, WireTSTypePID} = wireTypTS:create(),
	{ok, WireETypePID} = wireTypE:create(),
	
	{ok, MeterTypI} = meterTypI:create(),
	{ok, MeterTypV} = meterTypV:create(),
	
	{ok, SwitchTyp} = switchTyp:create(),
	{ok, UserTyp} = userTyp:create(),
	{ok, BatTyp} = batTyp:create(),
	{ok, CcTyp} = ccTyp:create(),
	{ok, ZpTyp} = zpTyp:create(),
	
	TCC 	= createWire(WireTSTypePID),
	TBAT 	= createWire(WireTTypePID),
	TV1 	= createWire(WireTTypePID),
	
	WZP 	= createWire(WireTypeSPID),
	W_ZP 	= createWire(WireTypeSPID),
	W_CC 	= createWire(WireTypeSPID),
	WA1 	= createWire(WireTypeSPID),
	W_A1 	= createWire(WireTypeSPID),
	WBAT 	= createWire(WireTypeSPID),
	WV1 	= createWire(WireTypeSPID),
	W_TV1 	= createWire(WireTypeSPID),
	WSW 	= createWire(WireTypeSPID),
	W_SW 	= createWire(WireTypeSPID),
	WA2 	= createWire(WireTypeSPID),
	W_A2 	= createWire(WireTypeSPID),
	WUS 	= createWire(WireTypeSPID),
	
	EZP 	= createWire(WireETypePID),
	ECC 	= createWire(WireETypePID),
	EBAT 	= createWire(WireETypePID),
	EV1 	= createWire(WireETypePID),
	EUS 	= createWire(WireETypePID),
	
	connectWire(EZP  , WZP  , 1, 1),%A
	connectWire(WZP  , W_ZP , 2, 1),%B
	connectWire(W_ZP , TCC  , 2, 1),%C
	connectWire(ECC  , TCC  , 1, 3),%D
	connectWire(TCC  , W_CC , 2, 1),%E
	connectWire(W_CC , WA1  , 2, 1),%F
	connectWire(WA1  , W_A1 , 2, 1),%G
	connectWire(W_A1 , TBAT , 2, 1),%H
	connectWire(TBAT , WBAT , 2, 1),%I
	connectWire(EBAT , WBAT , 1, 2),%J
	connectWire(TBAT , TV1  , 3, 1),%K
	connectWire(TV1  , WV1  , 2, 1),%L
	connectWire(EV1  , WV1  , 1, 2),%M
	connectWire(TV1  , W_TV1, 3, 1),%N
	connectWire(W_TV1, WSW  , 2, 1),%O
	connectWire(WSW  , W_SW , 2, 1),%P
	connectWire(W_SW , WA2  , 2, 1),%Q
	connectWire(WA2  , W_A2 , 2, 1),%R
	connectWire(W_A2 , WUS  , 2, 1),%S
	connectWire(WUS  , EUS  , 2, 1),%T
	
	{ok, SW1} = switchInst:create(self(), SwitchTyp, WSW, RWSwitchFunc),
	{ok, US1} = userInst:create(self(), UserTyp, WUS),
	{ok, BAT1} = batInst:create(self(), BatTyp, WBAT),
	{ok, CC1} = ccInst:create(self(), CcTyp, TCC),
	{ok, ZP1} = zpInst:create(self(), ZpTyp, WZP),
	
	{ok, MA1} = meterInst:create(self(), MeterTypI, WA1),
	{ok, MA2} = meterInst:create(self(), MeterTypI, WA2),
	{ok, MV1} = meterInst:create(self(), MeterTypV, WV1),
	
	Wires = {[TCC, TBAT, TV1], [WZP, W_ZP, W_CC, WA1, W_A1, WBAT, WV1, W_TV1, WSW, W_SW, WA2, W_A2, WUS], [EZP, ECC, EBAT, EV1, EUS]},
	Devices = {[SW1, US1, BAT1, CC1, ZP1], [MA1, MA2, MV1]},
	loop(Host, Wires, Devices).
	
loop(Host, Wires, Devices) -> 
	%{Twires, Wwires, Ewires} = Wires,
	%[TCC, TBAT, TV1] = Twires, 
	%[WZP, W_ZP, W_CC, WA1, W_A1, WBAT, WV1, W_TV1, WSW, W_SW, WA2, W_A2, WUS] = Wwires, 
	%[EZP, ECC, EBAT, EV1, EUS] = Ewires,
	{[SW1, US1, BAT1, CC1, ZP1], [MA1, MA2, MV1]} = Devices,
	
	receive
		{get_VI, Name, ReplyFn} -> 
			{ok, WPID} = get_thing_from_name(Name, wire, Wires, Devices),
			ReplyFn(wireInst:get_VI(WPID)),
			loop(Host, Wires, Devices);
		{get_simVI, Name, ReplyFn} ->
			{ok, WPID} = get_thing_from_name(Name, wire, Wires, Devices),
			ReplyFn(wireInst:get_simVI(WPID)),
			loop(Host, Wires, Devices);
		{set_measured, {Name, Measured}, ReplyFn} ->
			{ok, MPID} = get_meter_from_name(Name, Wires, Devices),
			meterInst:set_measured(MPID, Measured),
			ReplyFn(ok),
			loop(Host, Wires, Devices);
		{set_on_off, {Name, OnOff}, ReplyFn} ->
			{ok, SPID} = get_thing_from_name(Name, switch, Wires, Devices),
			switchInst:set_on_off(SPID, OnOff),
			ReplyFn(ok),
			loop(Host, Wires, Devices);
		{tick, DMsTime, ReplyFn} ->
			tick_all([SW1, US1, BAT1, CC1, ZP1, MA1, MA2, MV1], DMsTime),
			ReplyFn(ok),
			loop(Host, Wires, Devices)
	end.

tick_all([], _) -> ok;
tick_all([Device | T], DMsTime) ->
	resource_instance:tick(Device, DMsTime),
	tick_all(T, DMsTime).
	
createWire(WireTypePID)->
	{ok, Wire} = wireInst:create(self(), WireTypePID),
	Wire.

connectWire(W1, W2, C1, C2)->
	{ok, WL1} = resource_instance:list_connectors(W1),
	{ok, WL2} = resource_instance:list_connectors(W2),
	Con1 = lists:nth(C1, WL1),
	Con2 = lists:nth(C2, WL2),
	connector:connect(Con1, Con2),
	connector:connect(Con2, Con1).
	
%needs the wirelist and the devices list from the loop
%and returns {type, PID of return, name of other thing in this spot}
nameToPID(tcc, {[TCC, _, _], _, _}, _) ->									{wire      , TCC  , cc1 };
nameToPID(tbat, {[_, TBAT, _], _, _}, _) ->									{wire      , TBAT , null};
nameToPID(tv1, {[_, _, TV1], _, _}, _) ->									{wire      , TV1  , null};
nameToPID(wzp, {_, [WZP, _, _, _, _, _, _, _, _, _, _, _, _], _}, _) ->		{wire      , WZP  , zp1 };
nameToPID(w_zp, {_, [_, W_ZP, _, _, _, _, _, _, _, _, _, _, _], _}, _) ->	{wire      , W_ZP , null};
nameToPID(w_cc, {_, [_, _, W_CC, _, _, _, _, _, _, _, _, _, _], _}, _) ->	{wire      , W_CC , null};
nameToPID(wa1, {_, [_, _, _, WA1, _, _, _, _, _, _, _, _, _], _}, _) ->		{wire      , WA1  , ma1 };
nameToPID(w_a1, {_, [_, _, _, _, W_A1, _, _, _, _, _, _, _, _], _}, _) ->	{wire      , W_A1 , null};
nameToPID(wbat, {_, [_, _, _, _, _, WBAT, _, _, _, _, _, _, _], _}, _) ->	{wire      , WBAT , bat1};
nameToPID(wv1, {_, [_, _, _, _, _, _, WV1, _, _, _, _, _, _], _}, _) ->		{wire      , WV1  , mv1 };
nameToPID(w_tv1, {_, [_, _, _, _, _, _, _, W_TV1, _, _, _, _, _], _}, _) ->	{wire      , W_TV1, null};
nameToPID(wsw, {_, [_, _, _, _, _, _, _, _, WSW, _, _, _, _], _}, _) ->		{wire      , WSW  , sw1 };
nameToPID(w_sw, {_, [_, _, _, _, _, _, _, _, _, W_SW, _, _, _], _}, _) ->	{wire      , W_SW , null};
nameToPID(wa2, {_, [_, _, _, _, _, _, _, _, _, _, WA2, _, _], _}, _) ->		{wire      , WA2  , ma2 };
nameToPID(w_a2, {_, [_, _, _, _, _, _, _, _, _, _, _, W_A2, _], _}, _) ->	{wire      , W_A2 , null};
nameToPID(wus, {_, [_, _, _, _, _, _, _, _, _, _, _, _, WUS], _}, _) ->		{wire      , WUS  , us1 };
nameToPID(ezp, {_, _, [EZP, _, _, _, _]}, _) ->								{wire      , EZP  , null};
nameToPID(ecc, {_, _, [_, ECC, _, _, _]}, _) ->								{wire      , ECC  , null};
nameToPID(ebat, {_, _, [_, _, EBAT, _, _]}, _) ->							{wire      , EBAT , null};
nameToPID(ev1, {_, _, [_, _, _, EV1, _]}, _) ->								{wire      , EV1  , null};
nameToPID(eus, {_, _, [_, _, _, _, EUS]}, _) ->								{wire      , EUS  , null};
nameToPID(sw1, _, {[SW1, _, _, _, _], _}) ->								{switch    , SW1  , wsw };
nameToPID(us1, _, {[_, US1, _, _, _], _}) ->								{user      , US1  , wus };
nameToPID(bat1, _, {[_, _, BAT1, _, _], _}) ->								{batery    , BAT1 , wbat};
nameToPID(cc1, _, {[_, _, _, CC1, _], _}) ->								{chargeConv, CC1  , tcc };
nameToPID(zp1, _, {[_, _, _, _, ZP1], _}) ->								{solarPanel, ZP1  , wzp };
nameToPID(ma1, _, {_, [MA1, _, _]}) ->										{currentM  , MA1  , wa1 };
nameToPID(ma2, _, {_, [_, MA2, _]}) ->										{currentM  , MA2  , wa2 };
nameToPID(mv1, _, {_, [_, _, MV1]}) ->										{voltageM  , MV1  , wv1 };
nameToPID(_, _, _) ->														{null      , null , null}.

get_thing_from_name(Name, Thing, Wires, Devices) ->
	{Wtype, WPID, WOther} = nameToPID(Name, Wires, Devices),
	if
	Wtype == Thing ->		{ok, WPID};
	WOther /= null ->
		{Otype, OPID, _} = nameToPID(WOther, Wires, Devices),
		if
		Otype == Thing -> 	{ok, OPID};
		true ->				{nok, null}
		end;
	true ->					{nok, null}
	end.	

get_meter_from_name(Name, Wires, Devices) ->
	{Wtype, WPID, WOther} = nameToPID(Name, Wires, Devices),
	if
	(Wtype == currentM) or (Wtype == voltageM) ->		{ok, WPID};
	WOther /= null ->
		{Otype, OPID, _} = nameToPID(WOther, Wires, Devices),
		if
		(Otype == currentM) or (Otype == voltageM)  ->	{ok, OPID};
		true ->											{nok, null}
		end;
	true ->												{nok, null}
	end.
