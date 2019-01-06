%%%-------------------------------------------------------------------
%% @doc erlang_project public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_project_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	survivor:start(),
	GPIO = gpio_driver:init(),
	{ok, DT_PID} = digitalTwin:create(self(), fun(D) -> gpio_driver:setRelay(GPIO, D) end),
	loop(GPIO, DT_PID, erlang:monotonic_time(milli_seconds)).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

loop(GPIO, DT_PID, LastT) ->
	A1 = gpio_driver:measureValue(GPIO, chargeI),
	A2 = gpio_driver:measureValue(GPIO, dischargeI),
	V1 = gpio_driver:measureValue(GPIO, batteryV),
	io:format("--------------------------------------------------------------------------------------------------------------\n", []),
	io:format("Measurements: ~p ~p ~p \n", [A1, A2, V1]),
	digitalTwin:set_measured(DT_PID, ma1, A1),
	digitalTwin:set_measured(DT_PID, ma2, A2),
	digitalTwin:set_measured(DT_PID, mv1, V1),
	DiffT = erlang:monotonic_time(milli_seconds) - LastT,
	digitalTwin:tick(DT_PID, DiffT),
	io:format("Batery: ~p ~p \n", [digitalTwin:get_VI(DT_PID, wbat), digitalTwin:get_simVI(DT_PID, wbat)]),
	io:format("User: ~p ~p \n", [digitalTwin:get_VI(DT_PID, wus), digitalTwin:get_simVI(DT_PID, wus)]),
	io:format("Solar Panel: ~p ~p \n", [digitalTwin:get_VI(DT_PID, wzp), digitalTwin:get_simVI(DT_PID, wzp)]),
	doDiscision(GPIO, DT_PID),
	timer:sleep(1000),
	loop(GPIO, DT_PID, LastT + DiffT).
	
doDiscision(GPIO, DT_PID) ->
	{VBat, IBat} = digitalTwin:get_VI(DT_PID, wbat),
	if
	VBat < 11 -> 
		digitalTwin:set_on_off(DT_PID, sw1, 0);
	true ->
		digitalTwin:set_on_off(DT_PID, sw1, 1)
	end.