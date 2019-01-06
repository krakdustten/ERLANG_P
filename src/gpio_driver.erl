
-module(gpio_driver).
%%-enum(measurementDevice, {chargeI, dischargeI, batteryV}).

-export([init/0, measureValue/2, setRelay/2]).


init() ->
    {ok, Spi} = spi:start_link("spidev0.0", []),
    {ok, MS0} = gpio:start_link(16, output),
    {ok, MS1} = gpio:start_link(20, output),
    {ok, MS2} = gpio:start_link(21, output),
    {ok, RLA} = gpio:start_link(26, output),
    CurB = {3221, 3276, 3292, 3352}, %OFFIN, OFFOUT, ONIN, ONOUT
    {Spi, MS0, MS1, MS2, RLA, CurB}.

measureValue({Spi, MS0, MS1, MS2, _, {CBFI, CBFO, CBTI, CBTO}}, chargeI) ->
    Val = measureValue({Spi, MS0, MS1, MS2, {CBFI, CBFO, CBTI, CBTO}}, 1, 0),
    %io:format("chargeI: ~p \n", [Val]),
    if
    abs(Val - CBFI) > 50 ->
	abs(Val - CBTI) * 0.01;
    true ->
	abs(Val - CBFI) * 0.01
    end;
measureValue({Spi, MS0, MS1, MS2, _, {CBFI, CBFO, CBTI, CBTO}}, dischargeI) ->
    Val = measureValue({Spi, MS0, MS1, MS2, {CBFI, CBFO, CBTI, CBTO}}, 2, 0),
    %io:format("dischargeI: ~p \n", [Val]),
    if
    abs(Val - CBFO) > 50 ->
	abs(Val - CBTO) * 0.01;
    true ->
	abs(Val - CBFO) * 0.01
    end;
measureValue({Spi, MS0, MS1, MS2, _, {CBFI, CBFO, CBTI, CBTO}}, batteryV) ->
    Val = measureValue({Spi, MS0, MS1, MS2, {CBFI, CBFO, CBTI, CBTO}}, 0, 0),
    %io:format("batteryV: ~p \n", [Val]),
    (3.0 / 4096.0) * Val * 6.5.

measureValue({Spi, MS0, MS1, MS2, CurB}, D, _) ->
    <<I2:1, I1:1, I0:1>> = <<D:3>>,
    gpio:write(MS0, I0),
    gpio:write(MS1, I1),
    gpio:write(MS2, I2),
    timer:sleep(200),
    measureValueAverage(1000, Spi).


measureValueAverage(D, Spi)->
	measureValueAverage(D, 0, 0, Spi) .
measureValueAverage(0, Total, Amount, Spi) ->
	Total/Amount;
measureValueAverage(D, Total, Amount, Spi) ->
	<<_:4, Value:12>> = spi:transfer(Spi, <<16#00, 16#00>>),
	measureValueAverage(D - 1, Total + Value, Amount + 1, Spi).


setRelay({_, _, _, _, RLA, _}, V) ->
    gpio:write(RLA, V).