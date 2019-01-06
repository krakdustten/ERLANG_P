-module(t).
-export([go/0, g/0]).



%   +---------+ B          C  +---------+ E          F  +---------+ G          H           K          N          O  +---------+ P          Q  +---------+ R          S  +---------+
%   | ZP      | ##  W_ZP   ## | CC      | ##  W_CC   ## | A1      | ##  W_A1   ##  TBAT    ##  TV1    ##  W_TV1  ## | SW      | ##  W_SW   ##+| A2      |-##  W_A2   ## | US      |
%   | WZP     +-21---------21-+ TCC     +-31---------21-+ WA1     +-21---------21----+-----31----+----31---------21-+ WSW     +-21---------21-+ WA2     +-21---------21-+ WUS     |
%   |         | ##         ## |         | ##         ## |         | ##         ##    |     ##    |    ##         ## |         | ##         ## |         | ##         ## |         |
%   +----+----+               +----+----+               +---------+                  |           |                  +---------+               +---------+               +----+----+
%        |                         |                                              I #2#       L #2#                                                                          |
%       #1# A                     #2# D                                             #1#         #1#                                                                         #2# T
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


go()->
	{ok, DT_PID} = digitalTwin:create(self(), fun(D) -> survivor:entry(D) end),
	
	digitalTwin:set_measured(DT_PID, ma1, 1.354),
	digitalTwin:set_measured(DT_PID, ma2, 2.346),
	digitalTwin:set_measured(DT_PID, mv1, 15.346),

	digitalTwin:tick(DT_PID, 1000),
	digitalTwin:tick(DT_PID, 1000),
	digitalTwin:tick(DT_PID, 1000),
	Cur = digitalTwin:get_VI(DT_PID, w_tv1),
	CurS = digitalTwin:get_simVI(DT_PID, w_tv1),
	digitalTwin:set_on_off(DT_PID, sw1, 1),
	digitalTwin:tick(DT_PID, 2000),
	digitalTwin:tick(DT_PID, 3000),
	digitalTwin:tick(DT_PID, 2000),
	Cur1 = digitalTwin:get_VI(DT_PID, w_tv1),
	Cur1S = digitalTwin:get_simVI(DT_PID, w_tv1),
	
	Cur1Z = digitalTwin:get_simVI(DT_PID, w_zp),
	
	[Cur, CurS, Cur1, Cur1S, Cur1Z].
g()->
	observer:start(),
	survivor:start(),
	go().