-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- negamax.ads
--
-------------------------------------------------------------------------------

with Ada.Real_Time;  use Ada.Real_Time;
with Board;          use Board;

package Negamax is
   
   Max_Depth : Integer := 40;
   Max_Time  : Integer := 7450; -- Max nr milliseconds of thinking per turn;
   
   
   function Negamax (State         : in out Game_State_Type;
                     Depth         : in     Integer;
                     Top_Level     : in     Integer;
                     Alpha,
                     Beta          : in     Integer;
                     Best_Move     :    out Move_Type;
                     Best_Depth    :    out Integer;
                     Thinking_Time : in     Time;
                     Partial_Flag  : in out boolean)
                    return Integer;
   
end Negamax;
