-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- negamax.ads
--
-------------------------------------------------------------------------------

with Board; use Board;

package Negamax is
   
   Max_Depth : Integer := 4;
   
   
   function Negamax (State : in out Game_State_Type;
                     Depth : in     Integer)
                    return Integer;
   
end Negamax;
