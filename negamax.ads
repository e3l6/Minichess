-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- negamax.ads
--
-------------------------------------------------------------------------------

with Board; use Board;

package Negamax is
   
   Max_Depth          : Integer := 6;
   
   Nr_Nodes_Searched,
   Nr_Branches_Pruned : Integer := 0;
   
   --  function Negamax (State      : in out Game_State_Type;
   --                    Depth      : in     Integer;
   --                    Top_Level  : in     Integer;
   --                    Alpha,
   --                    Beta       : in     Integer;
   --                    Best_Move  :    out Move_Type;
   --                    Best_Depth :    out Integer)
   --                   return Integer;
   
   function Negamax (State      : in out Game_State_Type;
                     Depth      : in     Integer;
                     Top_Level  : in     Integer;
                     Alpha,
                       Beta     : in     Integer;
                     Move_List  : in out Move_Vectors.Vector)
                    return integer;

end Negamax;
