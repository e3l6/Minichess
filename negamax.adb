-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- negamax.adb
--
-------------------------------------------------------------------------------

with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Text_IO;          use Ada.Text_IO;

package body Negamax is
   
   function Negamax (State         : in out Game_State_Type;
                     Depth         : in     Integer;
                     Top_Level     : in     Integer;
                     Alpha,
                     Beta          : in     Integer;
                     Best_Move     :    out Move_Type;
                     Best_Depth    :    out Integer;
                     Thinking_Time : in     Time;
                     Partial_Flag  : in out Boolean)
                    return integer is
      
      Move_Cursor     : Move_Vectors.Cursor;
      Move_List       : Move_Vectors.Vector;
      Move            : Move_Type;
      
      Best_Score,
      Nega_Score      : Integer;
      
      Local_Alpha     : Integer := Alpha;
      Local_Beta      : Integer := Beta;
      
   begin
      
      Evaluate_Score (State);
      
      if ((Depth <= 0) or
          (State.Turn_Counter = 41) or
          ((State.White_King_In_Play = False) or
           (State.Black_King_In_Play = False))) then
         
         return (State.Score);
         
      end if;
      
      if (State.Side_On_Move = W) then
         
         if (State.White_King_In_Play = False) then
            
            return -10_000;
            
         end if;
         
         Move_List := Move_Generator (State, State.White_Positions);
         
      else
         
         if (State.Black_King_In_Play = False) then
            
            return -10_000;
            
         end if;
         
         Move_List := Move_Generator (State, State.Black_Positions);
         
      end if;
      
      Best_Score      := -10_000;
      Move_Cursor     := Move_Vectors.First (Move_List);
      
  Child_Search_Loop :
      while (Move_Vectors.Has_Element (Move_Cursor)) loop
         
         if (Clock > Thinking_Time) then
            
            Best_Score := Integer'First + 1;
            
            Partial_Flag := True;
            
            exit Child_Search_Loop;
            
         end if;
         
         
         Move := Move_Vectors.Element (Move_Cursor);
         
         Move_Piece (State, Move);
         
         Nega_Score := - Negamax (State, Depth - 1, Top_Level,
                                  - Local_Beta, - Local_Alpha,
                                  Best_Move, Best_Depth,
                                  Thinking_Time, Partial_Flag);
         
         Undo_Move (State);
                  
         
         if (Depth = 1) then
            Best_Depth := Depth;
         end if;
         
         
         if (Nega_Score > Best_Score) then
            
            Best_Score := Nega_Score;
            Best_Depth := Depth;
            
            if (Depth = Top_level) then
               Best_Move  := Move;
            end if;
            
         end if;
         
         
         Local_Alpha := Integer'Max (Local_Alpha, Nega_Score);
         
         
         if (Local_Alpha >= Local_Beta) then
            
            exit Child_Search_Loop;
            
         end if;
         
         
         Move_Vectors.Next (Move_Cursor);
         
      end loop Child_Search_Loop;
      
      return Best_Score;   
      
   end Negamax;
   
end Negamax;
