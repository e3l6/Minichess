-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- negamax.adb
--
-------------------------------------------------------------------------------

with Ada.Text_Io;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

package body Negamax is
   
   function Negamax (State : in out Game_State_Type;
                     Depth : in     Integer)
                    return integer is
      
      Move_Cursor : Move_Vectors.Cursor;
      Move_List   : Move_Vectors.Vector;
      Move        : Move_Type;
      
      Best_Score,
      Nega_Score  : Integer;
      
   begin
      
      Best_Score  := State.Score;
      
      if ((Depth <= 0) or (State.Turn_Counter > 40)) then
         
         Put_Line ("Max depth reached. Backing out");
         New_Line;
         
         return (Best_Score);
         
      end if;
      
      if (State.Side_On_Move = W) then
         
         if (State.White_King_In_Play = False) then
            
            return -10_000;
            
         end if;
         
         Move_List   := Move_Generator (State, State.White_Positions);
         
      else
         
         if (State.Black_King_In_Play = False) then
            
            return -10_000;
            
         end if;
         
         Move_List   := Move_Generator (State, State.Black_Positions);
         
      end if;
      
      Move_Cursor := Move_Vectors.First (Move_List);
      
      while (Move_Vectors.Has_Element (Move_Cursor)) loop
         
         Move := Move_Vectors.Element (Move_Cursor);
         
         Move_Piece (State, Move);
         
         Put ("SOM: " & Side_On_Move_Type'Image (State.Side_On_Move) & "  D:");
         Put (Depth, 0);
         Put ("  ");
         Put (State.Score, 5);
         Put (": ");
         Print_Move (Move);
         New_Line (2);
         
         Print_Board (State);
         
         Nega_Score := - Negamax (State, Depth - 1);
         
         Best_Score := Integer'Max (Best_Score, Nega_Score);
         
         Put ("Best score : ");
         Put (Best_Score, 0);
         Put ("  Nega score : ");
         Put (Nega_Score, 0);
         New_Line;
         
         Undo_Move (State);
         
         Move_Vectors.Next (Move_Cursor);
         
      end loop;
      
      return Best_Score;   
      
   end Negamax;
   
end Negamax;
