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
   
   function Negamax (State      : in out Game_State_Type;
                     Depth      : in     Integer;
                     Alpha,
                     Beta       : in     Integer;
                     Best_Move  :    out Move_Type;
                     Best_Depth :    out Integer)
                    return integer is
      
      Move_Cursor : Move_Vectors.Cursor;
      Move_List   : Move_Vectors.Vector;
      Move        : Move_Type;
      
      Best_Score,
      Nega_Score  : Integer;
      
      Local_Alpha      : Integer := Alpha;
      Local_Beta       : Integer := Beta;
      
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
      
      Best_Score  := -10_000;
      Move_Cursor := Move_Vectors.First (Move_List);
      
      --  Put_Line ("Depth : " & Integer'Image (Depth));
      --  Print_Position_Lists (State);
      
  Child_Search_Loop :
      while (Move_Vectors.Has_Element (Move_Cursor)) loop
         
         Move := Move_Vectors.Element (Move_Cursor);
         
         Move_Piece (State, Move);
         
         Nega_Score := - Negamax (State, Depth - 1,
                                  - Local_Beta, - Local_Alpha,
                                  Best_Move, Best_Depth);
         
         Undo_Move (State);
                  
         if (Depth = 1) then
            Best_Depth := Depth;
         end if;
         
         --  if (Nega_Score = Best_Score) then
         --     if (Depth > Best_Depth) then
                              
         --        Best_Depth := Depth;
               
         --        if (Depth = Max_Depth) then
         --           Put ("New best move: ");
         --           Print_Move (Move);
         --           New_Line;
         --           Best_Move  := Move;
         --        end if;
               
         --     end if;
         --  end if;
               
         
         if (Nega_Score > Best_Score) then
            
            Best_Score := Nega_Score;
            Best_Depth := Depth;
            
            if (Depth = Max_Depth) then
               Best_Move  := Move;
            end if;
            
         end if;
         
         Local_Alpha := Integer'Max (Local_Alpha, Nega_Score);
         
         --  if (Depth = Max_Depth) then
         --     Put ("Best Depth :" & Integer'Image (Best_Depth));
         --     --  Put        (" SOM " & Side_On_Move_Type'Image (State.Side_On_Move));
         --     Put ("  Move : ");
         --     Print_Move (Move);
         --     Set_Col (44);
         --     Put ("Best : ");
         --     Put (Best_Score, 6);
         --     Put ("  Nega : ");
         --     Put (Nega_Score, 6);
         --     New_Line;
         --  end if;
         
         if (Local_Alpha >= Local_Beta) then
            
            exit Child_Search_Loop;
            
         end if;
         
         Move_Vectors.Next (Move_Cursor);
         
      end loop Child_Search_Loop;
      
      return Best_Score;   
      
   end Negamax;
   
end Negamax;
