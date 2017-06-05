-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- negamax.adb
--
-------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Ada.Text_IO;             use Ada.Text_IO;
with Board;                   use Board;

package body Negamax is
   
   --  function Negamax (State      : in out Game_State_Type;
   --                    Depth      : in     Integer;
   --                    Top_Level  : in     Integer;
   --                    Alpha,
   --                    Beta       : in     Integer;
   --                    Best_Move  :    out Move_Type;
   --                    Best_Depth :    out Integer)
   --                   return integer is
   
   function Negamax (State      : in out Game_State_Type;
                     Depth      : in     Integer;
                     Top_Level  : in     Integer;
                     Alpha,
                       Beta     : in     Integer;
                     Move_List  : in out Move_Vectors.vector)
                    return Integer is
      
      Local_Move_List   : Move_Vectors.Vector;
      Local_Move_Cursor : Move_Vectors.Cursor;
      Move_Cursor       : Move_Vectors.Cursor;
      Move              : Move_Type;
      
      Best_Score,
      Nega_Score      : Integer;
      
      Local_Alpha     : Integer := Alpha;
      Local_Beta      : Integer := Beta;
      
   begin
      
      Evaluate_Score (State);
      
      --  Put ("Negamax state score ");
      --  Put (State.Score);
      --  Put (" Depth ");
      --  Put (Depth);
      --  New_Line;
      
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
         
         Local_Move_List := Move_Generator (State, State.White_Positions);
         
         --  if (Depth = Top_Level) then
         
         --     Print_Move_List (Move_List);
         
         --  end if;
         
      else
         
         if (State.Black_King_In_Play = False) then
            
            return -10_000;
            
         end if;
         
         Local_Move_List := Move_Generator (State, State.Black_Positions);
         
         --  if (Depth = Top_Level) then
         
         --     Print_Move_List (Move_List);
         
         --  end if;
         
      end if;
      
      
      if (Depth = Top_Level) then
         
         Move_List := Move_Vectors.Copy (Local_Move_List);
         
      end if;
      
      
      Best_Score  := -10_000;
      Local_Move_Cursor := Move_Vectors.First (Local_Move_List);
      Move_Cursor       := Move_Vectors.First (Move_List);
      --  Put_Line ("Depth : " & Integer'Image (Depth));
      --  Print_Position_Lists (State);
      
  Child_Search_Loop :
      while (Move_Vectors.Has_Element (Local_Move_Cursor)) loop
         
         Nr_Nodes_Searched := Nr_Nodes_Searched + 1;
         
         Move := Move_Vectors.Element (Local_Move_Cursor);
         
         --  Print_Move_List (Move_List);
         
         if (State.Side_On_Move = W) then
            if (not Is_In (Move.piece, White_Piece_Set)) then
               Put_Line ("*** ERROR ***");
               Put_Line ("Move Log:");
               Print_Move_List      (State.Move_Log);
               Print_Board          (State);
               Print_Position_Lists (State);
               Print_Move_List      (Local_Move_List);
               Put ("Attempting move ");
               Print_Move (Move);
               New_Line;
               Put      ("Position ");
               Print_Position (Move.From);
               Put_Line (" is marked '" & State.Board_Array(Move.From.R, Move.From.C) & "'");
               --  raise Illegal_Move with "Origin piece not owned by side on move";
               
               -- Skip the rest of the loop and go to the next move
               goto Continue_Loop;
            end if;
         else
            if (not Is_In (Move.piece, Black_Piece_Set)) then
               Put_Line ("*** ERROR ***");
               Put_Line ("Move Log:");
               Print_Move_List      (State.Move_Log);
               Print_Board          (State);
               Print_Position_Lists (State);
               Print_Move_List      (Local_Move_List);
               Put ("Attempting move ");
               Print_Move (Move);
               New_Line;
               Put      ("Position ");
               Print_Position (Move.From);
               Put_Line (" is marked '" & State.Board_Array(Move.From.R, Move.From.C) & "'");
               --  raise Illegal_Move with "Origin piece not owned by side on move";
               
               -- Skip the rest of the loop and go to the next move
               goto Continue_Loop;
            end if;
         end if;            

         Move_Piece (State, Move);
         
         --  Nega_Score := - Negamax (State, Depth - 1, Top_Level,
         --                           - Local_Beta, - Local_Alpha,
         --                           Best_Move, Best_Depth);
         
         Nega_Score := - Negamax (State, Depth - 1, Top_Level,
                                  - Local_Beta, - Local_Alpha,
                                  Local_Move_List);
         
         Undo_Move (State);
         
         --  if (Depth = 1) then
         --     Best_Depth := Depth;
         --  end if;
         
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
         
         
         if (Depth = Top_level) then
            
            --  Best_Move  := Move;
            Move.Score := Nega_Score;
            
            if (Move.Capture /= '.') then
               Move.Score := Move.Score + 50;
            end if;
            
            Move_Vectors.Replace_Element (Move_List, Move_Cursor, Move);
            
         end if;
         
         if (Nega_Score > Best_Score) then
            
            Best_Score := Nega_Score;
            --  Best_Depth := Depth;
            
         end if;
         
         Local_Alpha := Integer'Max (Local_Alpha, Nega_Score);
         
         --  if (Depth = Top_Level) then
         --     Put ("Best Depth :" & Integer'Image (Best_Depth));
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
            
            Nr_Branches_Pruned := Nr_Branches_Pruned + 1;
            
            exit Child_Search_Loop;
            
         end if;
         
     <<Continue_Loop>>
         
         Move_Vectors.Next (Local_Move_Cursor);
         
         if (Depth = Top_Level) then
            
            Move_Vectors.Next (Move_Cursor);
            
         end if;
         
      end loop Child_Search_Loop;
      
      return Best_Score;   
      
   end Negamax;
   
end Negamax;
