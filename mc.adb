-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- mc.adb
--
-------------------------------------------------------------------------------

with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Board;                use Board;
with Negamax;              use Negamax;

procedure MC is
   
   Move_List   : Move_Vectors.Vector;
   Move_Cursor : Move_Vectors.Cursor;
   Best_Move   : Move_Type;
   Move        : Move_Type;
   
   Best_Score,
   Nega_Score  : Integer := -10_000;
   
   Position    : Board_Position_Type := (2, A);
   
begin
   
   Initialize_Game;
   
   Print_Board (Game_State);
   
   New_Line;
   
   --  Move_Piece (Game_State, "b2-b3");
   
   --  Print_Board (Game_State);
   
   Put_Line ("Move list:");
   
   Move_List := Move_Generator (Game_State, Game_State.White_Positions);
      
   Print_Move_List (Move_List);
   
   
   Move_Cursor := Move_Vectors.First (Move_List);
   Best_Score  := Game_State.Score;
   Best_Move   := Move_Vectors.Element (Move_Cursor);
   
   while Move_Vectors.Has_Element (Move_Cursor) loop
      
      Move := Move_Vectors.Element (Move_Cursor);
      
      Move_Piece (Game_State, Move);
      
      Print_Board (Game_State);
      
      Nega_Score := - Negamax.Negamax (Game_State, 4);
      
      Best_Score := Integer'Max (Best_Score, Nega_Score);
      
      Undo_Move (Game_State);
      
      Move_Cursor := Move_Vectors.Next (Move_Cursor);
      
   end loop;
   
   Put ("Best score: ");
   Put (Best_Score, 0);
   Put ("  ");
   Print_Move (Best_Move);
   New_Line;
   
end;
