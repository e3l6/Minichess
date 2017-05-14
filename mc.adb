-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- mc.adb
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Board;       use Board;

procedure MC is
   
   Move_List : Move_Vectors.Vector;
   
begin
   
   Initialize_Game;
   
   Print_Board;
   
   Print_Position_Lists;
      
   Put_Line ("Move list:");
   
   Move_List := Move_Generator (White_Positions);
   
   Print_Move_List (Move_List);
   
   Put_Line ("Move:");
   
   Move_Piece ("b1-c3");
   
   Print_Board;
   
   Print_Position_Lists;
   
   Put_Line ("Move list:");
   
   Move_List := Move_Generator (Black_Positions);
   
   Print_Move_List (Move_List);
   
   Put_Line ("Move:");
   
   Move_Piece ("a5-a4");
   
   Print_Board;
   
   Print_Position_Lists;
   
   Put_Line ("Move list:");
   
   Move_List := Move_Generator (White_Positions);
     
   Print_Move_List (Move_List);
   
   Put_Line ("Move:");
   
   Move_Piece ("c3-a4");
   
   Print_Board;
end;
