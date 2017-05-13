-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- mc.adb
--
-------------------------------------------------------------------------------

with Board;  use Board;

procedure MC is
   
begin
   
   Initialize_Game;
   
   Print_Board;
   
   Undo_Move;  -- Invalid (No moves played yet);
   
   Move_Piece ("a2-a3"); -- Valid
   
   Print_Board;
   
   --  Move_Piece ("a2-a3"); -- Invalid (no piece to move)
   
   --  Print_Board;
   
   Move_Piece ("b5-b4"); -- Valid
   
   Print_Board;
   
   --  Move_Piece ("a3-b1"); -- Invalid (Attempting to caputure own piece)
   
   --  Print_Board;
   
   Undo_Move;
   
   Print_Board;
   
end;
