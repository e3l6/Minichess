-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- board.ads
--
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;  use Ada.Containers;

package Board is
   
   Illegal_Move     : exception;
   
   
   ----------------------------------------------------------------------------
   subtype Turn_Counter_Type is Positive range 1 .. 41;
   
   type Side_On_Move_Type is (W, B);  -- W for white, B for black
     
   type Board_Column_Type is (A, B, C, D, E);
   
   subtype Board_Row_Type is Positive range 1 .. 6;
   
   type Board_Array_Type is array (Board_Row_Type'Range, Board_Column_Type'Range) of
     Character;
   
   type Board_Position_Type is record
      R : Board_Row_Type;
      C : Board_Column_Type;
   end record;
   
   type Move_Type is record
      From, To : Board_Position_Type;
      Capture  : Character;
   end record;      
   
   type Board_State_Type is record
      Turn_Counter : Turn_Counter_Type;
      Side_On_Move : Side_On_Move_Type;
      Board_Array  : access Board_Array_Type;
   end record;
   
   
   ----------------------------------------------------------------------------
   package Move_Vectors is new Vectors (Positive, Move_Type);
   
   
   ----------------------------------------------------------------------------
   procedure Initialize_Game;
   
   procedure Move_Piece  (Move        : in     Move_Type);
   procedure Move_Piece  (Move_string : in     String);
   
   function  Move_Scan   return Move_Vectors.Vector;
   
   procedure Play_Game;
   
   function  Next_Move   return Integer;
   
   
   procedure Print_Board;
   
   procedure Undo_Move;
   
end Board;