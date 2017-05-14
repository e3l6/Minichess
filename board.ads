-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- board.ads
--
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;  use Ada.Containers;

package Board is
   
   ----------------------------------------------------------------------------
   -- Types
   ----------------------------------------------------------------------------
   
   subtype Board_Row_Type is Positive range 1 .. 6;
     
   type Board_Column_Type is (A, B, C, D, E);
   
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
   
   subtype Turn_Counter_Type is Positive range 1 .. 41;
   
   type Side_On_Move_Type is (W, B);  -- W for white, B for black
   
   type Board_State_Type is record
      Turn_Counter : Turn_Counter_Type;
      Side_On_Move : Side_On_Move_Type;
      Board_Array  : access Board_Array_Type;
   end record;
   
   type Capture_Type is (True, False, Only);
   
   
   ----------------------------------------------------------------------------
   -- Generic package instantiations
   ----------------------------------------------------------------------------
   
   package Position_Vectors is new Vectors (Positive, Board_Position_Type);
   package Move_Vectors     is new Vectors (Positive, Move_Type);
   
   
   ----------------------------------------------------------------------------
   -- Package Globals
   ----------------------------------------------------------------------------
   
   White_Positions : Position_Vectors.Vector;
   Black_Positions : Position_Vectors.Vector;
   Illegal_Move    : exception;
   
   
   ----------------------------------------------------------------------------
   -- Package functions and procedures
   ----------------------------------------------------------------------------
   
   procedure Initialize_Game;
   
   function  Move_Generator     (Position_List : in     Position_Vectors.vector)
                                return Move_Vectors.Vector;

   
   procedure Move_Piece         (Move          : in out Move_Type);
   procedure Move_Piece         (Move_string   : in     String);
   
   function  Move_Scan          (Position      : in     Board_Position_Type;
                                 dx, dy        : in     Integer;
                                 Stop_Short    : in     Boolean      := False;
                                 Capture       : in     Capture_Type := True)
                                return Move_Vectors.Vector;
   
   function  Move_Symmetry_Scan (Position      : in     Board_Position_Type;
                                 dx, dy        : in     Integer;
                                 Stop_Short    : in     Boolean      := False;
                                 Capture       : in     Capture_Type := True)
                                return Move_Vectors.Vector;
    
   procedure Print_Board;
   
   procedure Print_Move         (Piece         : in     Character;
                                 Move          : in     Move_Type);
   
   procedure Print_Move_List    (Move_List     : in     Move_Vectors.Vector);
   
   procedure Print_Position     (Position      : in     Board_Position_Type);
   
   procedure Print_Position_Lists;
   
   procedure Undo_Move;
   
end Board;
