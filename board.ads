-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- board.ads
--
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;  use Ada.Containers;
with Ada.Strings.Maps;        use Ada.Strings.Maps;


package Board is
   
   ----------------------------------------------------------------------------
   -- Types
   ----------------------------------------------------------------------------
   
   subtype Board_Row_Type is Positive range 1 .. 6;
     
   type Board_Column_Type is (A, B, C, D, E);
   
   type Board_Array_Type is
     array (Board_Row_Type'Range, Board_Column_Type'Range) of Character;
   
   type Board_Position_Type is record
      R : Board_Row_Type;
      C : Board_Column_Type;
   end record;
   
   type Move_Type is record
      From, To        : Board_Position_Type;
      Piece, Capture  : Character;
      Score           : Integer;
   end record;      
   
   
   -- Need to put these two package instantiations here, otherwise it all
   --   fails to load properly. Friggin' spaghetti.
   
   package Position_Vectors is new Vectors (Positive, Board_Position_Type);
   package Move_Vectors     is new Vectors (Positive, Move_Type);
   
   
   subtype Turn_Counter_Type is Positive range 1 .. 41;
   
   type Side_On_Move_Type is (W, B);  -- W for white, B for black
   
   type Capture_Type is (True, False, Only);
   
   type Game_State_Type is record
      Turn_Counter       : Turn_Counter_Type;
      Side_On_Move       : Side_On_Move_Type;
      Board_Array        : Board_Array_Type;
      
      Score              : Integer;
      
      White_King_In_Play : Boolean := True;
      Black_King_In_Play : Boolean := True;
      
      White_Positions,
      Black_Positions    : Position_Vectors.Vector;
      
      Move_Log           : Move_Vectors.Vector;
   end record;
   
   
   ----------------------------------------------------------------------------
   -- Package globals for external view
   ----------------------------------------------------------------------------
   
   White_Piece_Set : Character_Set;
   Black_Piece_Set : Character_Set;
   
   Game_State      : Game_State_Type;
   

   ----------------------------------------------------------------------------
   -- Package functions and procedures
   ----------------------------------------------------------------------------
   
   procedure Initialize_Game;
   
   procedure Evaluate_Score     (State         : in out Game_State_Type);
   
   function  Is_Greater         (Left, Right   : in     Move_Type)
                                return Boolean;
   
   function  Move_Generator     (State         : in     Game_State_Type;
                                 Position_List : in     Position_Vectors.vector)
                                return Move_Vectors.Vector;
   
   procedure Move_Piece         (State         : in out Game_State_Type;
                                 Move          : in out Move_Type);
   procedure Move_Piece         (State         : in out Game_State_Type;
                                 Move_String   : in     String);
   
   function  Move_Scan          (State         : in     Game_State_Type;
                                 Position      : in     Board_Position_Type;
                                 dx, dy        : in     Integer;
                                 Stop_Short    : in     Boolean      := False;
                                 Capture       : in     Capture_Type := True)
                                return Move_Vectors.Vector;
   
   function  Move_Symmetry_Scan (State         : in     Game_State_Type;
                                 Position      : in     Board_Position_Type;
                                 dx, dy        : in     Integer;
                                 Stop_Short    : in     Boolean      := False;
                                 Capture       : in     Capture_Type := True)
                                return Move_Vectors.Vector;
    
   procedure Print_Board        (State         : in     Game_State_Type);
   
   procedure Print_Move         (Move          : in     Move_Type);
   
   procedure Print_Move_List    (Move_List     : in     Move_Vectors.Vector);
   
   procedure Print_Position     (Position      : in     Board_Position_Type);
   
   procedure Print_Position_Lists
                                (State         : in     Game_State_Type);
   
   procedure Undo_Move          (State         : in out Game_State_Type);
   
   
   ----------------------------------------------------------------------------
   -- Generic package instations
   ----------------------------------------------------------------------------
   
   package Move_Sorter is new Move_Vectors.Generic_Sorting ("<" => Is_Greater);
   
end Board;
