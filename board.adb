-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- board.adb
--
-------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Strings.Maps;     use Ada.Strings.Maps;
with Ada.Text_IO;          use Ada.Text_IO;

package body Board is
   
   White_Piece_Set : Character_Set := To_Set ("BKNPRQ");
   Black_Piece_Set : Character_Set := To_Set ("bknprq");
   
   White_Positions : Position_Vectors.Vector;
   Black_Positions : Position_Vectors.Vector;
   
   Board_State     : Board_State_Type;
   
   Game_Log        : Move_Vectors.Vector;
   
   ----------------------------------------------------------------------------
   procedure Initialize_Game is
      
   begin
      
      Board_State.Turn_Counter := 1;
      Board_State.Side_On_Move := W;
      
      Board_State.Board_Array :=
        new Board_Array_Type'(('R', 'N', 'B', 'Q', 'K'),
                              ('P', 'P', 'P', 'P', 'P'),
                              ('.', '.', '.', '.', '.'),
                              ('.', '.', '.', '.', '.'),
                              ('p', 'p', 'p', 'p', 'p'),
                              ('k', 'q', 'b', 'n', 'r'));
      
      -- Scan the board and populate the position vectors for each player
      
      for R in Board_Row_Type'Range loop
         
         for C in Board_Column_Type'Range loop
            
            if (Is_In (Board_State.Board_Array(R, C), White_Piece_Set)) then
               
               Position_Vectors.Append (White_Positions, (R, C));
               
            end if;
            
            if (Is_In (Board_State.Board_Array(R,C), Black_Piece_Set)) then
               
               Position_Vectors.Append (Black_Positions, (R, C));
               
            end if;
            
         end loop;
         
      end loop;
      
   end;
   
   
   ----------------------------------------------------------------------------
   --  procedure Move_generator is
   
   
   
   
   ----------------------------------------------------------------------------
   procedure Move_Piece (Move : in out Move_Type) is
      
      Cursor : Position_Vectors.Cursor;
      
   begin
      
      -- Check that we're moving a piece that belongs to the side on move,
      --   then check that the captured piece (including '.' for simplicity)
      --   isn't owned by the side on move. If either condition isn't true,
      --   then throw an exception. Then, update the positions list for the
      --   side on move and the "capture piece". If an actual piece was
      --   captured, remove it from the opposing player's position list.
      
      if (Board_State.Side_On_Move = W) then
         
         if (not Is_In (Board_State.Board_Array(Move.From.R, Move.From.C),
                        White_Piece_Set)) then
            
            raise Illegal_Move with "Origin piece not owned by side on move";
            
         end if;
         
         if (Is_In (Board_State.Board_Array(Move.To.R, Move.To.C),
                    White_Piece_Set)) then
            
            raise Illegal_Move with "White attempting to capture own piece";
            
         end if;
         
         -- Update the positions lists
         
         Cursor := Position_Vectors.Find  (White_Positions,
                                           Move.From);
         Position_Vectors.Replace_Element (White_Positions,
                                           Cursor,
                                           Move.To);
         
         Move.Capture := Board_State.Board_Array(Move.To.R, Move.To.C);
         
         if (Move.Capture /= '.') then
            
            Cursor := Position_Vectors.Find (Black_Positions,
                                             Move.To);
            Position_Vectors.Delete         (Black_Positions,
                                             Cursor);
         end if;
         
         -- Change the side on move
         
         Board_State.Side_On_Move := B;
         
      else
         
         if (not Is_In (Board_State.Board_Array(Move.From.R, Move.From.C),
                        Black_Piece_Set)) then
            
            raise Illegal_Move with "Origin piece not owned by side on move";
            
         end if;
         
         if (Is_In (Board_State.Board_Array(Move.To.R, Move.To.C),
                    Black_Piece_Set)) then
            
            raise Illegal_Move with "Black attempting to capture own piece";
            
         end if;
         
         -- Update the positions lists
         
         Cursor := Position_Vectors.Find  (Black_Positions,
                                           Move.From);
         Position_Vectors.Replace_Element (Black_Positions,
                                           Cursor,
                                           Move.To);
         
         Move.Capture := Board_State.Board_Array(Move.To.R, Move.To.C);
         
         if (Move.Capture /= '.') then
            
            Cursor := Position_Vectors.Find (White_Positions,
                                             Move.To);
            Position_Vectors.Delete         (White_Positions,
                                             Cursor);
         end if;
         
         -- Increment the turn counter and change the side on move.
         
         Board_State.Turn_Counter := Board_State.Turn_Counter + 1;
         Board_State.Side_On_Move := W;
         
      end if;
      
      -- Log the move in the running game log
      
      Move_Vectors.Append (Game_Log, Move);
      
      -- Move the piece to the new position and clear the new position
      
      Board_State.Board_Array(Move.To.R, Move.To.C) :=
        Board_State.Board_Array(Move.From.R, Move.From.C);
      
      Board_State.Board_Array(Move.From.R, Move.From.C) := '.';
      
   end Move_Piece;


   ----------------------------------------------------------------------------
   procedure Move_Piece (Move_String : in     String) is
      
      Element : String (1 .. 1);
      Move    : Move_Type;
      
   begin
      
      -- Move strings are given in "a2-a3" format, I.e. ColRow-ColRow,
      --   convert them and feed them to a Move_Type
      
      Element(1)  := Move_String(Move_String'First);
      Move.From.C := Board_Column_Type'Value (Element);
      
      Element(1)  := Move_String(Move_String'First + 1);
      Move.From.R := Positive'Value (Element);
      
      Element(1)  := Move_String(Move_String'First + 3);
      Move.To.C   := Board_Column_Type'Value (Element);
      
      Element(1)  := Move_String(Move_String'First + 4);
      Move.To.R   := Positive'Value (Element);
      
      Move.Capture := '.';
      
      Move_Piece (Move);
      
      Print_Move (Board_State.Board_Array(Move.To.R, Move.To.C),
                  Move);
      
      New_Line (2);
      
   end;
   
   
   ----------------------------------------------------------------------------
   function Move_Scan (Position   : in     Board_Position_Type;
                       dx, dy     : in     Integer;
                       Stop_Short : in     Boolean      := False;
                       Capture    : in     Capture_Type := True)
                      return Move_Vectors.Vector is
      
      Move_List : Move_Vectors.Vector;
      X, Y      : Integer;
      Stop_Flag : Boolean := Stop_Short;
      Piece     : Character;             -- The contents of 
                                         --   Board_Array(X + dx, Y + dy)

   begin
      
      -- Convert the board position to a pair of integers (X, Y) so we can
      --   math on them more easily (particulary for checking our move won't
      --   take us off the board without throwing an exception)
      
      X     := Position.R;
      Y     := Board_Column_Type'Pos (Position.C);
            
  Scan_Loop :
      loop
         
         X := X + dx;
         Y := Y + dy;
         
         if ((X < Board_Row_Type'First) or
             (X > Board_Row_Type'Last)  or
             (Y < Board_Column_Type'Pos (Board_Column_Type'First)) or
             (Y > Board_Column_Type'Pos (Board_Column_Type'Last))) then
            
            exit Scan_Loop;
            
         end if;
         
         Piece := Board_State.Board_Array(X, Board_Column_Type'Val (Y));
         
         if (Piece /= '.') then                         -- We found a piece
            
            if (Board_State.Side_On_Move = W) then      -- Check to see if it's
                                                        --   one of ours. If so
               if (Is_In (Piece, White_Piece_Set)) then --   exit the scan loop
                  
                  exit Scan_Loop;
                  
               end if;
               
            else
               
               if (Is_In (Piece, Black_Piece_Set)) then
                  
                  exit Scan_Loop;
                  
               end if;
               
            end if;
            
            if (Capture = False) then  -- We found an opponent's piece, but
                                       --   can't take it. Exit the scan loop
               exit Scan_Loop;
               
            end if;
            
            -- There's an opponent's piece, we've made it through all the
            --   reasons we can't take it, so set the stop flag so we can enter
            --   this move in the move list and end this direction of search
            
            Stop_Flag := True;
            
         elsif (Capture = Only) then   -- This direction of search only
                                       --   allowed our piece to capture on
            exit Scan_Loop;            --   this move, but there wasn't a
                                       --   piece there.
         end if;
         
         Move_Vectors.Append (Move_List,
                              (Position,
                               (X, Board_Column_Type'Val (Y)),
                               Piece));
         
         exit Scan_Loop when (Stop_Flag = True);
         
      end loop Scan_Loop;
      
      return Move_List;
      
   end Move_Scan;
   
   
   ----------------------------------------------------------------------------
   function Move_Symmetry_Scan (Position    : in     Board_Position_Type;
                                dx, dy      : in     Integer;
                                Stop_Short  : in     Boolean      := False;
                                Capture     : in     Capture_Type := True)
                               return Move_Vectors.Vector is
      
      Move_List : Move_Vectors.Vector;
      
      Tmp_dy,
      Loop_dx,
      Loop_dy   : Integer;
      
   begin
      
      Loop_dx := dx;
      Loop_dy := dy;
      
      for I in 1 .. 4 loop
         
         Move_List := Move_Vectors."&" (Move_List, 
                                        Move_Scan (Position, Loop_dx, Loop_dy,
                                                   Stop_Short, Capture));
         
         Tmp_dy  := Loop_dx;
         Loop_dx := Loop_dy;
         Loop_dy := - Tmp_dy;
         
      end loop;
      
      return Move_List;
      
   end Move_Symmetry_Scan;
   

   ----------------------------------------------------------------------------
   --  procedure Play_Game is
      
   --     Game_Won    : Boolean := False;
      
   --     Move_Cursor : Move_Vectors.Cursor;
   --     Move_List   : Move_Vectors.Vector;
      
   --     --  Next_Board  : Board_State_Type;
      
   --  begin
      
   --     Move_List := Move_Scan;
      
   --     Move_Cursor := Move_Vectors.First (Move_List);
      
   --     if (not Move_Vectors.Has_Element (Move_Cursor)) then
         
   --        Game_Won := False;
         
   --     else
         
   --    Check_Moves:
         
   --        while (Move_Vectors.Has_Element (Move_Cursor) and (not Game_Won)) loop
            
   --           Move_Piece (Move_Vectors.Element (Move_Cursor));
            
   --           --  if (Evaluate_Win (Board_State.Side_On_Move,
   --           --                    Next_Board) = True) then
            
   --           --     Game_Won := True;
            
   --           --  elsif (Next_Move (Next_Board) = -1) then  -- Negamax inversion here
            
   --           --     Game_Won := True;
   --           --     exit Check_moves;
            
   --           --  end if;
            
   --           Move_Vectors.Next (Move_Cursor);   
            
   --        end loop Check_Moves;
         
   --     end if;
      
   --     if (Game_Won) then
         
   --        Put_Line ("1");
         
   --     else
         
   --        Put_Line ("-1");
         
   --     end if;
      
   --  end Play_Game;
   
   
   --  ----------------------------------------------------------------------------
   --  function Next_Move return Integer is
      
   --     Game_Won    : Boolean := False;
      
   --     Max_Value   : Integer := -1;
   --     --  Next_Value  : Integer;
      
   --     Move_Cursor : Move_Vectors.Cursor;
   --     Move_List   : Move_Vectors.Vector;
      
   --     --  Next_Board  : Board_State_Type;
      
   --  begin
      
   --     Move_List := Move_Scan;
      
   --     Move_Cursor := Move_Vectors.First (Move_List);
      
   --     if (not Move_Vectors.Has_Element (Move_Cursor)) then
         
   --        return -1;
         
   --     else
         
   --        while (Move_Vectors.Has_Element (Move_Cursor)) loop
            
   --           Move_Piece (Move_Vectors.Element (Move_Cursor));
            
   --           --  if (Evaluate_Win (Board_State.Side_On_Move,
   --           --                    Next_Board) = True) then
            
   --           --     return 1;
            
   --           --  else
            
   --           --     Next_Value := -(Next_Move (Next_Board));  -- Negamax!
               
   --           --     if (Next_Value > Max_Value) then
                  
   --           --        Max_Value := Next_Value;
                  
   --           --     end if;

   --           --  end if;
            
   --           Move_Vectors.Next (Move_Cursor);
            
   --        end loop;
         
   --        return Max_Value;
         
   --     end if;
            
   --  end Next_Move;
    
   
   ----------------------------------------------------------------------------
   procedure Print_Board is
      
   begin
      
      Put      (Standard_Output, Board_State.Turn_Counter, 0);
      Put      (Standard_Output, " ");
      Put_Line (Standard_Output,
                Side_On_Move_Type'Image (Board_State.Side_On_Move));
      
      for R in reverse Board_Row_Type'Range loop
         
         for C in Board_Column_Type'Range loop
            
            Put (Standard_Error, Board_State.Board_Array(R, C));
            
         end loop;  -- Row
         
         New_Line (Standard_Error);
         
      end loop;  -- Col
      
      New_Line (Standard_Error);
      
   end Print_Board;
   
   
   ----------------------------------------------------------------------------
   procedure Print_Move (Piece : in     Character;
                         Move  : in     Move_Type) is
      
   begin
      
      Put (Piece);
      Put (" ");
      Print_Position (Move.From);
      Put ('-');
      Print_Position (Move.To);
      
      if (Move.Capture /= '.') then
         
         Put (" captures " & Move.Capture);
         
      end if;
      
   end Print_Move;
   
   
   ----------------------------------------------------------------------------
   procedure Print_Move_List (Move_List : in     Move_Vectors.Vector) is
      
      Cursor : Move_Vectors.Cursor;
      Move   : Move_Type;
      
   begin
      
      Cursor := Move_Vectors.First (Move_List);
      
      while (Move_Vectors.Has_Element (Cursor)) loop
         
         Move := Move_Vectors.Element (Cursor);
         
         Print_Move (Board_State.Board_Array(Move.From.R, Move.From.C),
                                             Move);
         
         New_Line (1);
         
         Cursor := Move_Vectors.Next (Cursor);
         
      end loop;
      
      New_Line;
      
   end Print_Move_List;
   
   
   ----------------------------------------------------------------------------
   procedure Print_Position (Position : in     Board_Position_Type) is
       
   begin
      
      Put (Standard_Output, To_Lower (Board_Column_Type'Image (Position.C)));
      Put (Standard_Output, Position.R, 0);
      
   end;
   
   
   ----------------------------------------------------------------------------
   procedure Print_Position_Lists is
      
      Position_Cursor : Position_Vectors.Cursor;
      Position        : Board_Position_Type;
      
   begin
      
      Position_Cursor := Position_Vectors.First (White_Positions);
      
      while (Position_Vectors.Has_Element (Position_Cursor)) loop
         
         Position := Position_Vectors.Element (Position_Cursor);
         
         Put (Board_State.Board_Array (Position.R, Position.C) & " ");
         
         Print_Position (Position);
         
         Put ("; ");
         
         Position_Cursor := Position_Vectors.Next (Position_Cursor);
         
      end loop;
      
      New_Line;
      
      Position_Cursor := Position_Vectors.First (Black_Positions);
      
      while (Position_Vectors.Has_Element (Position_Cursor)) loop
         
         Position := Position_Vectors.Element (Position_Cursor);
         
         Put (Board_State.Board_Array (Position.R, Position.C) & " ");
         
         Print_Position (Position);
         
         Put ("; ");
         
         Position_Cursor := Position_Vectors.Next (Position_Cursor);
         
      end loop;
      
      New_Line (2);
      
   end Print_Position_Lists;
   
   
   ----------------------------------------------------------------------------
   procedure Undo_Move is
      
      Old_Move : Move_Type;
      
   begin
      
      if (Board_State.Turn_Counter = 1) and then 
        (Board_State.Side_On_Move = W) then
         
         Put_Line (Standard_Error, "Attempting to undo with no moves in log");
         
         New_Line;
         
      else
         
         Old_Move := Move_Vectors.Last_Element (Game_Log);
         
         -- Move the piece back to its origin, and replace the captured piece
         
         Board_State.Board_Array (Old_Move.From.R, Old_Move.From.C) :=
           Board_State.Board_Array (Old_Move.To.R, Old_Move.To.C);
         
         Board_State.Board_Array (Old_Move.To.R, Old_Move.To.C) :=
           Old_Move.Capture;
         
         -- Remove the move from the game log
         
         Move_Vectors.Delete_Last (Game_Log);
         
         -- Update the move counter and side on move
         
         if (Board_State.Side_On_Move = W) then
            
            Board_State.Turn_Counter := Board_State.Turn_Counter - 1;
            Board_State.Side_On_Move := B;
            
         else
            
            Board_State.Side_On_Move := W;
            
         end if;
         
      end if;
      
   end;   
   
   
end Board;
