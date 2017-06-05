-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- board.adb
--
-------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Text_IO;             use Ada.Text_IO;

package body Board is
   
   Illegal_Move    : exception;

   ----------------------------------------------------------------------------
   procedure Initialize_Game is
      
   begin
      
      White_Piece_Set := To_Set ("BKNPRQ");
      Black_Piece_Set := To_Set ("bknprq");
      
      Game_State.Turn_Counter := 1;
      Game_State.Side_On_Move := W;
      
      Game_State.Board_Array := (6 => ('k', 'q', 'b', 'n', 'r'),
                                 5 => ('p', 'p', 'p', 'p', 'p'),
                                 4 => ('.', '.', '.', '.', '.'),
                                 3 => ('.', '.', '.', '.', '.'),
                                 2 => ('P', 'P', 'P', 'P', 'P'),
                                 1 => ('R', 'N', 'B', 'Q', 'K'));
      
      -- Scan the board and populate the position vectors for each player
      
      for R in Board_Row_Type'Range loop
         
         for C in Board_Column_Type'Range loop
            
            if (Is_In (Game_State.Board_Array(R, C), White_Piece_Set)) then
               
               Position_Vectors.Append (Game_State.White_Positions, (R, C));
               
            end if;
            
            if (Is_In (Game_State.Board_Array(R,C), Black_Piece_Set)) then
               
               Position_Vectors.Append (Game_State.Black_Positions, (R, C));
               
            end if;
            
         end loop;
         
      end loop;
      
      -- Set initial score
      
      Evaluate_Score (Game_State);
      
   end;
   
   
   ----------------------------------------------------------------------------
   procedure Evaluate_Score (State : in out Game_State_Type) is
      
      Cursor       : Position_Vectors.Cursor;
      Position     : Board_Position_Type;
      
      White_Score,
      Black_Score  : Integer := 0;
      
   begin
      
      State.Score := 0;
      
      Cursor := Position_Vectors.First (State.White_Positions);
      
      while (Position_Vectors.Has_Element (Cursor)) loop
         
         Position := Position_Vectors.Element (Cursor);
         
         case State.Board_Array(Position.R, Position.C) is
            
            when 'K' => 
               White_Score := White_Score + 1000;
               
            when 'Q' => 
               White_Score := White_Score + 900;
               
            when 'R' => 
               White_Score := White_Score + 500;
               
            when 'B' => 
               White_Score := White_Score + 300;
               
            when 'N' => 
               White_Score := White_Score + 300;
               
            when 'P' => 
               White_Score := White_Score + 100;
               
            when others => 
               Print_Move_List (State.Move_Log);
               Print_Board (State);
               Print_Position_Lists (State);
               Put      ("Position ");
               Print_Position (Position);
               Put_Line (" is marked '" & State.Board_Array(Position.R, Position.C) & "'");
               raise Illegal_Move with
                 "Invalid piece in score evaluation for white";
               
         end case;
         
         Position_Vectors.Next (Cursor);
         
      end loop;
      
      Cursor := Position_Vectors.First (State.Black_Positions);
      
      while (Position_Vectors.Has_Element (Cursor)) loop
         
         Position := Position_Vectors.Element (Cursor);
         
         case State.Board_Array(Position.R, Position.C) is
            
            when 'k' => 
               Black_Score := Black_Score + 1000;
               
            when 'q' => 
               Black_Score := Black_Score + 900;
               
            when 'r' => 
               Black_Score := Black_Score + 500;
               
            when 'b' => 
               Black_Score := Black_Score + 300;
               
            when 'n' => 
               Black_Score := Black_Score + 300;
               
            when 'p' => 
               Black_Score := Black_Score + 100;
               
            when others => 
               Print_Move_List (State.Move_Log);
               Print_Board (State);
               Print_Position_Lists (State);
               Put      ("Position ");
               Print_Position (Position);
               Put_Line (" is marked '" & State.Board_Array(Position.R, Position.C) & "'");
               raise Illegal_Move with
                 "Invalid piece in score evaluation for black";
               
         end case;
         
         Position_Vectors.Next (Cursor);
         
      end loop;
      
      if (State.Side_On_Move = W) then
         
         if (State.White_King_In_Play = True) then
            
            State.Score := White_Score - Black_Score;
            
         else
            
            State.Score := -10_000;
            
         end if;
         
         if (State.Black_King_In_Play = False) then
            
            State.Score := 10_000;
            
         end if;
         
      else
         
         if (State.Black_King_In_Play = True) then
            
            State.Score := Black_Score - White_Score;
            
         else
            
            State.Score := -10_000;
            
         end if;
         
         if (State.White_King_In_Play = False) then
            
            State.Score := 10_000;
            
         end if;
         
      end if;
      
   end Evaluate_Score;
   
   
   ----------------------------------------------------------------------------
   function Is_Greater (Left, Right : in     Move_Type) return Boolean is
      
      -- Used in instantiating the generic sort package for Move_Vectors
      --   to sort moves in the vector by their scores. This is the function
      --   that Move_Vectors.Generic_Sorting."<" will use to compare two moves.
      --   Using this function to sort a Move_Vectors container will result in
      --   the list being sorted from highest to lowest scores
      
   begin
      
      return (Left.Score > Right.Score);
      
   end Is_Greater;   
   
   
   ----------------------------------------------------------------------------
   function Move_Generator (State         : in     Game_State_Type;
                            Position_List : in     Position_Vectors.vector)
                           return Move_Vectors.Vector is
      
      Cursor     : Position_Vectors.Cursor;
      Move_List  : Move_Vectors.Vector;
      Position   : Board_Position_Type; 
      Stop_Short : Boolean                 := False;
      Capture    : Capture_Type            := False;
      
   begin
      
      Cursor := Position_Vectors.First (Position_List);
      
      while (Position_Vectors.Has_Element (Cursor)) loop
         
         Position := Position_Vectors.Element (Cursor);
         
         case State.Board_Array(Position.R, Position.C) is
            
            when 'K' | 'k' =>
               
               Stop_Short := True;
               Capture    := True;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Symmetry_Scan
                                               (State, Position, 1, 0,
                                                Stop_Short, Capture));
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Symmetry_Scan
                                               (State, Position, 1, 1,
                                                Stop_Short, Capture));
               
            when 'Q' | 'q' =>
               
               Stop_Short := False;
               Capture    := True;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Symmetry_Scan
                                               (State, Position, 1, 0,
                                                Stop_Short, Capture));
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Symmetry_Scan
                                               (State, Position, 1, 1,
                                                Stop_Short, Capture));
            when 'R' | 'r' =>
               
               Stop_Short := false;
               Capture    := True;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Symmetry_Scan
                                               (State, Position, 1, 0,
                                                Stop_Short, Capture));
               
            when 'B' | 'b' =>
               
               Stop_Short := True;
               Capture    := False;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Symmetry_Scan
                                               (State, Position, 1, 0,
                                                Stop_Short, Capture));
               Stop_Short := False;
               Capture    := True;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Symmetry_Scan
                                               (State, Position, 1, 1,
                                                Stop_Short, Capture)); 
               
            when 'N' | 'n' =>
               
               Stop_Short := True;
               Capture    := True;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Symmetry_Scan
                                               (State, Position, 2, 1,
                                                Stop_Short, Capture));
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Symmetry_Scan
                                               (State, Position, 2, -1,
                                                Stop_Short, Capture));
               
            when 'P' =>
               
               Stop_Short := True;
               Capture    := Only;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Scan
                                               (State, Position, 1, -1,
                                                Stop_Short, Capture));
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Scan
                                               (State, Position, 1, 1,
                                                Stop_Short, Capture));
               Capture    := False;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Scan
                                               (State, Position, 1, 0,
                                                Stop_Short, Capture));
               
            when 'p' =>
               
               Stop_Short := True;
               Capture    := Only;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Scan
                                               (State, Position, -1, -1,
                                                Stop_Short, Capture));
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Scan
                                               (State, Position, -1, 1,
                                                Stop_Short, Capture));
               Capture    := False;
               Move_List  := Move_Vectors."&" (Move_List,
                                               Move_Scan
                                               (State, Position, -1, 0,
                                                Stop_Short, Capture));
               
            when others =>
               
               Print_Move_List (State.Move_Log);
               Print_Board (State);
               Print_Position_Lists (State);
               Put      ("Position ");
               Print_Position (Position);
               Put_Line (" is marked '" & State.Board_Array(Position.R, Position.C) & "'");
               raise Illegal_Move with "Invalid piece in move scan";
               
         end case;
         
         Position_Vectors.Next (Cursor);
         
      end loop;
      
      return Move_List;
      
   end Move_Generator;
   
   
   ----------------------------------------------------------------------------
   procedure Move_Piece (State : in out Game_State_Type;
                         Move  : in out Move_Type) is
      
      Cursor : Position_Vectors.Cursor;
      
   begin
      
      -- Check that we're moving a piece that belongs to the side on move,
      --   then check that the captured piece (including '.' for simplicity)
      --   isn't owned by the side on move. If either condition isn't true,
      --   then throw an exception. Then, update the positions list for the
      --   side on move and the "capture piece". If an actual piece was
      --   captured, remove it from the opposing player's position list.
      
      if (State.Side_On_Move = W) then
         
         if (not Is_In (Move.piece, White_Piece_Set)) then
            
            Print_Move_List (State.Move_Log);
            Print_Board (State);
            Print_Position_Lists (State);
            Put ("White attempting move ");
            Print_Move (Move);
            New_Line;
            Put      ("Position ");
            Print_Position (Move.From);
            Put_Line (" is marked '" & State.Board_Array(Move.From.R, Move.From.C) & "'");
            raise Illegal_Move with "Origin piece not owned by side on move";
            
         end if;
         
         if (Is_In (Move.Capture, White_Piece_Set)) then
            
            Print_Move_List (State.Move_Log);
            Print_Board (State);
            Print_Position_Lists (State);
            Put ("White attempting move ");
            Print_Move (Move);
            New_Line;
            Put      ("Position ");
            Print_Position (Move.To);
            Put_Line (" is marked '" & State.Board_Array(Move.To.R, Move.To.C) & "'");
            raise Illegal_Move with "White attempting to capture own piece";
            
         end if;
         
         -- Update the positions lists
         
         Cursor := Position_Vectors.Find  (State.White_Positions,
                                           Move.From);
         Position_Vectors.Replace_Element (State.White_Positions,
                                           Cursor,
                                           Move.To);
         
         Move.Capture := State.Board_Array(Move.To.R, Move.To.C);
         
         if (Move.Capture /= '.') then
            
            if (Move.Capture = 'k') then
               
               State.Black_King_In_Play := False;
               
            end if;
            
            Cursor := Position_Vectors.Find (State.Black_Positions,
                                             Move.To);
            Position_Vectors.Delete         (State.Black_Positions,
                                             Cursor);
         end if;
         
         -- Change the side on move
         
         State.Side_On_Move := B;
         
      else
         
         if (not Is_In (Move.Piece, Black_Piece_Set)) then
            
            Print_Move_List (State.Move_Log);
            Print_Board (State);
            Print_Position_Lists (State);
            Put ("Black attempting move ");
            Print_Move (Move);
            New_Line;
            Put      ("Position ");
            Print_Position (Move.From);
            Put_Line (" is marked '" & State.Board_Array(Move.From.R, Move.From.C) & "'");
            raise Illegal_Move with "Origin piece not owned by side on move";
            
         end if;
         
         if (Is_In (Move.Capture, Black_Piece_Set)) then
            
            Print_Move_List (State.Move_Log);
            Print_Board (State);
            Print_Position_Lists (State);
            Put ("Black attempting move ");
            Print_Move (Move);
            New_Line;
            Put      ("Position ");
            Print_Position (Move.To);
            Put_Line (" is marked '" & State.Board_Array(Move.To.R, Move.To.C) & "'");
            raise Illegal_Move with "Black attempting to capture own piece";
            
         end if;
         
         -- Update the positions lists
         
         Cursor := Position_Vectors.Find  (State.Black_Positions,
                                           Move.From);
         Position_Vectors.Replace_Element (State.Black_Positions,
                                           Cursor,
                                           Move.To);
         
         Move.Capture := State.Board_Array(Move.To.R, Move.To.C);
         
         if (Move.Capture /= '.') then
            
            if (Move.Capture = 'K') then
               
               State.White_King_In_Play := False;
               
            end if;
            
            Cursor := Position_Vectors.Find (State.White_Positions,
                                             Move.To);
            Position_Vectors.Delete         (State.White_Positions,
                                             Cursor);
         end if;
         
         -- Increment the turn counter and change the side on move.
         
         State.Turn_Counter := State.Turn_Counter + 1;
         State.Side_On_Move := W;
         
      end if;
      
      -- Log the move in the running game log
      
      Move_Vectors.Append (State.Move_Log, Move);
      
      -- Move the piece to the new position and clear the old position
      
      -- Promote pawns
      
      if ((Move.Piece = 'P') and (Move.To.R = 6)) then
         
         State.Board_Array(Move.To.R, Move.To.C) := 'Q';
         
      elsif ((Move.Piece = 'p') and (Move.To.R = 1)) then
         
         State.Board_Array(Move.To.R, Move.To.C) := 'q';
         
      else
         
         State.Board_Array(Move.To.R, Move.To.C) :=
           State.Board_Array(Move.From.R, Move.From.C);
         
      end if;
            
      State.Board_Array(Move.From.R, Move.From.C) := '.';
      
      Evaluate_Score (State);
      
   end Move_Piece;


   ----------------------------------------------------------------------------
   procedure Move_Piece (State       : in out Game_State_Type;
                         Move_String : in     String) is
      
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
      
      Move.Piece   := State.Board_Array(Move.From.R, Move.From.C);
      Move.Capture := State.Board_Array(Move.To.R,   Move.To.C);
      
      Move_Piece (Game_State, Move);
      
   end;
   
   
   ----------------------------------------------------------------------------
   function Move_Scan (State      : in     Game_State_Type;
                       Position   : in     Board_Position_Type;
                       dx, dy     : in     Integer;
                       Stop_Short : in     Boolean      := False;
                       Capture    : in     Capture_Type := True)
                      return Move_Vectors.Vector is
      
      Move_List : Move_Vectors.Vector;
      X, Y      : Integer;
      Stop_Flag : Boolean := Stop_Short;
      Piece     : Character;             -- Piece being moved
      Cap_Piece : Character;             -- The contents of 
                                         --   Board_Array(X + dx, Y + dy)

   begin
      
      -- Convert the board position to a pair of integers (X, Y) so we can
      --   math on them more easily (particulary for checking our move won't
      --   take us off the board without throwing an exception)
      
      X     := Position.R;
      Y     := Board_Column_Type'Pos (Position.C);
      
      Piece := State.Board_Array(X, Board_Column_Type'Val (Y));
      
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
         
         Cap_Piece := State.Board_Array(X, Board_Column_Type'Val (Y));
         
         if (Cap_Piece /= '.') then                         -- We found a piece
            
            if (State.Side_On_Move = W) then                -- Check to see if it's
                                                            --   one of ours. If so
               if (Is_In (Cap_Piece, White_Piece_Set)) then --   exit the scan loop
                  
                  exit Scan_Loop;
                  
               end if;
               
            else
               
               if (Is_In (Cap_Piece, Black_Piece_Set)) then
                  
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
                               Piece, Cap_Piece, Integer'First + 1));         
         
         exit Scan_Loop when (Stop_Flag = True);
         
      end loop Scan_Loop;
      
      return Move_List;
      
   end Move_Scan;
   
   
   ----------------------------------------------------------------------------
   function Move_Symmetry_Scan (State       : in     Game_State_Type;
                                Position    : in     Board_Position_Type;
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
                                        Move_Scan (State,
                                                   Position,
                                                   Loop_dx, Loop_dy,
                                                   Stop_Short, Capture));
         
         Tmp_dy  := Loop_dx;
         Loop_dx := Loop_dy;
         Loop_dy := - Tmp_dy;
         
      end loop;
      
      return Move_List;
      
   end Move_Symmetry_Scan;
   

   ----------------------------------------------------------------------------
   procedure Print_Board (State : in     Game_State_Type) is
      
   begin
      
      Put      (Standard_Output, State.Turn_Counter, 0);
      Put      (Standard_Output, " ");
      Put_Line (Standard_Output,
                Side_On_Move_Type'Image (State.Side_On_Move));
      
      for R in reverse Board_Row_Type'Range loop
         
         for C in Board_Column_Type'Range loop
            
            Put (Standard_Output, State.Board_Array(R, C));
            
         end loop;  -- Row
         
         New_Line (Standard_Output);
         
      end loop;  -- Col
      
      New_Line (Standard_Output);
      
      Put ("Score: ");
      Put (State.Score, 0);
      New_Line;
      
   end Print_Board;
   
   
   ----------------------------------------------------------------------------
   procedure Print_Move (Move  : in     Move_Type) is
      
   begin
      
      Put (Move.Piece);
      Put (" ");
      Print_Position (Move.From);
      Put ('-');
      Print_Position (Move.To);
      
      if (Move.Capture /= '.') then
         
         Put (" captures " & Move.Capture);
         
      end if;
      
      Put ("  Score ");
      Put (Move.Score, 0);
      
   end Print_Move;
   
   
   ----------------------------------------------------------------------------
   procedure Print_Move_List (Move_List : in     Move_Vectors.Vector) is
      
      Cursor : Move_Vectors.Cursor;
      Move   : Move_Type;
      
   begin
      
      Cursor := Move_Vectors.First (Move_List);
      
      while (Move_Vectors.Has_Element (Cursor)) loop
         
         Move := Move_Vectors.Element (Cursor);
         
         Print_Move (Move);
         
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
   procedure Print_Position_Lists (State : in     Game_State_Type) is
      
      Position_Cursor : Position_Vectors.Cursor;
      Position        : Board_Position_Type;
      
   begin
      
      Position_Cursor := Position_Vectors.First (State.White_Positions);
      
      while (Position_Vectors.Has_Element (Position_Cursor)) loop
         
         Position := Position_Vectors.Element (Position_Cursor);
         
         Put (State.Board_Array(Position.R, Position.C) & " ");
         
         Print_Position (Position);
         
         Put ("; ");
         
         Position_Cursor := Position_Vectors.Next (Position_Cursor);
         
      end loop;
      
      New_Line;
      
      Position_Cursor := Position_Vectors.First (State.Black_Positions);
      
      while (Position_Vectors.Has_Element (Position_Cursor)) loop
         
         Position := Position_Vectors.Element (Position_Cursor);
         
         Put (State.Board_Array (Position.R, Position.C) & " ");
         
         Print_Position (Position);
         
         Put ("; ");
         
         Position_Cursor := Position_Vectors.Next (Position_Cursor);
         
      end loop;
      
      New_Line (2);
      
   end Print_Position_Lists;
   
   
   ----------------------------------------------------------------------------
   procedure Undo_Move (State : in out Game_State_Type) is
      
      Old_Move : Move_Type;
      Cursor   : Position_Vectors.Cursor;
      
   begin
      
      if (State.Turn_Counter = 1) and then 
        (State.Side_On_Move = W) then
         
         Put_Line (Standard_Error, "Attempting to undo with no moves in log");
         
         New_Line;
         
      else
         
         Old_Move := Move_Vectors.Last_Element (State.Move_Log);
         
         -- Move the piece back to its origin, and replace the captured piece
         
         --  Put ("Undoing ");
         --  Print_Move (Old_Move);
         --  New_Line;
         
         --  if (Old_Move.To.R = 6) then            
         --     Put ("Old piece is '" & Old_Move.Piece & "'");
         --     Put_line (" Old capture is '" & Old_Move.Capture & "'");
         --     Print_Board (State);
         --     Print_Move_List (State.Move_Log);
         --  end if;
         
         State.Board_Array (Old_Move.From.R, Old_Move.From.C) :=
           Old_Move.Piece;
         
         State.Board_Array (Old_Move.To.R, Old_Move.To.C) :=
           Old_Move.Capture;
         
         -- Remove the move from the game log
         
         Move_Vectors.Delete_Last (State.Move_Log);
         
         -- Update the move counter and side on move, replace captured pieces
         
         if (State.Side_On_Move = W) then
            
            State.Turn_Counter := State.Turn_Counter - 1;
            State.Side_On_Move := B;
            
            -- Update the positions lists. Since SOM is W, B moved last...
            --   Find the Move.To of the last move and update the black
            --   position list to reflect the piece moved to be at
            --   Old_Move.From and if there was a piece captured, but that
            --   position back on white's position list.
            
            Cursor := Position_Vectors.Find  (State.Black_Positions,
                                              Old_Move.To);
            
            if (Position_Vectors.Has_Element (Cursor)) then
               
               Position_Vectors.Replace_Element (State.Black_Positions,
                                                 Cursor,
                                                 Old_Move.From);
               
               if (Old_Move.Capture /= '.') then
                  
                  Position_Vectors.append (State.White_Positions,
                                           Old_Move.To);
               end if;
               
            end if;
            
            if (Old_Move.Capture = 'K') then
               
               State.White_King_In_Play := True;
               
            end if;
            
         else
            
            State.Side_On_Move := W;
            
            -- Update the positions lists
            
            Cursor := Position_Vectors.Find  (State.White_Positions,
                                              Old_Move.To);
            
            if (Position_Vectors.Has_Element (Cursor)) then
               
               Position_Vectors.Replace_Element (State.White_Positions,
                                                 Cursor,
                                                 Old_Move.From);
               
               if (Old_Move.Capture /= '.') then
                  
                  Position_Vectors.append (State.Black_Positions,
                                           Old_Move.To);
               end if;
               
            end if;
            
            if (Old_Move.Capture = 'k') then
               
               State.Black_King_In_Play := True;
               
            end if;
            
         end if;
         
         Evaluate_Score (State);
         
      end if;
      
   end Undo_Move;
   
   
end Board;
