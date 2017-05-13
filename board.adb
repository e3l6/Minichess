-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- board.adb
--
-------------------------------------------------------------------------------

with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Strings.Maps;     use Ada.Strings.Maps;
with Ada.Text_IO;          use Ada.Text_IO;

package body Board is
   
   White_Piece_Set : Character_Set := To_Set ("BKNPRQ");
   Black_Piece_Set : Character_Set := To_Set ("bknprq");

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
   end;
   
   
   ----------------------------------------------------------------------------
   procedure Move_Piece (Move : in     Move_Type) is
      
   begin
      
      -- Check that we're moving a piece that belongs to the side on move,
      --   then check that the captured piece (including '.' for simplicity)
      --   isn't owned by the side on move. If either condition isn't true,
      --   then throw an exception
      
      if (Board_State.Side_On_Move = W) then
         
         if (not Is_In (Board_State.Board_Array(Move.From.R, Move.From.C),
                        White_Piece_Set)) then
            
            raise Illegal_Move with "Origin piece not owned by side on move";
            
         end if;
         
         if (Is_In (Board_State.Board_Array(Move.To.R, Move.To.C),
                    White_Piece_Set)) then
            
            raise Illegal_Move with "White attempting to capture own piece";
            
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
         
         -- Increment the turn counter and change the side on move.
         
         Board_State.Turn_Counter := Board_State.Turn_Counter + 1;
         Board_state.Side_On_Move := W;
         
      end if;
      
      -- Log the move in the running game log
      
      Move_Vectors.Append (Game_Log,
                           (Move.From,
                            Move.To,
                            Board_State.Board_Array(Move.To.R, Move.To.C)));
      
      -- Move the piece to the new position and clear the new position
      
      Board_State.Board_Array(Move.To.R, Move.To.C) :=
        Board_State.Board_Array(Move.From.R, Move.From.C);
      
      Board_State.Board_Array(Move.From.R, Move.From.C) := '.';
      
   end Move_Piece;


   ----------------------------------------------------------------------------
   procedure Move_Piece (Move_String : in     String) is
      
      Move    : Move_Type;
      Element : String (1 .. 1);
      
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
      
   end;
   
   
   ----------------------------------------------------------------------------
   function Move_Scan return Move_Vectors.Vector is
      
      Move_List : Move_Vectors.Vector;
      
   begin
      
      if (Board_State.Side_On_Move = W) then
         
         for R in reverse Board_Row_Type'Range Loop
            
            for C in Board_Column_Type'Range loop
               
               if Board_State.Board_Array(R, C) = 'P' then
                  
                  -- Check for capture left
                  
                  if ((R < Board_Row_Type'Last) and 
                      (C /= Board_Column_Type'First)) then
                     
                     if (Board_State.Board_Array(Board_Row_Type'Succ(R),
                                                 Board_Column_Type'Pred(C))
                         = 'p') then
                        
                        Move_Vectors.append (Move_List,
                                             ((R, C),
                                              (Board_Row_Type'Succ(R),
                                               Board_Column_Type'Pred(C)),
                                              'p'));
                        
                     end if;
                     
                  end if;
                  
                  -- Check for capture right
                  
                  if ((R < Board_Row_Type'Last) and
                      (C < Board_Column_Type'Last)) then
                     
                     if (Board_State.Board_Array(Board_Row_Type'Succ(R),
                                                 Board_Column_Type'Succ(C))
                         = 'p') then
                        
                        Move_Vectors.append (Move_List,
                                             ((R, C),
                                              (Board_Row_Type'Succ(R),
                                               Board_Column_Type'Succ(C)),
                                              'p'));
                        
                     end if;
                     
                  end if;
                  
                  -- Check for advance forward
                  
                  if (R < Board_Row_Type'Last) then
                     if (Board_State.Board_Array(Board_Row_Type'Succ(R), C)
                         = '.') then
                        
                        Move_Vectors.append (Move_List,
                                             ((R, C),
                                              (Board_Row_Type'Succ(R), C),
                                              '.'));
                        
                     end if;
                     
                  end if;
                  
               end if;
               
            end loop;  -- Col
            
         end loop;  -- Row
         
      else  -- Black on move
         
         for R in Board_Row_Type'Range loop
            
            for C in Board_Column_Type'Range loop
               
               if Board_State.Board_Array(R, C) = 'p' then
                  
                  -- Check for capture left
                  
                  if ((R > Board_Row_Type'First) and
                      (C > Board_Column_Type'first)) then
                     
                     if (Board_State.Board_Array(Board_Row_Type'Pred(R),
                                                 Board_Column_Type'Pred(C))
                         = 'P') then
                        
                        Move_Vectors.append (Move_List,
                                             ((R, C),
                                              (Board_Row_Type'Pred(R),
                                               Board_Column_Type'Pred(C)),
                                              'P'));
                        
                     end if;
                     
                  end if;
                  
                  -- Check for capture right
                  
                  if ((R > Board_Row_Type'First) and
                      (C < Board_Column_Type'Last)) then
                     
                     if (Board_State.Board_Array(Board_Row_Type'Pred(R),
                                                 Board_Column_Type'Succ(C))
                         = 'P') then
                        
                        Move_Vectors.append (Move_List,
                                             ((R, C),
                                              (Board_Row_Type'Pred(R),
                                               Board_Column_Type'Succ(C)),
                                              'P'));
                        
                     end if;
                     
                  end if;
                  
                  -- Check for advance forward
                  
                  if (R > 1) then
                     
                     if (Board_State.Board_Array(Board_Row_Type'Pred(R), C)
                         = '.') then
                        
                        Move_Vectors.append (Move_List, 
                                             ((R, C), 
                                              (R - Board_Row_Type'Pred(R),
                                               C),
                                              '.'));
                        
                     end if;
                     
                  end if;
                  
               end if;
               
            end loop;  -- Col
            
         end loop;  -- Row
         
      end if;  -- White/Black on move
      
      return Move_List;
   end Move_Scan;
   
   
   ----------------------------------------------------------------------------
   procedure Play_Game is
      
      Game_Won    : Boolean := False;
      
      Move_Cursor : Move_Vectors.Cursor;
      Move_List   : Move_Vectors.Vector;
      
      --  Next_Board  : Board_State_Type;
      
   begin
      Move_List := Move_Scan;
      
      Move_Cursor := Move_Vectors.First (Move_List);
      
      if (not Move_Vectors.Has_Element (Move_Cursor)) then
         
         Game_Won := False;
         
      else
         
     Check_Moves:
         
         while (Move_Vectors.Has_Element (Move_Cursor) and (not Game_Won)) loop
            
            Move_Piece (Move_Vectors.Element (Move_Cursor));
            
            --  if (Evaluate_Win (Board_State.Side_On_Move,
            --                    Next_Board) = True) then
            
            --     Game_Won := True;
            
            --  elsif (Next_Move (Next_Board) = -1) then  -- Negamax inversion here
            
            --     Game_Won := True;
            --     exit Check_moves;
            
            --  end if;
            
            Move_Vectors.Next (Move_Cursor);   
            
         end loop Check_Moves;
         
      end if;
      
      if (Game_Won) then
         
         Put_Line ("1");
         
      else
         
         Put_Line ("-1");
         
      end if;
      
   end Play_Game;
   
   
   ----------------------------------------------------------------------------
   function Next_Move return Integer is
      
      Game_Won    : Boolean := False;
      
      Max_Value   : Integer := -1;
      --  Next_Value  : Integer;
      
      Move_Cursor : Move_Vectors.Cursor;
      Move_List   : Move_Vectors.Vector;
      
      --  Next_Board  : Board_State_Type;
      
   begin
      
      Move_List := Move_Scan;
      
      Move_Cursor := Move_Vectors.First (Move_List);
      
      if (not Move_Vectors.Has_Element (Move_Cursor)) then
         
         return -1;
         
      else
         
         while (Move_Vectors.Has_Element (Move_Cursor)) loop
            
            Move_Piece (Move_Vectors.Element (Move_Cursor));
            
            --  if (Evaluate_Win (Board_State.Side_On_Move,
            --                    Next_Board) = True) then
            
            --     return 1;
            
            --  else
            
            --     Next_Value := -(Next_Move (Next_Board));  -- Negamax!
               
            --     if (Next_Value > Max_Value) then
                  
            --        Max_Value := Next_Value;
                  
            --     end if;

            --  end if;
            
            Move_Vectors.Next (Move_Cursor);
            
         end loop;
         
         return Max_Value;
         
      end if;
            
   end Next_Move;
   
   
   ----------------------------------------------------------------------------
   procedure Print_Board is
      
   begin
      
      Put      (Standard_Output, Board_State.Turn_Counter, 0);
      Put      (Standard_Output, " ");
      Put_line (Standard_Output,
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
   procedure Undo_Move is
      
      Old_Move : Move_Type;
      
   begin
      
      if (Board_State.Turn_Counter = 1) and then 
        (Board_State.Side_On_Move = W) then
         
         Put_Line (Standard_Error, "Attempting to undo with no moves in log");
         
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
