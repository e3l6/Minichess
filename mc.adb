-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- mc.adb
--
-------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Board;                   use Board;
with Negamax;                 use Negamax;
with Netops;                  use Netops;

procedure MC is
   
   Move_List    : Move_Vectors.Vector;
   Move_Cursor  : Move_Vectors.Cursor;
   Best_Move    : Move_Type;
   Move         : Move_Type;
   
   Best_Score,
   Nega_Score   : Integer := Integer'First + 1;
   
   Alpha        : Integer := Integer'First + 1; 
   Beta         : Integer := Integer'Last;
   
   Position     : Board_Position_Type;
   
   Response     : Integer;
   
   My_Side      : Side_On_Move_Type;
   Im_On_Move   : Boolean := False;
   Response_Str : Unbounded_String  := Null_Unbounded_String;
   Move_Command : String (1 .. 5);
   Key_Symbol   : Character         := ' ';
   Negamax_Score: Integer;
   
begin
   
   if (Argument_Count /= 3) then
      
      Put_Line ("Usage: mc <Game_ID> <Side> <Password>");
      return;
      
   end if;
   
   
   -- Set up game board
   
   Initialize_Game;
   
   
   -- Connect to server and initiate game
   
   if (Initialize (Argument (3)) /= True) then
      
      Put_Line ("Unable to initialize connection with game server");
      return;
      
   end if;
   
   Sendcmd ("accept " & Argument (1) & " " & Argument (2));
   
   Response := Expectcmd;
   
   if ((Response /= 105) and (Response /= 106)) then
      
      Put_Line ("Unexpected server response");
      return;
      
   end if;
      
   if (Argument (2) = "W") then
      My_Side := W;
      Put_Line (Standard_Error, "I am white");
   else
      My_Side := B;
      Put_Line (Standard_Error, "I am black");
   end if;
   
   
   Response_Str := Getnet (Key_Symbol);
   --  Put_Line ("Key symbol = " & Key_Symbol);
   --  Put_Line (">> " & To_String (Response_Str) & " <<");
   
Game_Loop :
    loop
       
       -- Check to see if the game is concluded
       
       exit when (Key_Symbol = '=');
       
       --  if (Key_Symbol = '=') then
       --     Put_Line (Standard_Error, "Exiting game loop");
       --     exit Game_Loop;
       --  end if;
       
       
       if (To_String (Head (Response_Str, 1)) = "!") then
          
          Im_On_Move := False;
          
          --  Put      (Standard_Error, "Applying opponent move: ");
          --  Put_Line (Standard_Error, """" & 
          --            To_String (Tail (Head (Response_Str, 7), 5)) & """");
          
          Move_Piece (Game_State, To_String (Tail (Head (Response_Str, 7), 5)));
          
          --  Put_Line (Standard_Error, "Opponent move applied");
          
       end if;
       
       
       if (Key_Symbol = '?') then
          
          if ((My_Side = W) and (Game_State.Side_On_Move = W)) then
             
             --  Put_Line   (Standard_Error, "I'm white and on move. Constructing move_list");
             --  Move_List  := Move_Generator (Game_State, Game_State.White_Positions);
             Im_On_Move := True;
             
             --  Put_Line   (Standard_Error, "Move list complete");
             
          elsif ((My_Side = B) and (Game_State.Side_On_Move = B)) then
             
             --  Put_Line   (Standard_Error, "I'm Black and on move. Constructing move_list");
             --  Move_List  := Move_Generator (Game_State, Game_State.Black_Positions);
             Im_On_Move := True;
             
             --  Put_Line   (Standard_Error, "Move list complete");
             
          end if; -- Key_Symbol = '?'
          
       --  elsif (To_String (Head (Response_Str, 1)) = "!" then
          
       --     Im_On_Move := False;
          
       --     Put      (Standard_Error, "Applying opponent move: ");
       --     Put_Line (Standard_Error, """" & 
       --               To_String (Tail (Head (Response_Str, 7), 5)) & """");
          
       --     Move_Piece (Game_State, To_String (Tail (Head (Response_Str, 7), 5)));
          
       --     Put_Line (Standard_Error, "Opponent move applied");
          
       --  else
          
       --     Put_Line ("Unknow server response with key symbol = " & Key_Symbol);
          
       end if;             
       
       
       if (Im_On_Move = True) then
          
      --      -- Find the best move for me
          
      --      Move_Cursor := Move_Vectors.First (Move_List);
      --      Best_Score  := Game_State.Score;
      --      Best_Move   := Move_Vectors.Element (Move_Cursor);
          
      --  Traverse_Move_List :
      --      while Move_Vectors.Has_Element (Move_Cursor) loop
             
      --         Move := Move_Vectors.Element (Move_Cursor);
             
      --         Move_Piece (Game_State, Move);
             
      --         Nega_Score := - Negamax.Negamax (Game_State, Max_Depth,
      --                                          Integer'First + 1, Integer'Last);
             
      --         if (Nega_Score > Best_Score) then
                
      --            Best_Move  := Move;
      --            Best_Score := Nega_Score;
                
      --         end if;
             
      --         Undo_Move (Game_State);
             
      --         Move_Cursor := Move_Vectors.Next (Move_Cursor);
             
      --      end loop Traverse_Move_List;
          
          
          --  Put_Line (Standard_Error, "Initiating negamax search for " &
          --            Side_On_Move_Type'Image (Game_State.Side_On_Move));
          
          Negamax_Score := Negamax.Negamax (Game_State, Max_Depth,
                                            Integer'First + 1, Integer'Last,
                                            Best_Move);
          
          -- Apply best move
          
          Move_Command := To_Lower (Board_Column_Type'Image (Best_Move.From.C)) &
            Trim (Board_Row_Type'Image (Best_Move.From.R), Both) & "-" &
            To_Lower (Board_Column_Type'Image (Best_Move.to.C)) &
            Trim (Board_Row_Type'Image (Best_Move.To.R), Both);
          
          --  Put_Line (Standard_Error, "I'm moving " & Move_Command);
          
          Sendcmd (Move_Command);
          
          Move_Piece (Game_State, Move_Command);
          
          --  Move_Vectors.Clear (Move_List);
          
       end if;
       
       Im_On_Move := False;
       
       Response_Str := Getnet (Key_Symbol);
       --  Put_Line ("Key symbol = " & Key_Symbol);
       
    end loop Game_Loop;
    
    
    -- Quit the server and close the connections
    
    Put_Line (Standard_Error, "Quitting game");
    
    Sendcmd ("quit");
    
    Shutdown;
    
end MC;
