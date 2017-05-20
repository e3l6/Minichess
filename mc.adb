-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- mc.adb
--
-- Some concepts (Key_Symbol in particular) borrowed from Bart Massey's
--   from his example minichess player minime2-net.c found at
--   https://github.com/BartMassey/imcs/tree/master/client/c as of
--   20 May 2017.
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
   
   Best_Move     : Move_Type;
   Move_List     : Move_Vectors.Vector;
   Move_Cursor   : Move_Vectors.Cursor;
   
   Best_Score,
   Nega_Score    : Integer := Integer'First + 1;
   
   Alpha         : Integer := Integer'First + 1; 
   Beta          : Integer := Integer'Last;
   
   Response      : Integer;
   
   Im_On_Move    : Boolean := False;
   Key_Symbol    : Character         := ' ';
   Move_Command  : String (1 .. 5);
   My_Side       : Side_On_Move_Type;
   Negamax_Score : Integer;
   Response_Str  : Unbounded_String  := Null_Unbounded_String;
   
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
   
Game_Loop :
    loop
       
       -- Check to see if the game is concluded
       
       exit when (Key_Symbol = '=');
       
       
       if (To_String (Head (Response_Str, 1)) = "!") then
          
          Im_On_Move := False;
          Move_Piece (Game_State, To_String (Tail (Head (Response_Str, 7), 5)));
          
       end if;
       
       
       if (Key_Symbol = '?') then
          
          if ((My_Side = W) and (Game_State.Side_On_Move = W)) then
             
             Im_On_Move := True;
             
          elsif ((My_Side = B) and (Game_State.Side_On_Move = B)) then
             
             Im_On_Move := True;
                          
          end if; -- Key_Symbol = '?'
          
       end if;             
       
       
       if (Im_On_Move = True) then
          
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
       
    end loop Game_Loop;
    
    
    -- Quit the server and close the connections
    
    Put_Line (Standard_Error, "Quitting game");
    
    Sendcmd ("quit");
    
    Shutdown;
    
end MC;
