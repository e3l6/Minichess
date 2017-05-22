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
   
   Best_Move      : Move_Type;
   Iterative_Move : Move_Type;
   Move_List      : Move_Vectors.Vector;
   Move_Cursor    : Move_Vectors.Cursor;
   
   Best_Score,
   Nega_Score     : Integer := Integer'First + 1;
   
   Best_Depth     : Integer := 0;
   
   Alpha          : Integer := Integer'First + 1; 
   Beta           : Integer := Integer'Last;
   
   Response       : Integer;
   
   Im_On_Move     : Boolean := False;
   Key_Symbol     : Character         := ' ';
   Move_Command   : String (1 .. 5);
   My_Side        : Side_On_Move_Type;
   Negamax_Score  : Integer;
   Response_Str   : Unbounded_String  := Null_Unbounded_String;
   
begin
   
   if (Argument_Count < 4) then
      Put      ("Usage: mc <Username> <Password> ");
      Put_Line ("{-a <Game_ID> <Side> | -o <W|B>} [-d <Max Depth>]");
      return;
   end if;
   
   
   -- Set up game board
   
   Initialize_Game;
   
   
   if (Argument_Count > 5) then
      Negamax.Max_Depth := Integer'Value (Argument (Argument_Count));
   end if;
   
   Put ("Max search depth set to ");
   Put (Negamax.Max_Depth, 0);
   New_Line;
   
   
   -- Connect to server and initiate game
   
   if (Initialize (Argument (1), Argument (2)) /= True) then
      
      Put_Line ("Unable to initialize connection with game server");
      return;
      
   end if;
   
   if (Argument(3) = "-a") then
      
      Sendcmd ("accept " & Argument (4) & " " & Argument (5));
      
      Response := Expectcmd;
      
      if ((Response /= 105) and (Response /= 106)) then
         
         Put_Line ("Unexpected server response");
         return;
         
      end if;
      
      if (Argument (5) = "W") then
         My_Side := W;
         Put_Line (Standard_Error, "I am white");
      else
         My_Side := B;
         Put_Line (Standard_Error, "I am black");
      end if;
      
   elsif (Argument (3) = "-o") then
      
      Sendcmd ("offer " & Argument (4));
      
      Response := Expectcmd;
      
      while ((Response /= 105) and (Response /= 106)) loop
         Response := Expectcmd;
      end loop;
      
      if (Argument (4) = "W") then
         My_Side := W;
         Put_Line (Standard_Error, "I am white");
      else
         My_Side := B;
         Put_Line (Standard_Error, "I am black");
      end if;
      
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
          
          --  Print_Position_Lists (Game_State);
          
          Best_Score := Negamax.Negamax (Game_State, 1, 1,
                                         Integer'First + 1, Integer'Last,
                                         Best_Move, Best_Depth);
          
          --  Put ("Depth 1 computed, best move is ");
          --  Print_Move (Best_Move);
          --  Put (" score ");
          --  Put (Best_Score);
          --  New_Line;
          
          if (Best_Score /= 10_000) then
             
         Iterative_Loop :
             for I in 2 .. Max_Depth loop
                
                Negamax_Score := Negamax.Negamax (Game_State, I, I,
                                                  Integer'First + 1,
                                                  Integer'Last, Iterative_Move,
                                                  Best_Depth);
                
                --  Put ("Depth ");
                --  Put (I, 0);
                --  Put (" computed, iterative move is ");
                --  Print_Move (Iterative_Move);
                --  Put (" score ");
                --  Put (Negamax_Score);
                
                if (Nega_Score = -10_000) then
                   
                   --  New_Line;
                   exit Iterative_Loop;
                   
                elsif (Best_Score = 10_000) then
                   
                   Best_Move  := Iterative_Move;
                   Best_Score := Nega_Score;
                   --  New_Line;
                   exit Iterative_Loop;
                   
                else
                   
                   Best_Move  := Iterative_Move;
                   Best_Score := Nega_Score;
                   
                end if;
                
                --  New_Line;
                
             end loop Iterative_loop;
             
          end if;
          
          -- Apply best move
          
          Move_Command := 
            To_Lower (Board_Column_Type'Image (Best_Move.From.C)) &
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
