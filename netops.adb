-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- netops.adb
--
-------------------------------------------------------------------------------

package body Netops is
   
   Address    : Sock_Addr_Type;
   Socket     : Socket_Type;
   Channel    : Stream_Access;
   
   Host_Name  : String    := "imcs.svcs.cs.pdx.edu";
   Port       : Port_Type := 3589;
   
   Data       : Stream_Element_Array (1..1024);
   Size       : Stream_Element_Offset;
   Return_Str : Unbounded_String;
   
   CRLF       : constant String := ASCII.CR & ASCII.LF;
   
   Username   : constant String := "Archimeathead";
   
   ----------------------------------------------------------------------------
   function Expectcmd return Integer is
      
   begin
      
      Return_Str := Null_Unbounded_String;
      
      Receive_Socket (Socket, Data, Size);
      
      for I in 1 .. Size loop
         Return_Str := Return_Str & Character'Val(Data(I));
      end loop;
      
      Put_Line (To_String(Return_Str));
      
      return (Integer'Value (To_String (Head (Return_Str, 3))));
      
   exception when E : others =>
      
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      
   end expectcmd;
   
   
   ----------------------------------------------------------------------------
   function Getnet (Key_Symbol :    out Character) return Unbounded_String is
      
   begin
      
      Return_Str := Null_Unbounded_String;
      
      Receive_Socket (Socket, Data, Size);
      
      for I in 1 .. Size loop
         Return_Str := Return_Str & Character'Val(Data(I));
      end loop;
      
      if (Index (Return_Str, "=", 1) /= 0) then
         
         Key_Symbol := '=';
         
      elsif (Index (Return_Str, CRLF & "?", 1) /= 0) then
         
         Key_Symbol := '?';
         
      else
         
         Key_Symbol := 'o';
         
      end if;
      
      Put_Line (To_String(Return_Str));
      
      return Return_Str;
      
   exception when E : others =>
      
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      
   end Getnet;
   
   
   ----------------------------------------------------------------------------
   function Initialize (Password : String) return Boolean is
      
   begin
      
      Put_Line ("Initializing");
      
      Initialize (Process_Blocking_IO => False);
      
      Put_Line ("Looking up " & Host_Name);
      
      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      Address.Port := Port;
      
      Put_Line ("Creating socket");
      
      Create_Socket (Socket);
      
      --  Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
      
      Put_Line ("Opening connection with " & Image (Address));
      
      Connect_Socket (Socket, Address);
      
      Put_Line ("Assigning channel");
      New_Line;
      
      Channel := Stream (Socket);
      
      if (Expectcmd = 100) then
      
         Sendcmd ("me " & Username & " " & Password);
         
         if (Expectcmd = 201) then
            
            return True;
            
         end if;
         
      else
         
         return False;
         
      end if;
      
   exception when E : others =>
      
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      
   end Initialize;
   
   
   ----------------------------------------------------------------------------
   procedure Sendcmd (Command : String) is
      
   begin
      
      Put_Line (Command);
      
      String'Write (Channel, Command & CRLF);
      
   exception when E : others =>
      
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      
   end Sendcmd;
   
   
   ----------------------------------------------------------------------------
   procedure Shutdown is
      
   begin
      
      Shutdown_Socket (Socket);
      
      Close_Socket (Socket);
      
      Put_Line ("Connection closed");
      
   exception when E : others =>
      
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      
   end Shutdown;
   
end Netops;
