-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- netops.ads
--
-------------------------------------------------------------------------------

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;      
with GNAT.Sockets;          use GNAT.Sockets;

package Netops is
   
   function  Expectcmd return Integer;
   
   function  Getnet     (Key_Symbol :    out Character)
                        return Unbounded_String;
   
   function  Initialize (Password   : in     String) return Boolean;
   
   procedure Sendcmd    (Command    : in     String);
   
   procedure Shutdown;
   
end Netops;
