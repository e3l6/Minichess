-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 31 May 2017, CS 442P-003 Term Project
--
-- netops.ads
--
-- Much of the format and procedure names (and algorithms) were unabashedly
--   borrowed from Bart Massey's netops.[hc] for his example minichess player
--   found at https://github.com/BartMassey/imcs/tree/master/client/c as of
--   20 May 2017.
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
   
   function  Initialize (Username   : in     String;
                         Password   : in     String) return Boolean;
   
   procedure Sendcmd    (Command    : in     String);
   
   procedure Shutdown;
   
end Netops;
