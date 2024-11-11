------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           S Y S T E M . T R A C E B A C K . S Y M B O L I C              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2024, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the default implementation for platforms where the full capability
--  is not supported. It returns tracebacks as lists of hexadecimal addresses
--  of the form "0x...".

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
with System.Address_Image;

package body System.Traceback.Symbolic is

   --  Note that Suppress_Hex is ignored in this version of this package.

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback
     (Traceback : System.Traceback_Entries.Tracebacks_Array) return String
   is
   begin
      if Traceback'Length = 0 then
         return "";

      else
         declare
            function Executable_Load_Address return System.Address;
            pragma Import
              (C, Executable_Load_Address,
               "__gnat_get_executable_load_address");

            function Trim_Hex (S : String) return String;
            function Trim_Hex (S : String) return String is
               Non_0 : Positive;
            begin
               for J in S'Range loop
                  if S (J) /= '0' or else J = S'Last then
                     Non_0 := J;
                     exit;
                  end if;
               end loop;
               return S (Non_0 .. S'Last);
            end Trim_Hex;

            Load_Address : constant System.Address :=
              Executable_Load_Address;
            One_If_Executable_Is_PI : constant Natural :=
              Boolean'Pos (Load_Address /= Null_Address);

            --  How long is an Address_Image?
            Max_Image_Length : constant Natural :=
              System.Address_Image (Traceback (Traceback'First))'
                Length;

            Load_Address_Prefix : constant String :=
              "Load address: ";

            Max_Length_Needed : constant Positive :=
              (Load_Address_Prefix'Length *
               One_If_Executable_Is_PI) +
              (Max_Image_Length + 3) *
                (Traceback'Length + One_If_Executable_Is_PI) +
              2;

            Result : String (1 .. Max_Length_Needed);

            Last : Natural := 0;

         begin

            if One_If_Executable_Is_PI /= 0 then
               declare
                  item : constant String :=
                    Load_Address_Prefix & "0x" &
                    Trim_Hex
                      (System.Address_Image (Load_Address)) &
                    ASCII.LF;
               begin
                  Last := item'Length;
                  Result (1 .. Last) := item;
               end;
            end if;

            for J in Traceback'Range loop
               declare
                  Img : constant String :=
                    Trim_Hex
                      (System.Address_Image (Traceback (J)));
               begin
                  Result (Last + 1 .. Last + 2) := "0x";
                  Last := Last + 2;
                  Result (Last + 1 .. Last + Img'Length) := Img;
                  Last := Last + Img'Length + 1;
                  Result (Last)                          := ' ';
               end;
            end loop;

            Result (Last) := ASCII.LF;
            return Result (1 .. Last);
         end;
      end if;
   end Symbolic_Traceback;

   --  "No_Hex" is ignored in this version, because otherwise we have nothing
   --  at all to print.

   function Symbolic_Traceback_No_Hex
     (Traceback : System.Traceback_Entries.Tracebacks_Array) return String is
   begin
      return Symbolic_Traceback (Traceback);
   end Symbolic_Traceback_No_Hex;

   function Symbolic_Traceback
     (E : Ada.Exceptions.Exception_Occurrence) return String
   is
   begin
      return Symbolic_Traceback (Ada.Exceptions.Traceback.Tracebacks (E));
   end Symbolic_Traceback;

   function Symbolic_Traceback_No_Hex
     (E : Ada.Exceptions.Exception_Occurrence) return String is
   begin
      return Symbolic_Traceback (E);
   end Symbolic_Traceback_No_Hex;

   ------------------
   -- Enable_Cache --
   ------------------

   procedure Enable_Cache (Include_Modules : Boolean := False) is
   begin
      null;
   end Enable_Cache;

end System.Traceback.Symbolic;
