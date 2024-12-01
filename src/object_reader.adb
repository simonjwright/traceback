with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_Line;

pragma Warnings (Off, "internal GNAT unit");
with System.Object_Reader;
pragma Warnings (On, "internal GNAT unit");

procedure Object_Reader is
   Command_Line_Config : GNAT.Command_Line.Command_Line_Configuration;
begin
   GNAT.Command_Line.Set_Usage
     (Command_Line_Config,
      Usage => "ARG",
      Help  => "Report structure of executable");
   GNAT.Command_Line.Getopt (Command_Line_Config);

   declare
      Arg : constant String :=
        GNAT.Command_Line.Get_Argument (Do_Expansion => True);
      F   : System.Object_Reader.Object_File_Access;
   begin
      if Arg'Length > 0 then
         F := System.Object_Reader.Open (Arg);

      else
         Put_Line ("No executable specified");
      end if;
   end;

exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      null;
end Object_Reader;
