with Ada.Exceptions;
with Ada.Exceptions.Traceback;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
procedure Traceback is
begin
   raise Constraint_Error;
exception
   when E : others =>
      Put_Line ("--- Ada.Exceptions.Exception_Information ---");
      Put_Line (Ada.Exceptions.Exception_Information (E));
   Put_Line ("--- GNAT.Traceback.Symbolic.Symbolic_Traceback ---");

   --  Catch any exceptions raised in the software under development.
   begin
      Put_Line
        (GNAT.Traceback.Symbolic.Symbolic_Traceback
           (Ada.Exceptions.Traceback.Tracebacks (E)));
   exception
      when E : others =>
         Put_Line ("exception in Symbolic_Traceback: " &
            Ada.Exceptions.Exception_Name (E));
   end;
end Traceback;
