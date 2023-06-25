with Ada.Text_IO;

package body WL.Trace is

   Current_Level_Default : Trace_Level := Default_Level;
   Current_Trace_Level   : Trace_Level := 0;
   Trace_On              : Boolean := False;
--     Screen_Echo           : Boolean := False;
--     Screen_Echo_Level     : Trace_Level := 0;

   Trace_File            : Ada.Text_IO.File_Type;

   -----------------------
   -- Set_Default_Level --
   -----------------------
   procedure Set_Default_Level
     (To_Level : Trace_Level := Default_Level)
   is
   begin
      Current_Level_Default := To_Level;
   end Set_Default_Level;

   -----------------
   -- Start_Trace --
   -----------------
   procedure Start_Trace
     (Level     : Trace_Level;
      File_Name : String := "trace.txt")
   is
   begin
      Current_Trace_Level := Level;
      Trace_On := True;
      Ada.Text_IO.Create (Trace_File, Ada.Text_IO.Out_File, File_Name);
   end Start_Trace;

   ---------
   -- Put --
   ---------
   procedure Put (Message : String) is
   begin
      Put (Current_Level_Default, Message);
   end Put;

   ---------
   -- Put --
   ---------
   procedure Put (Level : Trace_Level; Message : String) is
   begin
      if Trace_On and then Level <= Current_Trace_Level then
         Ada.Text_IO.Put (Trace_File, Message);
      end if;
   end Put;

   procedure Put_Line (Message : in String) is
   begin
      Put_Line (Current_Level_Default, Message);
   end Put_Line;

   procedure Put_Line (Level : in Trace_Level; Message : in String) is
   begin
      Put (Level, Message);
      New_Line (Level);
   end Put_Line;

   procedure New_Line is
   begin
      New_Line (Current_Level_Default);
   end New_Line;

   procedure New_Line (Level : in Trace_Level) is
   begin
      New_Line (Level, 1);
   end New_Line;

   procedure New_Line (Level : in Trace_Level; Count : in Natural) is
   begin
      if Trace_On and then Level <= Current_Trace_Level then
         for I in 1 .. Count loop
            Ada.Text_IO.New_Line (Trace_File);
         end loop;
         Ada.Text_IO.Flush (Trace_File);
      end if;
   end New_Line;


   procedure Set_Col (Col : in Positive) is
   begin
      Set_Col (Current_Level_Default, Col);
   end Set_Col;

   procedure Set_Col (Level : in Trace_Level; Col : in Positive) is
   begin
      if Trace_On and then Level <= Current_Trace_Level then
         Ada.Text_IO.Set_Col
           (Trace_File, Ada.Text_IO.Positive_Count (Col));
      end if;
   end Set_Col;

   function Col return Positive is
   begin
      if Trace_On then
         return Positive (Ada.Text_IO.Col (Trace_File));
      else
         return 1;
      end if;
   end Col;

   procedure End_Trace is
   begin
      Ada.Text_IO.Close (Trace_File);
      Trace_On := False;
   end End_Trace;

end WL.Trace;
