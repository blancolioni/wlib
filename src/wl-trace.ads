package WL.Trace is
   pragma Elaborate_Body;

   type Trace_Level is new Natural;

   Default_Level : constant Trace_Level := 3;

   procedure Set_Default_Level
     (To_Level : Trace_Level := Default_Level);

   procedure Start_Trace
     (Level     : Trace_Level;
      File_Name : String := "trace.txt");

   procedure Put (Message : String);

   procedure Put (Level : in Trace_Level; Message : in String);

   procedure Put_Line (Message : String);

   procedure Put_Line (Level : in Trace_Level; Message : in String);

   procedure New_Line;
   procedure New_Line (Level : in Trace_Level);
   procedure New_Line (Level : in Trace_Level; Count : in Natural);

   procedure Set_Col (Col : in Positive);
   procedure Set_Col (Level : in Trace_Level; Col : in Positive);

   function Col return Positive;

   procedure End_Trace;

end WL.Trace;
