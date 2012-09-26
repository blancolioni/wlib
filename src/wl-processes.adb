with Ada.Text_IO;                       use Ada.Text_IO;

package body WL.Processes is

   Progress_Characters : constant String :=
                           "-/|\";

   ------------
   -- Finish --
   ------------

   procedure Finish (Process : in out Process_Type) is
   begin
      case Process.Display is
         when Spinner =>
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                             Character'Val (8) & ' ');
            Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
         when Bar =>
            null;
         when Percentage =>
            Put (Ada.Text_IO.Standard_Error,
                 Character'Val (13) &
                   Process.Name.all & ": 100%");
      end case;

      Process.Tick := 0;
      New_Line (Ada.Text_IO.Standard_Error);
   end Finish;

   ---------------
   -- Start_Bar --
   ---------------

   procedure Start_Bar
     (Process   :    out Process_Type;
      Name      : String;
      Tick_Size : Positive := 1)
   is
   begin
      Put (Ada.Text_IO.Standard_Error, Name & ": ");
      Flush (Ada.Text_IO.Standard_Error);
      Process.Display := Bar;
      Process.Tick    := 0;
      Process.Step    := Tick_Size;
   end Start_Bar;

   ----------------------
   -- Start_Percentage --
   ----------------------

   procedure Start_Percentage (Process   :    out Process_Type;
                               Name      : String;
                               Finish    : Positive;
                               Tick_Size : Positive := 1)
   is
   begin
      Put (Ada.Text_IO.Standard_Error, Name & ": 0%");
      Flush (Ada.Text_IO.Standard_Error);
      Process.Name    := new String'(Name);
      Process.Display := Percentage;
      Process.Tick    := 0;
      Process.Finish  := Finish;
      Process.Step    := Tick_Size;
      Process.Acc     := 1;
   end Start_Percentage;

   -------------------
   -- Start_Spinner --
   -------------------

   procedure Start_Spinner
     (Process   :    out Process_Type;
      Name      : String;
      Tick_Size : Positive := 1)
   is
   begin
      Put (Ada.Text_IO.Standard_Error, Name & ": " &
           Progress_Characters (1));
      Flush (Ada.Text_IO.Standard_Error);
      Process.Display := Spinner;
      Process.Tick    := 0;
      Process.Step    := Tick_Size;
      Process.Acc     := 1;
   end Start_Spinner;

   ----------
   -- Tick --
   ----------

   procedure Tick (Process : in out Process_Type) is
   begin
      Process.Tick := Process.Tick + 1;
      if Process.Tick mod Process.Step = 0 then
         case Process.Display is
            when Spinner =>
               Process.Acc := Process.Acc + 1;
               if Process.Acc > Progress_Characters'Last then
                  Process.Acc := 1;
               end if;
               Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                                Character'Val (8) &
                                  Progress_Characters (Process.Acc));
               Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
            when Bar =>
               Put (Ada.Text_IO.Standard_Error, ".");
               Flush (Ada.Text_IO.Standard_Error);
            when Percentage =>
               if Process.Tick > Process.Finish then
                  Process.Tick := Process.Finish;
               end if;
               declare
                  Value : constant Natural :=
                            Natural (Float (Process.Tick)
                                     / Float (Process.Finish) * 100.0);
               begin
                  Put (Ada.Text_IO.Standard_Error,
                       Character'Val (13) &
                         Process.Name.all & ":"
                       & Natural'Image (Value) & "%");
                  Flush (Ada.Text_IO.Standard_Error);
               end;
         end case;
      end if;
   end Tick;

end WL.Processes;
