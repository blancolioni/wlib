with Ada.Integer_Text_IO;
with Ada.Text_IO;                       use Ada.Text_IO;

package body WL.Processes is

   Progress_Characters : constant String :=
                           "-\|/";

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
            declare
               Count  : constant Natural := Process.Bar_Length;
               Final  : constant String (1 .. Count) :=
                          (others => '=');
            begin
               Put
                 (Ada.Text_IO.Standard_Error,
                  Character'Val (13)
                  & Process.Name.all
                  & ": 100% [" & Final & "]");
            end;
         when Percentage =>
            Put (Ada.Text_IO.Standard_Error,
                 Character'Val (13) &
                   Process.Name.all & ": 100%");
         when Counter =>
            Put (Ada.Text_IO.Standard_Error,
                 Character'Val (13)
                 & Process.Name.all & ":"
                 & Natural'Image (Process.Tick));
      end case;

      Process.Tick := 0;
      New_Line (Ada.Text_IO.Standard_Error);
   end Finish;

   ---------------
   -- Start_Bar --
   ---------------

   procedure Start_Bar
     (Process    :    out Process_Type;
      Name       : String;
      Finish     : Positive;
      Bar_Length : Natural  := 40;
      Tick_Size  : Positive := 1)
   is
      Spaces : constant String (1 .. Process.Bar_Length) :=
                 (others => ' ');
   begin
      Put (Ada.Text_IO.Standard_Error,
           Name & ":      [" & Spaces & "]");
      Flush (Ada.Text_IO.Standard_Error);
      Process.Name    := new String'(Name);
      Process.Display := Bar;
      Process.Tick    := 0;
      Process.Finish  := Finish;
      Process.Step    := Tick_Size;
      Process.Acc     := 1;
      Process.Bar_Length := Bar_Length;
   end Start_Bar;

   -------------------
   -- Start_Counter --
   -------------------

   procedure Start_Counter (Process   :    out Process_Type;
                            Name      : String;
                            Tick_Size : Positive := 1)
   is
   begin
      Put (Ada.Text_IO.Standard_Error, Name & ": 0");
      Flush (Ada.Text_IO.Standard_Error);
      Process.Name    := new String'(Name);
      Process.Display := Counter;
      Process.Tick    := 0;
      Process.Step    := Tick_Size;
      Process.Prev    := Ada.Calendar.Clock;
   end Start_Counter;

   ----------------------
   -- Start_Percentage --
   ----------------------

   procedure Start_Percentage
     (Process   :    out Process_Type;
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
      if Process.Tick > Process.Finish then
         Process.Tick := Process.Finish;
      end if;

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
               declare
                  Count  : constant Natural :=
                             Process.Finish / Process.Step;
                  Current : constant Natural :=
                              Process.Tick / Process.Step;
                  Filled_Length : constant Natural :=
                                    Current * Process.Bar_Length / Count;
                  Empty_Length  : constant Natural :=
                                    (Count - Current) * Process.Bar_Length
                                    / Count;
                  Half_Length   : constant Natural :=
                                    Process.Bar_Length
                                      - Filled_Length - Empty_Length;
                  Filled        : constant String (1 .. Filled_Length) :=
                                    (others => '=');
                  Half          : constant String (1 .. Half_Length) :=
                                    (others => '-');
                  Empty         : constant String (1 .. Empty_Length) :=
                                    (others => ' ');
                  Bar           : constant String := Filled & Half & Empty;

               begin
                  if Current /= Process.Last_Value then
                     Put
                       (Ada.Text_IO.Standard_Error,
                        Character'Val (13)
                        & Process.Name.all
                        & ": ");
                     Ada.Integer_Text_IO.Put
                       (Standard_Error,
                        100 * Current / Count, 3);
                     Ada.Text_IO.Put
                       (Standard_Error,
                        "% [" & Bar & "]");
                     Process.Last_Value := Current;
                     Flush (Ada.Text_IO.Standard_Error);
                  end if;
               end;
            when Percentage =>
               declare
                  Last_Value : constant Natural :=
                                 Natural (Float (Process.Tick - 1)
                                          / Float (Process.Finish) * 100.0);
                  Value : constant Natural :=
                                 Natural (Float (Process.Tick)
                                          / Float (Process.Finish) * 100.0);
               begin
                  if Last_Value /= Value then
                     Put (Ada.Text_IO.Standard_Error,
                          Character'Val (13) &
                            Process.Name.all & ":"
                          & Natural'Image (Value) & "%");
                     Flush (Ada.Text_IO.Standard_Error);
                  end if;
               end;
            when Counter =>
               declare
                  use Ada.Calendar;
                  Now : constant Time := Clock;
               begin
                  if Now - Process.Prev > 0.1 then
                     Put (Ada.Text_IO.Standard_Error,
                          Character'Val (13)
                          & Process.Name.all & ":"
                          & Natural'Image (Process.Tick));
                     Flush (Ada.Text_IO.Standard_Error);
                     Process.Prev := Now;
                  end if;
               end;
         end case;
      end if;
   end Tick;

end WL.Processes;
