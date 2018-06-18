with Ada.Exceptions;
with Ada.Text_IO;

package body WL.Tests is

   -----------------
   -- Add_Message --
   -----------------

   procedure Add_Message
     (Test    : in out Root_Test_Type'Class;
      Message : String)
   is
   begin
      Test.Messages.Append (Message);
   end Add_Message;

   ------------
   -- Append --
   ------------

   procedure Append
     (Suite     : in out Test_Suite'Class;
      Category  : String;
      Name      : String;
      Test      : Test_Function)
   is
      Rec : constant Function_Test :=
              Function_Test'
                (Messages => <>,
                 Test     => Test);
   begin
      Suite.Append (Category, Name, Rec);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Suite     : in out Test_Suite'Class;
      Category  : String;
      Name      : String;
      Test      : Root_Test_Type'Class)
   is
      use Ada.Strings.Unbounded;
   begin
      Suite.Tests.Append
        (Test_Record'
           (Category_Name => To_Unbounded_String (Category),
            Test_Name     => To_Unbounded_String (Name),
            Test          => new Root_Test_Type'Class'(Test)));
   end Append;

   -------------
   -- Execute --
   -------------

   overriding function Execute (Test : in out Function_Test) return Boolean is
   begin
      if not Test.Test.all then
         Test.Add_Message ("test function returned false");
         return False;
      end if;
      return True;
   end Execute;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests
     (Suite : Test_Suite'Class)
   is
      Count  : Natural := 0;
      Passed : Natural := 0;

      procedure Write
        (Test    : Test_Record;
         Message : String);

      -----------
      -- Write --
      -----------

      procedure Write
        (Test    : Test_Record;
         Message : String)
      is
      begin
         Ada.Text_IO.Put_Line
           (Ada.Strings.Unbounded.To_String (Test.Category_Name)
            & "/"
            & Ada.Strings.Unbounded.To_String (Test.Test_Name)
            & ": "
            & Message);
      end Write;

   begin
      for Rec of Suite.Tests loop
         Count := Count + 1;
         select
            delay 5.0;
            Write (Rec, "timed out");
         then abort
            begin
               if Rec.Test.Execute then
                  Passed := Passed + 1;
               else
                  Write (Rec, "failed");
                  for Message of Rec.Test.Messages loop
                     Write (Rec, Message);
                  end loop;
               end if;
            exception
               when E : others =>
                  Write (Rec, "caught exception: "
                         & Ada.Exceptions.Exception_Message (E));
            end;
         end select;
      end loop;
      Ada.Text_IO.Put_Line
        ("tests:" & Count'Image
         & "; passed:" & Passed'Image
         & "; failed: " & Natural'Image (Count - Passed));

   end Run_Tests;

end WL.Tests;
