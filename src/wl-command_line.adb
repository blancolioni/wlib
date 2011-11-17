with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Text_IO;

package body WL.Command_Line is

   type Registered_Handler is
      record
         Handler    : access Root_Argument_Handler'Class;
         Long_Name  : access String;
         Short_Name : Character;
      end record;

   package Handler_Vectors is
      new Ada.Containers.Vectors (Positive, Registered_Handler);

   package String_Vectors is
      new Ada.Containers.Indefinite_Vectors (Positive, String);

   Handlers    : Handler_Vectors.Vector;
   Dispatchers : Handler_Vectors.Vector;

   Processed_Command_Line : String_Vectors.Vector;

   type Default_Handler is
     new Root_Argument_Handler with
      record
         Bad_Option : access String;
      end record;

   procedure Handle (Handler : in out Default_Handler;
                     Value   : in     String);

   function Find_Handler_By_Short_Name
     (Short_Name : Character)
     return Root_Argument_Handler'Class;

   function Find_Handler_By_Long_Name
     (Long_Name : String)
     return Root_Argument_Handler'Class;

   --------------
   -- Argument --
   --------------

   function Argument (Index : Positive) return String is
   begin
      return Processed_Command_Line.Element (Index);
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is
   begin
      return Processed_Command_Line.Last_Index;
   end Argument_Count;

   ----------------
   -- Exit_After --
   ----------------

   function Exit_After (Handler : Root_Argument_Handler) return Boolean is
      pragma Unreferenced (Handler);
   begin
      return False;
   end Exit_After;

   --------------------------------
   -- Find_Handler_By_Long_Name --
   --------------------------------

   function Find_Handler_By_Long_Name
     (Long_Name : String)
     return Root_Argument_Handler'Class
   is
   begin
      for H of Handlers loop
         if H.Long_Name.all = Long_Name then
            return H.Handler.all;
         end if;
      end loop;
      return D : Default_Handler do
        D.Bad_Option := new String'(Long_Name);
      end return;
   end Find_Handler_By_Long_Name;

   --------------------------------
   -- Find_Handler_By_Short_Name --
   --------------------------------

   function Find_Handler_By_Short_Name
     (Short_Name : Character)
     return Root_Argument_Handler'Class
   is
   begin
      for H of Handlers loop
         if H.Short_Name = Short_Name then
            return H.Handler.all;
         end if;
      end loop;
      return D : Default_Handler do
         D.Bad_Option := new String'((1 => Short_Name));
      end return;
   end Find_Handler_By_Short_Name;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Handler : in out Flag_Argument_Handler;
      Value   : in     String)
   is
   begin
      if Value = "" or else Value = "false" then
         Flag_Argument_Handler'Class (Handler).Handle_Clear;
      else
         Flag_Argument_Handler'Class (Handler).Handle_Set;
      end if;
   end Handle;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Handler : in out Dispatch_Handler;
      Value   : in     String)
   is
      pragma Unreferenced (Value);
   begin
      Dispatch_Handler'Class (Handler).Execute;
   end Handle;

   ------------
   -- Handle --
   ------------

   procedure Handle (Handler : in out Default_Handler;
                     Value   : in     String)
   is
      pragma Unreferenced (Value);
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Unknown option: " & Handler.Bad_Option.all);
      Show_Usage;
   end Handle;

   ------------------
   -- Has_Argument --
   ------------------

   function Has_Argument (Handler : Root_Argument_Handler) return Boolean is
      pragma Unreferenced (Handler);
   begin
      return False;
   end Has_Argument;

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line is
      Skip_Rest      : Boolean := False;
      Tried_Dispatch : Boolean := False;
      Arg_Index      : Positive := 1;
      Dispatcher     : access Dispatch_Handler'Class;
   begin
      while Arg_Index <= Ada.Command_Line.Argument_Count loop
         declare
            Item : constant String :=
              Ada.Command_Line.Argument (Arg_Index);
         begin
            if not Skip_Rest
              and then Item (Item'First) = '-'
            then
               if Item'Length = 1 then
                  Show_Usage;
                  return;
               elsif Item = "--" then
                  Skip_Rest := True;
               elsif Item (Item'First + 1) = '-' then
                  declare
                     Handler : Root_Argument_Handler'Class :=
                       Find_Handler_By_Long_Name
                       (Item (Item'First + 2 .. Item'Last));
                  begin
                     if Handler.Has_Argument then
                        Arg_Index := Arg_Index + 1;
                        Handler.Handle (Ada.Command_Line.Argument (Arg_Index));
                     else
                        Handler.Handle ("true");
                     end if;
                     exit when Handler.Exit_After;
                  end;

               elsif Item'Length = 2 then
                  declare
                     Handler : Root_Argument_Handler'Class :=
                       Find_Handler_By_Short_Name (Item (Item'Last));
                  begin
                     if Handler.Has_Argument then
                        Arg_Index := Arg_Index + 1;
                        Handler.Handle (Ada.Command_Line.Argument (Arg_Index));
                     else
                        Handler.Handle ("true");
                     end if;
                     exit when Handler.Exit_After;
                  end;
               else
                  for J in Item'First + 1 .. Item'Last loop
                     declare
                        Handler : Root_Argument_Handler'Class :=
                          Find_Handler_By_Short_Name (Item (J));
                     begin
                        Handler.Handle ("true");
                        exit when Handler.Exit_After;
                     end;
                  end loop;
               end if;
            elsif not Tried_Dispatch then
               declare
                  Found : Boolean := False;
               begin
                  for D of Dispatchers loop
                     if D.Long_Name.all = Item then
                        Found := True;
                        Dispatcher :=
                          Dispatch_Handler'Class (D.Handler.all)'Access;
                        Processed_Command_Line.Clear;
                        exit;
                     end if;
                  end loop;
                  if not Found then
                     Processed_Command_Line.Append (Item);
                  end if;
               end;

               Tried_Dispatch := True;

            else
               Processed_Command_Line.Append (Item);
            end if;
         end;

         Arg_Index := Arg_Index + 1;

      end loop;

      if Dispatcher /= null then
         Dispatcher.Execute;
      end if;

   end Process_Command_Line;

   --------------
   -- Register --
   --------------

   procedure Register
     (Long_Name  : String;
      Short_Name : Character;
      Handler    : Root_Argument_Handler'Class)
   is
   begin
      Handlers.Append ((new Root_Argument_Handler'Class'(Handler),
                        new String'(Long_Name),
                        Short_Name));
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Long_Name  : String;
      Handler    : Root_Argument_Handler'Class)
   is
   begin
      if Handler in Dispatch_Handler'Class then
         Dispatchers.Append ((new Root_Argument_Handler'Class'(Handler),
                              new String'(Long_Name),
                              Character'Val (0)));
      else
         Register (Long_Name, Character'Val (0), Handler);
      end if;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Short_Name : Character;
      Handler    : Root_Argument_Handler'Class)
   is
   begin
      Register ("", Short_Name, Handler);
   end Register;

   ----------------
   -- Show_Usage --
   ----------------

   procedure Show_Usage is
   begin
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                       "Usage: "
                         & Ada.Command_Line.Command_Name
                         & " [options]");
   end Show_Usage;

end WL.Command_Line;
