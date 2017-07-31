with Ada.Command_Line;
with Ada.Strings.Fixed;

package body WL.Command_Line is

   -----------------
   -- Find_Option --
   -----------------

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character)
     return String
   is
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String :=
              Ada.Command_Line.Argument (I);
            Separator_Index : constant Natural :=
              Ada.Strings.Fixed.Index (Argument, "=");
         begin
            if Short_Name /= ' ' and then Argument = ('-', Short_Name) then
               if I < Ada.Command_Line.Argument_Count then
                  return Ada.Command_Line.Argument (I + 1);
               else
                  return "";
               end if;
            elsif Argument'Length > 3 and then Separator_Index > 0
              and then Argument (1 .. 2) = "--"
              and then Argument (3 .. Separator_Index - 1) = Long_Name
            then
               return Argument (Separator_Index + 1 .. Argument'Last);
            end if;
         end;
      end loop;
      return "";
   end Find_Option;

   -----------------
   -- Find_Option --
   -----------------

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character)
     return Boolean
   is
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String :=
              Ada.Command_Line.Argument (I);
         begin
            if Argument = "--" & Long_Name then
               return True;
            elsif Argument = "--no-" & Long_Name then
               return False;
            elsif Argument'Length >= 2
              and then Short_Name /= ' '
              and then Argument (Argument'First) = '-'
              and then Argument (Argument'First + 1) /= '-'
            then
               for Item of Argument loop
                  if Item /= '-' and then Item = Short_Name then
                     return True;
                  end if;
               end loop;
            end if;
         end;
      end loop;
      return False;
   end Find_Option;

   -----------------
   -- Find_Option --
   -----------------

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character;
      Default    : Integer)
     return Integer
   is
      Value : constant String := Find_Option (Long_Name, Short_Name);
   begin
      if Value = "" then
         return Default;
      else
         return Integer'Value (Value);
      end if;
   exception
      when Constraint_Error =>
         return Default;
   end Find_Option;

end WL.Command_Line;
