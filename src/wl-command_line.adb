with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with WL.String_Maps;

package body WL.Command_Line is

   package Option_Value_Maps is
     new WL.String_Maps (String);

   Default_Values : Option_Value_Maps.Map;
   Cached_Values : Option_Value_Maps.Map;

   -----------------
   -- Find_Option --
   -----------------

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character)
     return String
   is
   begin
      if not Cached_Values.Contains (Long_Name) then
         for I in 1 .. Ada.Command_Line.Argument_Count loop
            declare
               Argument        : constant String :=
                                   Ada.Command_Line.Argument (I);
               Separator_Index : constant Natural :=
                                   Ada.Strings.Fixed.Index (Argument, "=");
            begin
               if Short_Name /= ' ' and then Argument = ('-', Short_Name) then
                  if I < Ada.Command_Line.Argument_Count then
                     Cached_Values.Insert
                       (Long_Name, Ada.Command_Line.Argument (I + 1));
                  else
                     Cached_Values.Insert (Long_Name, "");
                  end if;
                  exit;
               elsif Argument'Length > 3 and then Separator_Index > 0
                 and then Argument (1 .. 2) = "--"
                 and then Argument (3 .. Separator_Index - 1) = Long_Name
               then
                  Cached_Values.Insert
                    (Long_Name,
                     Argument (Separator_Index + 1 .. Argument'Last));
                  exit;
               end if;
            end;
         end loop;

         if not Cached_Values.Contains (Long_Name) then
            if Default_Values.Contains (Long_Name) then
               Cached_Values.Insert
                 (Long_Name, Default_Values.Element (Long_Name));
            else
               Cached_Values.Insert
                 (Long_Name, "");
            end if;
         end if;
      end if;

      return Cached_Values.Element (Long_Name);

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
      if not Cached_Values.Contains (Long_Name) then
         Scan_Command_Line :
         for I in 1 .. Ada.Command_Line.Argument_Count loop
            declare
               Argument : constant String :=
                            Ada.Command_Line.Argument (I);
            begin
               if Argument = "--" & Long_Name then
                  Cached_Values.Insert (Long_Name, "true");
                  exit Scan_Command_Line;
               elsif Argument = "--no-" & Long_Name then
                  Cached_Values.Insert (Long_Name, "");
                  exit Scan_Command_Line;
               elsif Argument'Length >= 2
                 and then Short_Name /= ' '
                 and then Argument (Argument'First) = '-'
                 and then Argument (Argument'First + 1) /= '-'
               then
                  for Item of Argument loop
                     if Item /= '-' and then Item = Short_Name then
                        Cached_Values.Insert (Long_Name, "true");
                        exit Scan_Command_Line;
                     end if;
                  end loop;
               end if;
            end;
         end loop Scan_Command_Line;

         if not Cached_Values.Contains (Long_Name) then
            if Default_Values.Contains (Long_Name) then
               declare
                  Value : constant String :=
                            Default_Values.Element (Long_Name);
               begin
                  if Value = "true"
                    or else Value = "yes"
                    or else Value = "1"
                  then
                     Cached_Values.Insert
                       (Long_Name, "true");
                  else
                     Cached_Values.Insert (Long_Name, "");
                  end if;
               end;
            else
               Cached_Values.Insert (Long_Name, "");
            end if;
         end if;
      end if;

      return Cached_Values.Element (Long_Name) = "true";

   end Find_Option;

   -----------------
   -- Find_Option --
   -----------------

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character;
      Default    : Integer    := 0)
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

   -------------------
   -- Load_Defaults --
   -------------------

   procedure Load_Defaults
     (File_Path : String)
   is
      use Ada.Text_IO;
      File        : File_Type;
      Line_Number : Natural := 0;
   begin
      if not Ada.Directories.Exists (File_Path) then
         return;
      end if;

      Open (File, In_File, File_Path);
      while not End_Of_File (File) loop

         Line_Number := Line_Number + 1;

         declare
            use Ada.Strings, Ada.Strings.Fixed;
            Full_Line : constant String := Get_Line (File);
            Comment_Index : constant Natural :=
                              Index (Full_Line, "#");
            Active_Line   : constant String :=
                              (if Comment_Index = 0
                               then Full_Line
                               else Full_Line (1 .. Comment_Index - 1));
            Trimmed_Line  : constant String :=
                              Trim (Active_Line, Both);
            Equal_Index   : constant Natural :=
                              Index (Trimmed_Line, "=");
            Name          : constant String :=
                              Trim (Trimmed_Line (1 .. Equal_Index - 1),
                                    Right);
            Value         : constant String :=
                              Trim (Trimmed_Line (Equal_Index + 1 ..
                                      Trimmed_Line'Last),
                                    Left);
         begin
            if Trimmed_Line'Length > 0 then
               if Name = "" then
                  raise Constraint_Error with
                    "error in " & Ada.Directories.Simple_Name (File_Path)
                    & " line" & Natural'Image (Line_Number)
                    & ": invalid setting";
               end if;

               Ada.Text_IO.Put_Line ("option: " & Name & " = " & Value);
               Default_Values.Insert (Name, Value);
            end if;
         end;
      end loop;
      Close (File);
   exception
      when Name_Error =>
         null;
   end Load_Defaults;

end WL.Command_Line;
