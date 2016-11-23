with Ada.Strings.Fixed;
with Ada.Text_IO;

with WL.String_Maps;

package body WL.Localisation is

   package Text_Maps is
     new WL.String_Maps (String);

   Local_Map : Text_Maps.Map;

   --------------------
   -- Has_Local_Text --
   --------------------

   function Has_Local_Text
     (Tag          : String)
      return Boolean
   is
   begin
      return Local_Map.Contains (Tag);
   end Has_Local_Text;

   ------------------
   -- Local_Number --
   ------------------

   function Local_Number
     (Value : Natural)
      return String
   is
      Tag : constant String :=
              Ada.Strings.Fixed.Trim
                (Natural'Image (Value),
                 Ada.Strings.Both);
   begin
      if Has_Local_Text (Tag) then
         return Local_Text (Tag);
      else
         return Tag;
      end if;
   end Local_Number;

   ------------------
   -- Local_Number --
   ------------------

   function Local_Number
     (Localisation : Localisation_Interface'Class;
      Value        : Natural)
      return String
   is
      Tag : constant String :=
              Ada.Strings.Fixed.Trim
                (Natural'Image (Value),
                 Ada.Strings.Both);
   begin
      if Localisation.Has_Local_Text (Tag) then
         return Localisation.Local_Text (Tag);
      else
         return Tag;
      end if;
   end Local_Number;

   ----------------
   -- Local_Text --
   ----------------

   function Local_Text
     (Tag : String;
      Arg_1 : String := "";
      Arg_2 : String := "";
      Arg_3 : String := "";
      Arg_4 : String := "")
      return String
   is

      subtype Argument_Index is Character range '1' .. '4';

      function Sub (S : String) return String;

      ---------
      -- Sub --
      ---------

      function Sub (S : String) return String is
         Index : constant Natural :=
                   Ada.Strings.Fixed.Index (S, "{");
      begin
         if Index > 0
           and then Index < S'Last - 1
           and then S (Index + 1) in Argument_Index
           and then S (Index + 2) = '}'
         then
            declare
               Front : constant String :=
                         (case Argument_Index (S (Index + 1)) is
                             when '1' => Arg_1,
                             when '2' => Arg_2,
                             when '3' => Arg_3,
                             when '4' => Arg_4);
               Rest : constant String := Sub (S (Index + 3 .. S'Last));
            begin
               return S (S'First .. Index - 1) & Front & Rest;
            end;
         else
            return S;
         end if;
      end Sub;

   begin
      if Tag = "" then
         return "";
      elsif Local_Map.Contains (Tag) then
         return Sub (Local_Map.Element (Tag));
      else
         return "[" & Tag & "]"
           & (if Arg_1 = "" then "" else " " & Arg_1)
           & (if Arg_2 = "" then "" else " " & Arg_2)
           & (if Arg_3 = "" then "" else " " & Arg_3)
           & (if Arg_4 = "" then "" else " " & Arg_4);
      end if;
   end Local_Text;

   -----------------------
   -- Read_Localisation --
   -----------------------

   procedure Read_Localisation
     (Path : String)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
            Index : constant Natural :=
                      Ada.Strings.Fixed.Index (Line, ",");
         begin
            if Index > 0 then
               Local_Map.Insert
                 (Line (Line'First .. Index - 1),
                  Line (Index + 1 .. Line'Last));
            end if;
         end;
      end loop;
   end Read_Localisation;

   -----------------------
   -- Read_Localisation --
   -----------------------

   procedure Read_Localisation
     (Language : out Language_Type;
      Path     : String)
   is
   begin
      Language := "en-uk";
      Read_Localisation (Path);
   end Read_Localisation;

end WL.Localisation;
