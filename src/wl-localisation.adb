with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with WL.String_Maps;

package body WL.Localisation is

   package Text_Maps is
     new WL.String_Maps (String);

   Local_Map : Text_Maps.Map;

   Default_Language : constant Language_Type := ("en", "  ");

   function To_Key
     (Language : Language_Type;
      Tag      : String)
      return String
   is (String (Language.Language)
       & (if Language.Country /= "  "
          then "-" & String (Language.Country)
          else "")
       & "--["
       & Tag
       & "]");

   function Get_Local_Text
     (Key   : String;
      Arg_1 : String;
      Arg_2 : String;
      Arg_3 : String;
      Arg_4 : String)
      return String;

   --------------------
   -- Get_Local_Text --
   --------------------

   function Get_Local_Text
     (Key   : String;
      Arg_1 : String;
      Arg_2 : String;
      Arg_3 : String;
      Arg_4 : String)
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
               Rest  : constant String := Sub (S (Index + 3 .. S'Last));
            begin
               return S (S'First .. Index - 1) & Front & Rest;
            end;
         else
            return S;
         end if;
      end Sub;

   begin
      if Local_Map.Contains (Key) then
         return Sub (Local_Map.Element (Key));
      else
         return Key
           & (if Arg_1 = "" then "" else " " & Arg_1)
           & (if Arg_2 = "" then "" else " " & Arg_2)
           & (if Arg_3 = "" then "" else " " & Arg_3)
           & (if Arg_4 = "" then "" else " " & Arg_4);
      end if;
   end Get_Local_Text;

   --------------------
   -- Has_Local_Text --
   --------------------

   function Has_Local_Text
     (Tag          : String)
      return Boolean
   is
   begin
      return Has_Local_Text (Default_Language, Tag);
   end Has_Local_Text;

   --------------------
   -- Has_Local_Text --
   --------------------

   function Has_Local_Text
     (Language     : Language_Type;
      Tag          : String)
      return Boolean
   is
   begin
      return Local_Map.Contains (To_Key (Language, Tag));
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
   begin
      return Local_Text (Default_Language, Tag, Arg_1, Arg_2, Arg_3, Arg_4);
   end Local_Text;

   ----------------
   -- Local_Text --
   ----------------

   function Local_Text
     (Language : Language_Type;
      Tag      : String;
      Arg_1    : String := "";
      Arg_2    : String := "";
      Arg_3    : String := "";
      Arg_4    : String := "")
      return String
   is
   begin
      return Get_Local_Text (To_Key (Language, Tag),
                             Arg_1, Arg_2, Arg_3, Arg_4);
   end Local_Text;

   -------------------------------
   -- Read_Language_Directories --
   -------------------------------

   procedure Read_Language_Directories
     (Path     : String)
   is
      use Ada.Directories;

      procedure Process (Item : Directory_Entry_Type);

      -------------
      -- Process --
      -------------

      procedure Process (Item : Directory_Entry_Type) is
         Name : constant String := Simple_Name (Item);
      begin
         if Name /= "." and then Name /= ".." then
            declare
               Language : constant Language_Type :=
                            To_Language (Name);
            begin
               if Is_Valid (Language) then
                  Ada.Text_IO.Put_Line
                    ("available language: " & Name);
                  Read_Localisation_Directory
                    (Language, Full_Name (Item));
               end if;
            end;
         end if;
      end Process;

   begin
      Search
        (Directory => Path,
         Pattern   => "*",
         Filter    => (Directory => True, others => False),
         Process   => Process'Access);
   end Read_Language_Directories;

   -----------------------
   -- Read_Localisation --
   -----------------------

   procedure Read_Localisation
     (Path : String)
   is
   begin
      Read_Localisation_File (Default_Language, Path);
   end Read_Localisation;

   ---------------------------------
   -- Read_Localisation_Directory --
   ---------------------------------

   procedure Read_Localisation_Directory
     (Language : Language_Type;
      Path     : String)
   is
      use Ada.Directories;

      procedure Process (Item : Directory_Entry_Type);

      -------------
      -- Process --
      -------------

      procedure Process (Item : Directory_Entry_Type) is
      begin
         Read_Localisation_File (Language, Full_Name (Item));
      end Process;

   begin
      Search
        (Directory => Path,
         Pattern   => "*",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Process'Access);
   end Read_Localisation_Directory;

   -----------------------
   -- Read_Localisation --
   -----------------------

   procedure Read_Localisation_File
     (Language : Language_Type;
      Path     : String)
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
               declare
                  Key : constant String :=
                          To_Key (Language, Line (Line'First .. Index - 1));
                  Value : constant String :=
                            Line (Index + 1 .. Line'Last);
               begin
                  if Local_Map.Contains (Key) then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "duplicate localisation entries found for '"
                        & Key & "'"
                        & (if Value = Local_Map.Element (Value)
                          then " with identical values"
                          else " with different values"));
                  else
                     Local_Map.Insert (Key, Value);
                  end if;
               end;
            end if;
         end;
      end loop;
      Close (File);
   end Read_Localisation_File;

   -----------------
   -- To_Language --
   -----------------

   function To_Language
     (Code : String)
      return Language_Type
   is
      use Ada.Characters.Handling;
   begin
      if (Code'Length /= 2 and then Code'Length /= 5)
        or else (Code'Length = 5 and then Code (Code'First + 2) /= '-')
        or else not Is_Letter (Code (Code'First))
        or else not Is_Letter (Code (Code'First + 1))
        or else (Code'Length = 5 and then
                   (not Is_Letter (Code (Code'First + 3))
                    or else not Is_Letter (Code (Code'First + 4))))
      then
         return No_Language;
      end if;

      if Code'Length = 2 then
         return (ISO_Language (Code), "  ");
      else
         return (ISO_Language (Code (Code'First .. Code'First + 1)),
                 ISO_Country (Code (Code'First + 3 .. Code'First + 4)));
      end if;
   end To_Language;

end WL.Localisation;
