with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;

with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;

with Ada.Strings.Unbounded.Hash;

with Ada.Directories;

package body WL.File_Manager is

   package List_Of_Paths is
      new Ada.Containers.Doubly_Linked_Lists (Unbounded_String);

   type Path_List_Access is access all List_Of_Paths.List'Class;

   package Table_Of_Class_Entries is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Path_List_Access,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=");

   File_Classes : Table_Of_Class_Entries.Map;

   ----------------------
   -- Add_Include_Path --
   ----------------------

   procedure Add_Include_Path (Class : String;
                               Path  : String) is
      U_Class : constant Unbounded_String :=
                  To_Unbounded_String (Class);
   begin
      if not File_Classes.Contains (U_Class) then
         File_Classes.Insert (U_Class,
                              new List_Of_Paths.List);
      end if;
      File_Classes.Element (U_Class).Append
        (To_Unbounded_String (Path));
   end Add_Include_Path;

   ---------------
   -- Find_File --
   ---------------

   function Find_File (Class     : String;
                       File_Name : String)
                       return String
   is
      use List_Of_Paths;
      U_Class : constant Unbounded_String := To_Unbounded_String (Class);
      It : Cursor;
   begin
      if not File_Classes.Contains (U_Class) then
         return "";
      end if;

      It := File_Classes.Element (U_Class).First;
      while Has_Element (It) loop
         declare
            use type Ada.Directories.File_Kind;
            Path_Try : constant String :=
                         Ada.Directories.Compose
                           (To_String (Element (It)),
                            File_Name);
         begin
            if Ada.Directories.Kind (Path_Try)
              = Ada.Directories.Ordinary_File
            then
               return Path_Try;
            end if;
         end;
         Next (It);
      end loop;

      return "";
   end Find_File;

end WL.File_Manager;
