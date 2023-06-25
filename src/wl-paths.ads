with WL.Lists.String_List;

--  WL.Paths: maintains a list of search paths


package WL.Paths is
   pragma Elaborate_Body;

   type Path_List is private;

   --  Adds a single path to the path list
   procedure Add_Path
     (For_Path_List : in out Path_List;
      Path          : in String);

   --  Adds a number of paths.  The Paths parameter should
   --  contain a sequence of paths separated by spaces,
   --  colons or commas.  Single and double quotes are
   --  recognised correctly.
   procedure Add_Paths
     (For_Path_List : in out Path_List;
      Paths         : in String);

   --  Returns a string containing the full path + file name.
   --  Returns "" if the file could not be found.
   function Find_File (On_Path : in Path_List; Name : in String) return String;

   --  Concatenates two paths together, and inserts or removes
   --  a path separator if necessary.
   function Path_Concat (Left, Right : String) return String;

private
   type Path_List is
      record
         Paths : WL.Lists.String_List.List;
      end record;

end WL.Paths;
