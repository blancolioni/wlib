------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                             W L . P A T H S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (c) 2004 Fraser Wilson                      --
--                                                                          --
-- WLib is free software; you can redistribute it and/or  modify  it  under --
-- terms  of  the  GNU  General  Public  License  as  published by the Free --
-- Software Foundation; either version 2, or (at  your  option)  any  later --
-- version.  WLib  is  distributed  in the hope that it will be useful, but --
-- WITHOUTANY WARRANTY; without even the implied warranty of  MERCHANTABIL- --
-- ITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details. You should have received a  copy  of  the  GNU --
-- General  Public License distributed with WLib; see file COPYING. If not, --
-- write to the Free Software Foundation, 59  Temple  Place  -  Suite  330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

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
