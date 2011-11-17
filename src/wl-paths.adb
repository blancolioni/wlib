------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                             W L . P A T H S                              --
--                                                                          --
--                                 B o d y                                  --
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

with WL.Strings;
with GNAT.OS_Lib;

package body WL.Paths is

   Separator           : constant array (Character) of Boolean :=
     (':' => True, ' ' => True, ',' => True, others => False);

   Directory_Separator : constant Character :=
     GNAT.OS_Lib.Directory_Separator;

   --------------
   -- Add_Path --
   --------------

   procedure Add_Path
     (For_Path_List : in out Path_List;
      Path          : in String)
   is
      Name : WL.Strings.String_Access;
   begin
      if Path (Path'Last) /= Directory_Separator then
         Name := new String'(Path & Directory_Separator);
      else
         Name := new String'(Path);
      end if;

      WL.Lists.String_List.Append (For_Path_List.Paths, Name);

   end Add_Path;

   ---------------
   -- Add_Paths --
   ---------------
   procedure Add_Paths
     (For_Path_List : in out Path_List;
      Paths         : in String)
   is
      Start, Current : Positive := Paths'First;
   begin
      while Current < Paths'Last loop
         while Current < Paths'Last and then not Separator
           (Paths (Current)) loop
            Current := Current + 1;
         end loop;
         if Current = Paths'Last then
            Add_Path (For_Path_List, Paths (Start .. Current));
         else
            Add_Path (For_Path_List, Paths (Start .. Current - 1));
            while Current < Paths'Last and then Separator
              (Paths (Current)) loop
               Current := Current + 1;
            end loop;
            Start := Current;
         end if;
      end loop;
   end Add_Paths;

   ---------------
   -- Find_File --
   ---------------
   function Find_File (On_Path : in Path_List; Name : in String)
                      return String is
      use WL.Lists.String_List;
      It : Iterator := Get_Start (On_Path.Paths);
   begin
      while not Off_Right (It) loop
         declare
            Full_Name : constant String := Current (It).all & Name;
         begin
            if GNAT.OS_Lib.Is_Regular_File (Full_Name) then
               return Full_Name;
            end if;
         end;
         Next (It);
      end loop;
      return "";
   end Find_File;

   -----------------
   -- Path_Concat --
   -----------------
   function Path_Concat (Left, Right : String) return String is
   begin
      if Left
        (Left'Last) /= Directory_Separator and Right
        (Right'First) /= Directory_Separator then
         return Left & Directory_Separator & Right;
      elsif Left
        (Left'Last) = Directory_Separator and Right
        (Right'First) = Directory_Separator then
         return Left & Right (Right'First + 1 .. Right'Last);
      else
         return Left & Right;
      end if;
   end Path_Concat;

end WL.Paths;
