------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                      W L . S E T S . S T R I N G S                       --
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

package body WL.Sets.Strings is

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Set) return Boolean is
   begin
      return Table_Of_Strings.Not_Found
        (Table_Of_Strings.First (S.String_Table));
   end Is_Empty;

   ----------
   -- "<=" --
   ----------

   function "<=" (E : String; S : Set) return Boolean is
   begin
      return Table_Of_Strings.Found
        (Table_Of_Strings.First (S.String_Table, E));
   end "<=";

   ---------
   -- Add --
   ---------

   procedure Add (S : in out Set; E : String) is
   begin
      Table_Of_Strings.Insert
        (S.String_Table,
         (Text => Ada.Strings.Unbounded.To_Unbounded_String (E)));
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (S : in out Set; E : String) is
   begin
      Table_Of_Strings.Delete
        (S.String_Table,
         (Text => Ada.Strings.Unbounded.To_Unbounded_String (E)));
   end Remove;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (From : Element_Type) return String is
   begin
      return Ada.Strings.Unbounded.To_String (From.Text);
   end Get_Text;

end WL.Sets.Strings;
