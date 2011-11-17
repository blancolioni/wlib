------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                      W L . S E T S . S T R I N G S                       --
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

with Ada.Strings.Unbounded;
with WL.Tables.Hash_Table;
--  what a giveaway!
pragma Elaborate_All (WL.Tables.Hash_Table);

package WL.Sets.Strings is

   pragma Elaborate_Body;

   type Set is limited private;
   --  sorry about the limited bit

   function Is_Empty (S : Set) return Boolean;

   function "<=" (E : String; S : Set) return Boolean;

   procedure Add (S : in out Set; E : String);
   procedure Remove (S : in out Set; E : String);

private
   type Element_Type is
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Get_Text (From : Element_Type) return String;

   package Table_Of_Strings is
      new WL.Tables.Hash_Table (Element_Type, Get_Text);

   type Set is
      record
         String_Table : Table_Of_Strings.Table;
      end record;

end WL.Sets.Strings;
