------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--            W L . T A B L E S . E X I S T E N C E _ T A B L E             --
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

package body WL.Tables.Existence_Table is

   ------------
   -- Exists --
   ------------

   function Exists (In_Table : Table; Name : String) return Boolean is
      Pos : constant Table_Of_Strings.Position :=
        Table_Of_Strings.First (In_Table.The_Table, Name);
   begin
      return Table_Of_Strings.Found (Pos);
   end Exists;

   ------------
   -- Insert --
   ------------

   procedure Insert (To_Table : in out Table; Name : String) is
   begin
      if not Exists (To_Table, Name) then
         Table_Of_Strings.Insert
           (To_Table.The_Table,
            Ada.Strings.Unbounded.To_Unbounded_String (Name));
      end if;
   end Insert;

end WL.Tables.Existence_Table;
