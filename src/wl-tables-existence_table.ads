------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--            W L . T A B L E S . E X I S T E N C E _ T A B L E             --
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

package WL.Tables.Existence_Table is

   type Table is private;

   procedure Insert (To_Table : in out Table; Name : String);

   function Exists (In_Table : Table; Name : String) return Boolean;

private

   package Table_Of_Strings is
      new WL.Tables.Hash_Table
     (Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded.To_String);

   type Table is
      record
         The_Table : Table_Of_Strings.Table;
      end record;

end WL.Tables.Existence_Table;
