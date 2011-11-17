------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--               W L . C O M M A N D _ L I N E . P A R S E R                --
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

with Ada.Command_Line;

with WL.Lists.String_List;

package body WL.Command_Line.Parser is

   Args         : WL.Lists.String_List.List;

   procedure Parse_Command_Line is

      Found : Boolean;
      Skip  : Boolean := False;

   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         if not Skip then
            Argument_Search : declare
               Arg : constant String := Ada.Command_Line.Argument (I);
            begin
               if Arg (Arg'First) /= '-' then
                  WL.Lists.String_List.Append (Args, new String'(Arg));
               else
                  if Flag_Instan.Parse_Flag (Arg) then
                     null;
                  else
                     Option_Instan.Parse_Option (Arg, Found, Skip, I);
                     if not Found then
                        raise Constraint_Error with Arg;
                     end if;
                  end if;
               end if;
            end Argument_Search;
         else
            Skip := False;
         end if;
      end loop;

   end Parse_Command_Line;

   function Argument_Count return Natural is
   begin
      return WL.Lists.String_List.Length (Args);
   end Argument_Count;

   function Argument (Index : Positive) return String is
   begin
      return WL.Lists.String_List.Get (Args, Index).all;
   end Argument;

end WL.Command_Line.Parser;
