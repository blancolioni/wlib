------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--              W L . C O M M A N D _ L I N E . O P T I O N S               --
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

with WL.Strings;
use WL.Strings;

package body WL.Command_Line.Options is

   Values : array (String_Option) of String_Access := (others => null);

   function Get_Value (Option : String_Option) return String is
   begin
      if Values (Option) /= null then
         return Values (Option).all;
      else
         return "";
      end if;
   end Get_Value;

   function Get_Value (Option : String_Option) return Natural is
   begin
      if Values (Option) /= null then
         return Natural'Value (Values (Option).all);
      else
         return 0;
      end if;
   exception
      when Constraint_Error =>
         return 0;
   end Get_Value;

   function Has_Value (Option : String_Option) return Boolean is
   begin
      return Values (Option) /= null;
   end Has_Value;

   procedure Parse_Option
     (Arg    : in String;
      Found  : out Boolean;
      Skip   : out Boolean;
      Arg_No : in Positive)
   is
      Start  : Positive;
      Index  : Positive;
      Option : String_Option;

   begin

      Skip := False;

      if Arg'Length > 1 and then Arg (Arg'First + 1) = '-' then
         --  Long option
         Start := Long_Options'First;
         Index := Start;
         Found := False;
         Option := String_Option'First;

         loop

            while Index <= Long_Options'Last and then Long_Options
              (Index) /= ' ' loop
               Index := Index + 1;
            end loop;

            Found :=
              Long_Options
              (Start .. Index - 1) = Arg (Arg'First + 2 .. Arg'Last);

            exit when Found or else
              Index > Long_Options'Last or else
              Option = String_Option'Last;

            Option := String_Option'Succ (Option);
            Index := Index + 1;
            Start := Index;

         end loop;

         if Found then
            if Arg_No < Ada.Command_Line.Argument_Count then
               Set_Value
                 (Option, Ada.Command_Line.Argument (Arg_No + 1));
               Skip := True;
            else
               Found := False;
            end if;

         end if;

      elsif Arg'Length > 1 and then Arg (Arg'First) = '-' then
         --  short option
         Option := String_Option'First;
         Found := False;
         for I in Short_Options'Range loop

            if Short_Options (I) = Arg (Arg'First + 1) then
               Found := True;
               exit;
            end if;

            exit when Option = String_Option'Last;
            Option := String_Option'Succ (Option);

         end loop;

         if Found then
            if Compressed_Options (Option) then
               if Arg'Length < 3 then
                  Found := False;
               else
                  Set_Value (Option, Arg (Arg'First + 2 .. Arg'Last));
               end if;
            else
               if Arg_No < Ada.Command_Line.Argument_Count then
                  Set_Value
                    (Option, Ada.Command_Line.Argument (Arg_No + 1));
                  Skip := True;
               else
                  Found := False;
               end if;
            end if;
         end if;

      else
         Found := False;
      end if;

   end Parse_Option;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Option : String_Option; Value : String) is
   begin
      if Values (Option) /= null then
         Free (Values (Option));
      end if;
      Values (Option) := new String'(Value);
   end Set_Value;

end WL.Command_Line.Options;
