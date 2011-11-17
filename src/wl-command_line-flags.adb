------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                W L . C O M M A N D _ L I N E . F L A G S                 --
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

package body WL.Command_Line.Flags is

   Set : array (Flag_Option) of Boolean := (others => False);

   -------------
   -- Disable --
   -------------

   procedure Disable (Flag : Flag_Option) is
   begin
      Set (Flag) := False;
   end Disable;

   ------------
   -- Enable --
   ------------

   procedure Enable (Flag : Flag_Option) is
   begin
      Set (Flag) := True;
   end Enable;

   -------------
   -- Enabled --
   -------------

   function Enabled (Flag : Flag_Option) return Boolean is
   begin
      return Set (Flag);
   end Enabled;

   ----------------
   -- Parse_Flag --
   ----------------

   function Parse_Flag (Arg : String) return Boolean is
      Start : Positive;
      Index : Positive;
      Flag  : Flag_Option;
      Found : Boolean;
   begin
      if Arg'Length > 1 and then Arg (Arg'First + 1) = '-' then
         --  Long option
         Start := Long_Options'First;
         Index := Start;
         Flag := Flag_Option'First;
         Found := False;

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
              Flag = Flag_Option'Last;

            Flag := Flag_Option'Succ (Flag);
            Index := Index + 1;
            Start := Index;
         end loop;

         if not Found then
            return False;
         else
            Set (Flag) := True;
            return True;
         end if;

      elsif Arg'Length = 2 then
         --  short option
         Flag := Flag_Option'First;
         Found := False;
         for I in Short_Options'Range loop
            if Short_Options (I) = Arg (Arg'First + 1) then
               Found := True;
               exit;
            end if;
            exit when Flag = Flag_Option'Last;
            Flag := Flag_Option'Succ (Flag);
         end loop;

         if not Found then
            return False;
         else
            Set (Flag) := True;
            return True;
         end if;
      else
         return False;
      end if;

   end Parse_Flag;

end WL.Command_Line.Flags;
