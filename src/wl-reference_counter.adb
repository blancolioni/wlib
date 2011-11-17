------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                 W L . R E F E R E N C E _ C O U N T E R                  --
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

with Ada.Unchecked_Deallocation;

package body WL.Reference_Counter is

   use Ada.Finalization;

   procedure Free is
      new Ada.Unchecked_Deallocation
     (Object => Natural, Name => Reference_Count);

   procedure Initialize (Ref : in out Reference_Controlled) is
   begin
      Ref.References := new Natural'(1);
   end Initialize;

   procedure Adjust (Ref : in out Reference_Controlled) is
   begin
      Ref.References.all := Ref.References.all + 1;
   end Adjust;

   procedure Finalize (Ref : in out Reference_Controlled) is
   begin
      if Ref.References = null then
         --  this shouldn't happen!
         null;
      else
         Ref.References.all := Ref.References.all - 1;
         if Ref.References.all = 0 then
            Free (Ref.References);
            Finalize_Reference (Reference_Controlled'Class (Ref));
         end if;
      end if;

   end Finalize;

end WL.Reference_Counter;
