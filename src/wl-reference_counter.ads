------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                 W L . R E F E R E N C E _ C O U N T E R                  --
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

with Ada.Finalization;

package WL.Reference_Counter is

   pragma Elaborate_Body;

   type Reference_Controlled is abstract tagged private;

   procedure Finalize_Reference (R : in out Reference_Controlled) is
   abstract;

private
   type Reference_Count is access Natural;

   type Reference_Controlled is
     abstract new Ada.Finalization.Controlled with
      record
         References : Reference_Count;
      end record;

   procedure Initialize (Ref : in out Reference_Controlled);
   procedure Adjust (Ref : in out Reference_Controlled);
   procedure Finalize (Ref : in out Reference_Controlled);

end WL.Reference_Counter;
