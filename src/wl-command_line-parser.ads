------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--               W L . C O M M A N D _ L I N E . P A R S E R                --
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

--  Instantiate this package with instatiations of the the flags
--  and options packages.  Then call the Enabled function of the
--  flags package and the Has_Value/Get_Value functions of the
--  options package to retrieve your results.

with WL.Command_Line.Flags;
with WL.Command_Line.Options;

generic

   with package Flag_Instan is new WL.Command_Line.Flags (<>);
   with package Option_Instan is new WL.Command_Line.Options (<>);

package WL.Command_Line.Parser is

   pragma Elaborate_Body;

   procedure Parse_Command_Line;

   --  The following functions mimic Ada.Command_Line, but
   --  if called after Parse_Command_Line, they ignore any
   --  options that have already been processed.
   function Argument_Count return Natural;
   function Argument (Index : Positive) return String;

end WL.Command_Line.Parser;
