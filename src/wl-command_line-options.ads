------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--              W L . C O M M A N D _ L I N E . O P T I O N S               --
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

generic

   type String_Option is (<>);
   type String_Set is array (String_Option) of Boolean;

   Short_Options      : String;
   Long_Options       : String;
   Compressed_Options : String_Set;

package WL.Command_Line.Options is

   pragma Elaborate_Body;

   procedure Set_Value (Option : String_Option; Value : String);
   function Get_Value (Option : String_Option) return String;
   function Get_Value (Option : String_Option) return Natural;
   function Has_Value (Option : String_Option) return Boolean;

   procedure Parse_Option
     (Arg    : in String;
      Found  : out Boolean;
      Skip   : out Boolean;
      Arg_No : in Positive);

end WL.Command_Line.Options;
