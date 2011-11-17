------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                           W L . S T R I N G S                            --
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

--  This package puts a few useful properties around a string access
--  type.  I use it quite heavily when strings are computed at run
--  time, but are reasonably static thereafter.

with Ada.Streams;

package WL.Strings is

   pragma Preelaborate;

   type String_Access is access all String;
   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      S      : out String_Access);
   for String_Access'Read use Read;

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      S      : in String_Access);
   for String_Access'Write use Write;

   function "+" (S : String) return String_Access;
   function "-" (S : String_Access) return String;
   function "&" (S1 : String; S2 : String_Access) return String;
   function "&" (S1 : String_Access; S2 : String) return String;
   function "&" (S1 : String_Access; S2 : String_Access) return String;
   function "&" (S : String_Access; Ch : Character) return String;
   function "&" (Ch : Character; S : String_Access) return String;
   procedure Free (S : in out String_Access);

private
   pragma Inline ("&");
   pragma Inline ("+");
   pragma Inline ("-");

end WL.Strings;
