------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                           W L . B U I L D E R                            --
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

pragma Warnings (Off);
--  this is just an empty main program used to
--  force all the library components to be built.

with WL;
with WL.Arrays;
with WL.Arrays.Dynamic;
with WL.Arrays.Exceptions;
with WL.Caches;
with WL.Command_Line;
with WL.Command_Line.Flags;
with WL.Command_Line.Options;
with WL.Command_Line.Parser;
with WL.Debug;
with WL.File_Manager;
with WL.Image;
with WL.Lists;
with WL.Lists.Generic_List;
with WL.Lists.String_List;
with WL.Paths;
with WL.Queues.Basic;
with WL.Random;
with WL.Reference_Counter;
with WL.Sets;
with WL.Sets.Bounded;
with WL.Sets.Strings;
with WL.Sets.Unbounded;
with WL.Sorts.Insertion_Sort;
with WL.Stacks;
with WL.Strings;
with WL.Strings.String_List;
with WL.Tables;
with WL.Tables.Hash_Table;
with WL.Tables.Existence_Table;
with WL.Trace;
with WL.Trees;
with WL.Trees.Binary;
with WL.Trees.Generic_Trees;
with WL.Version;

procedure WL.Builder is
begin
   null;
end WL.Builder;
