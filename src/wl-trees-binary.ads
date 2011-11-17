------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                      W L . T R E E S . B I N A R Y                       --
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
   type Key is private;
   type Element is private;
   with function Get_Key (E : Element) return Key is <>;
   with function "<" (Left, Right : Key) return Boolean is <>;
package WL.Trees.Binary is

   pragma Elaborate_Body;

   type Tree is private;
   type Position is private;

   Empty     : constant Tree;
   Not_Found : constant Position;

   procedure Insert (T : in out Tree; E : in Element);
   procedure Delete (T : in out Tree; P : in Position);

   function Search (T : in Tree; E : in Element) return Position;
   function Search (T : in Tree; K : in Key) return Position;
   function Search_Ge (T : in Tree; K : in Key) return Position;
   function Search (T : in Tree; K : in Key) return Element;
   function First (T : in Tree) return Position;
   function Next (P : in Position) return Position;

   function Contents (P : Position) return Element;

private
   type Tree_Node;

   type Tree is access Tree_Node;

   type Tree_Node is
      record
         Parent : Tree;
         Value  : Element;
         Left   : Tree;
         Right  : Tree;
      end record;

   type Position is new Tree;

   Empty     : constant Tree := null;
   Not_Found : constant Position := null;

end WL.Trees.Binary;
