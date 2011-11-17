------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                      W L . T R E E S . B I N A R Y                       --
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

package body WL.Trees.Binary is

   procedure Free is
      new Ada.Unchecked_Deallocation (Tree_Node, Tree);

   --  Implementation from Cormen, Leiserson & Rivest "Introduction
   --  to Algorithms" chapter 13

   ------------
   -- Insert --
   ------------
   procedure Insert (T : in out Tree; E : in Element) is
      X : Tree := T;
      Y : Tree := null;
      Z : Tree := new Tree_Node'(null, E, null, null);
   begin
      while X /= null loop
         Y := X;
         if Get_Key (E) < Get_Key (X.Value) then
            X := X.Left;
         else
            X := X.Right;
         end if;
      end loop;

      Z.Parent := Y;

      if Y = null then
         T := Z;
      elsif Get_Key (Z.Value) < Get_Key (Y.Value) then
         Y.Left := Z;
      else
         Y.Right := Z;
      end if;
   end Insert;

   ------------
   -- Delete --
   ------------
   procedure Delete (T : in out Tree; P : in Position) is
      X : Tree;
      Y : Tree;
      Z : Tree := Tree (P);
   begin
      if Z.Left = null or Z.Right = null then
         Y := Z;
      else
         Y := Tree (Next (P));
      end if;

      if Y.Left /= null then
         X := Y.Left;
      else
         X := Y.Right;
      end if;

      if X /= null then
         X.Parent := Y.Parent;
      end if;

      if Y.Parent = null then
         T := X;
      elsif Y = Y.Parent.Left then
         Y.Parent.Left := X;
      else
         Y.Parent.Right := X;
      end if;

      if Y /= Z then
         Z.Value := Y.Value;
      end if;

      Free (Y);

   end Delete;

   ------------
   -- Search --
   ------------
   function Search (T : in Tree; E : in Element) return Position is
   begin
      return Search (T, Get_Key (E));
   end Search;

   ------------
   -- Search --
   ------------
   function Search (T : in Tree; K : in Key) return Position is
      X : Tree := T;
   begin
      while X /= null and then Get_Key (X.Value) /= K loop
         if K < Get_Key (X.Value) then
            X := X.Left;
         else
            X := X.Right;
         end if;
      end loop;

      return Position (X);
   end Search;

   ------------
   -- Search --
   ------------
   function Search (T : in Tree; K : in Key) return Element is
   begin
      return Contents (Search (T, K));
   end Search;

   ---------------
   -- Search_GE --
   ---------------
   function Search_Ge (T : in Tree; K : in Key) return Position is
      X : Tree := T;
      Y : Tree := null;
   begin
      while X /= null and then Get_Key (X.Value) /= K loop
         Y := X;
         if K < Get_Key (X.Value) then
            X := X.Left;
         else
            X := X.Right;
         end if;
      end loop;

      if X = null then
         return Position (Y);
      else
         return Position (X);
      end if;

   end Search_Ge;


   -----------
   -- First --
   -----------
   function First (T : in Tree) return Position is
      X : Tree := T;
   begin
      if X /= null then
         while X.Left /= null loop
            X := X.Left;
         end loop;
      end if;

      return Position (X);
   end First;

   function Next (P : in Position) return Position is
      X : Tree := Tree (P);
      Y : Tree;
   begin
      if X.Right /= null then
         return First (X.Right);
      end if;

      Y := X.Parent;
      while Y /= null and then X = Y.Right loop
         X := Y;
         Y := Y.Parent;
      end loop;

      return Position (Y);
   end Next;

   function Contents (P : Position) return Element is
   begin
      return P.Value;
   end Contents;

end WL.Trees.Binary;
