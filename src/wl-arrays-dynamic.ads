------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                    W L . A R R A Y S . D Y N A M I C                     --
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

--  One annoying restriction is that the Get function returns a
--  value, so it's not possible to say Get (D, 2).Y := 3;
--  I generally instantiate this package with an access type
--  if there's going to be a bunch of updates.

with Ada.Finalization;
with Ada.Streams;

generic
   type Index is (<>);
   type Element is private;
package WL.Arrays.Dynamic is

   pragma Elaborate_Body;

   type Dynamic_Array is private;

   function Get (D : Dynamic_Array; From : Index) return Element;
   --  Get tries to return the element residing at 'From'.
   --  It raises Bounds_Error if From is not a valid index.

   procedure Set
     (D        : in out Dynamic_Array;
      To_Index : in Index;
      Value    : in Element);
   --  Puts Value at To_Index, enlarging the array if necessary.

   procedure Append (D : in out Dynamic_Array; Value : in Element);
   --  Puts Value onto the end of the array, enlarging if necessary.

   function Valid_Index (D : in Dynamic_Array; Idx : in Index) return Boolean;
   --  Returns true if Idx is currently a valid index of D.
   --  i.e. it guarantees that Get (D, Idx) won't raise
   --  Bounds_Error

   function Last (D : Dynamic_Array) return Index;
   --  Returns the largest index in the array.  Raises Bounds_Error
   --  if the array is empty.  Shouldn't it return a 'zero' index?
   --  Well, we don't have one (Index'First - 1 doesn't count).

   function First_Free (D : Dynamic_Array) return Index;
   --  Returns the first free index.  Sort of like Last (D) + 1, except
   --  that it won't barf on a zero-length array.

   function Size (D : Dynamic_Array) return Natural;
   --  Returns the number of elements in the array.

   procedure Drop (D : in out Dynamic_Array; Count : in Natural);
   --  Removes Count elements from the end of the array.

   procedure Set_Size (D : in out Dynamic_Array; New_Size : in Natural);
   --  After calling this procedure, Size (D) will return New_Size.
   --  Use this if you know a minimum size for the array; because
   --  it will run faster.

   procedure Clear (D : in out Dynamic_Array);
   --  Removes everything from the array, and sets the size to zero.

   type Element_Action is access procedure (Item : in out Element);

   procedure For_All_Elements
     (Of_Array : in out Dynamic_Array;
      Action   : in Element_Action);
   --  Execute Action on each element in Of_Array

private
--  Implementation notes:
   --  The array is implementated as an access to a regular Ada
   --  array type, which gets reallocated as the size grows
   --  and shrinks.  Maximum memory requirements for N elements
   --  are O(1.5 * 2(log N + 1)) I think.

   type Array_List is array (Positive range <>) of Element;
   type Array_Access is access Array_List;
   type Dynamic_Array is new Ada.Finalization.Controlled with
      record
         Element_List : Array_Access;
         Length       : Natural;
      end record;

   --  Finalization is quite important.  However, if (as I do)
   --  you use an access type as the element type, be careful.
   procedure Initialize (D : in out Dynamic_Array);
   procedure Adjust (D : in out Dynamic_Array);
   procedure Finalize (D : in out Dynamic_Array);

   --  Friendly stream I/O.
   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Dynamic_Array);
   for Dynamic_Array'Write use Write;

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Dynamic_Array);
   for Dynamic_Array'Read use Read;

   --  I inline the following for the heck of it; I wouldn't be
   --  surprised if GNAT just as good a job without hacky hints
   --  like this.  At least in Ada it's just a pragma, rather than
   --  an ACTUAL DARN KEYWORD like in other languages (didn't
   --  they learn from the 'register' ballsup?)

   pragma Inline (Get);
   pragma Inline (Set);
   pragma Inline (Size);
   pragma Inline (Drop);
   pragma Inline (Last);
   pragma Inline (First_Free);

end WL.Arrays.Dynamic;
