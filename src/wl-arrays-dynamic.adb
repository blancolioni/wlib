------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                    W L . A R R A Y S . D Y N A M I C                     --
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

with WL.Arrays.Exceptions;

package body WL.Arrays.Dynamic is


   Block_Size : constant := 16;

   procedure Free is
      new Ada.Unchecked_Deallocation (Array_List, Array_Access);


   procedure Adjust (D : in out Dynamic_Array) is
      Old_Element_List : constant Array_Access := D.Element_List;
   begin
      D.Element_List := new Array_List (Old_Element_List.all'Range);
      D.Element_List (1 .. D.Length) :=
        Old_Element_List (1 .. D.Length);
   end Adjust;

   procedure Finalize (D : in out Dynamic_Array) is
   begin
      Free (D.Element_List);
   end Finalize;

   procedure Size_To_Fit (D : in out Dynamic_Array; Size : in Natural);

   procedure Initialize (D : in out Dynamic_Array) is
   begin
      --  I don't know why you'd index a dynamic array
      --  with a small index type, but it doesn't really
      --  matter (except that up to Block_Size elements
      --  are wasted (tops)), so we don't worry about that.

      D :=
        (Ada.Finalization.Controlled with
         Element_List => new Array_List (1 .. Block_Size),
         Length => 0);

   end Initialize;

   procedure Clear (D : in out Dynamic_Array) is
      New_D : Dynamic_Array;
   begin
      New_D.Length := 0;
      --  to shut the compiler up
      D := New_D;
   end Clear;

   function Get (D : Dynamic_Array; From : Index) return Element is
   begin
      if Index'Pos (From) - Index'Pos (Index'First) + 1 > D.Length then
         raise WL.Arrays.Exceptions.Bounds_Error;
      end if;

      return D.Element_List
        (Index'Pos (From) - Index'Pos (Index'First) + 1);
   end Get;

   procedure Size_To_Fit (D : in out Dynamic_Array; Size : in Natural) is
   begin
      if Size > Block_Size and then
        Size not in D.Element_List.all'Last / 2 .. D.Element_List.all'Last
      then
         declare
            New_Size : Natural := D.Element_List.all'Last;
            Old_List : Array_Access := D.Element_List;
         begin

            while New_Size < Size loop
               New_Size := 2 * New_Size;
            end loop;

            while New_Size > Size * 2 loop
               New_Size := New_Size / 2;
            end loop;

            if New_Size < Block_Size then
               New_Size := Block_Size;
            end if;

            --            if D.Length > New_Size then
            --               D.Length := New_Size;
            --            end if;

            D.Element_List := new Array_List (1 .. New_Size);
            if New_Size > Old_List.all'Length then
               D.Element_List (Old_List.all'Range) := Old_List.all;
            else
               D.Element_List.all := Old_List (1 .. New_Size);
            end if;
            Free (Old_List);
         end;
      end if;
   end Size_To_Fit;

   procedure Set
     (D        : in out Dynamic_Array;
      To_Index : in Index;
      Value    : in Element)
   is
   begin
      if Index'Pos
        (To_Index) - Index'Pos (Index'First) + 1 > D.Length then
         D.Length :=
           Index'Pos (To_Index) - Index'Pos (Index'First) + 1;
         Size_To_Fit (D, D.Length);
      end if;
      D.Element_List
        (Index'Pos (To_Index) - Index'Pos (Index'First) + 1) := Value;
   end Set;

   procedure Append (D : in out Dynamic_Array; Value : in Element) is
   begin
      Set (D, Index'Val (D.Length + Index'Pos (Index'First)), Value);
   end Append;

   function Valid_Index (D : in Dynamic_Array;
                         Idx : in Index)
                        return Boolean
   is
   begin
      return Index'Pos (Idx) - Index'Pos (Index'First) + 1 <= D.Length;
   end Valid_Index;

   function Last (D : Dynamic_Array) return Index is
   begin
      if D.Length = 0 then
         raise WL.Arrays.Exceptions.Bounds_Error;
      end if;

      return Index'Val (D.Length + Index'Pos (Index'First) - 1);
   end Last;

   function First_Free (D : Dynamic_Array) return Index is
   begin
      --  the following check is commented out because
      --  gnat doesn't seem to do it properly.
      --  if D.Length > Index'Pos (Index'Last) - Index'Pos (Index'First) + 1
      --  then
      --     raise WL.Arrays.Exceptions.Bounds_Error;
      --  end if;

      return Index'Val (D.Length + Index'Pos (Index'First));

   end First_Free;

   function Size (D : Dynamic_Array) return Natural is
   begin
      return D.Length;
   end Size;

   procedure Drop (D : in out Dynamic_Array; Count : in Natural) is
   begin
      if Count > D.Length then
         raise WL.Arrays.Exceptions.Bounds_Error;
      end if;
      D.Length := D.Length - Count;
      Size_To_Fit (D, D.Length);
   end Drop;

   procedure Set_Size (D : in out Dynamic_Array; New_Size : Natural) is
   begin
      D.Length := New_Size;
      Size_To_Fit (D, D.Length);
   end Set_Size;


   ----------------------
   -- For_All_Elements --
   ----------------------
   procedure For_All_Elements
     (Of_Array : in out Dynamic_Array;
      Action   : in Element_Action)
   is
   begin
      for I in 1 .. Of_Array.Length loop
         Action (Of_Array.Element_List (I));
      end loop;
   end For_All_Elements;

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Dynamic_Array)
   is
   begin
      Integer'Write (Stream, Item.Length);
      for I in 1 .. Item.Length loop
         Element'Write (Stream, Item.Element_List (I));
      end loop;
   end Write;

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Dynamic_Array)
   is
      Last : Natural;
      E    : Element;
   begin
      Integer'Read (Stream, Last);
      for I in 1 .. Last loop
         Element'Read (Stream, E);
         Set (Item, Index'Val (I + Index'Pos (Index'First) - 1), E);
      end loop;
   end Read;

end WL.Arrays.Dynamic;
