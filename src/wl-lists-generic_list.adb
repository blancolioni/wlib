------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                W L . L I S T S . G E N E R I C _ L I S T                 --
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

package body WL.Lists.Generic_List is

   procedure Free is
      new Ada.Unchecked_Deallocation (List_Node, List_Node_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation
     (List_Record, List_Record_Access);
   procedure Free is
      new Ada.Unchecked_Deallocation (Element, Element_Access);

   procedure New_List (L : out List) is
   begin
      Free_List (L);
      L.Lst :=
        new List_Record'(First => null, Last => null, Count => 0);
   end New_List;

   function New_List return List is
      Result : List;
   begin
      New_List (Result);
      return Result;
   end New_List;

   procedure Free_List (L : in out List) is
      N : List_Node_Ptr := L.Lst.First;
      T : List_Node_Ptr;
   begin
      while N /= null loop
         T := N.Next;
         Free (N.E);
         Free (N);
         N := T;
      end loop;
      Free (L.Lst);
   end Free_List;

   procedure Initialize (L : in out List) is
   begin
      L.Lst :=
        new List_Record'(First => null, Last => null, Count => 0);
   end Initialize;

   procedure Adjust (L : in out List) is
      New_Lst : List_Record_Access;
      F, T    : List_Node_Ptr;
   begin
      New_Lst :=
        new List_Record'(First => null, Last => null,
                         Count => L.Lst.Count);
      if L.Lst.First /= null then
         F := L.Lst.First;
         New_Lst.First := new List_Node'(F.all);
         New_Lst.First.E := new Element'(F.E.all);
         T := New_Lst.First;
         while F.Next /= null loop
            T.Next := new List_Node'(F.Next.all);
            T.Next.E := new Element'(F.Next.E.all);
            T.Next.Prev := T;
            T := T.Next;
            F := F.Next;
         end loop;
         New_Lst.Last := T;
      end if;
      L.Lst := New_Lst;
   end Adjust;

   procedure Finalize (L : in out List) is
   begin
      Free_List (L);
   end Finalize;

   function Empty (L : in List) return Boolean is
   begin
      return L.Lst.First = null;
   end Empty;

   procedure Insert (L : in out List_Record; E : Element) is
   begin
      L.First := new List_Node'(new Element'(E), L.First, null);
      if L.First.Next /= null then
         L.First.Next.Prev := L.First;
      end if;
      if L.Last = null then
         L.Last := L.First;
      end if;
      L.Count := L.Count + 1;
   end Insert;

   procedure Insert (L : in out List; E : Element) is
   begin
      Insert (L.Lst.all, E);
   end Insert;

   procedure Append (L : in out List_Record; E : Element) is
   begin
      if L.Last = null then
         Insert (L, E);
      else
         L.Last.Next := new List_Node'(new Element'(E), null, L.Last);
         L.Last := L.Last.Next;
         L.Count := L.Count + 1;
      end if;
   end Append;

   procedure Append (L : in out List; E : Element) is
   begin
      Append (L.Lst.all, E);
   end Append;

   function Length (L : in List) return Natural is
   begin
      return L.Lst.Count;
   end Length;

   function First (L : in List) return Element is
   begin
      return L.Lst.First.E.all;
   end First;

   function Last (L : in List) return Element is
   begin
      return L.Lst.Last.E.all;
   end Last;

   function Get (L : in List; Entry_No : Natural) return Element is
      It : constant Iterator := Get_Entry (L, Entry_No);
   begin
      return Current (It);
   end Get;

   function Copy (L : in List) return List is
      Result : List;
      It     : Iterator := Get_Start (L);
   begin
      while not Off_Right (It) loop
         Append (Result, Current (It));
         Next (It);
      end loop;
      return Result;
   end Copy;

   function Head (L : in List) return Element is
   begin
      if L.Lst.Count = 0 then
         raise List_Error;
      end if;

      return L.Lst.First.E.all;
   end Head;

   function Tail (L : in List) return List is
   begin
      if L.Lst.Count = 0 then
         raise List_Error;
         return L;
      --  not executed
      else
         return (Ada.Finalization.Controlled with
                 Lst => new List_Record'(First => L.Lst.First.Next,
                                         Last  => L.Lst.Last,
                                         Count => L.Lst.Count - 1));
      end if;
   end Tail;

   function Get_Start (L : in List) return Iterator is
   begin
      return (L.Lst, L.Lst.First, L.Lst.First = null,
              L.Lst.First = null,
              1 - Boolean'Pos (L.Lst.First = null));
   end Get_Start;

   function Get_End (L : in List) return Iterator is
   begin
      return (L.Lst, L.Lst.Last, L.Lst.Last = null, L.Lst.Last = null,
              L.Lst.Count);
   end Get_End;

   function Get_Entry (L : in List; Entry_No : Natural) return Iterator is
      It : Iterator := Get_Start (L);
   begin
      Goto_Entry (It, Entry_No);
      return It;
   end Get_Entry;

   function Current (It : in Iterator) return Element is
   begin
      if It.Curr /= null then
         return It.Curr.E.all;
      else
         raise List_Error;
      end if;
   end Current;

   function Position (It : in Iterator) return Positive is
   begin
      return It.Position;
   end Position;

   procedure Goto_Start (It : in out Iterator) is
   begin
      if It.It_List.Count = 0 then
         raise List_Error;
      else
         It.Curr := It.It_List.First;
         It.Off_L := False;
         It.Off_R := False;
         It.Position := 1;
      end if;
   end Goto_Start;


   procedure Goto_End (It : in out Iterator) is
   begin
      if It.It_List.Count = 0 then
         raise List_Error;
      else
         It.Curr := It.It_List.Last;
         It.Off_L := False;
         It.Off_R := False;
         It.Position := It.It_List.Count;
      end if;
   end Goto_End;

   procedure Next (It : in out Iterator) is
   begin
      if It.It_List.Count = 0 or It.Off_R then
         raise List_Error;
      elsif It.Off_L then
         It.Off_L := False;
         It.Curr := It.It_List.First;
         It.Position := 1;
      else
         It.Curr := It.Curr.Next;
         It.Position := It.Position + 1;
         if It.Curr = null then
            It.Off_R := True;
         end if;
      end if;
   end Next;

   procedure Prev (It : in out Iterator) is
   begin
      if It.It_List.Count = 0 or It.Off_L then
         raise List_Error;
      elsif It.Off_R then
         It.Off_R := False;
         It.Curr := It.It_List.Last;
         It.Position := It.It_List.Count;
      else
         It.Curr := It.Curr.Prev;
         It.Position := It.Position - 1;
         if It.Curr = null then
            It.Off_L := True;
         end if;
      end if;
   end Prev;

   function Next (It : in Iterator) return Iterator is
      New_It : Iterator := It;
   begin
      Next (New_It);
      return New_It;
   end Next;

   function Prev (It : in Iterator) return Iterator is
      New_It : Iterator := It;
   begin
      Prev (New_It);
      return New_It;
   end Prev;

   procedure Goto_Entry (It : in out Iterator; Ent : Natural) is
   begin
      if It.Position = Ent then
         null;
      elsif Ent <= 1 then
         Goto_Start (It);
         if Ent = 0 then
            Prev (It);
         end if;
      elsif Ent >= It.It_List.Count then
         Goto_End (It);
         if Ent > It.It_List.Count then
            Next (It);
         end if;
      elsif Ent > It.It_List.Count / 2 then
         Goto_End (It);
         for I in Ent + 1 .. It.It_List.Count loop
            Prev (It);
         end loop;
      else
         Goto_Start (It);
         for I in 1 .. Ent - 1 loop
            Next (It);
         end loop;
      end if;
   end Goto_Entry;

   function At_Start (It : in Iterator) return Boolean is
   begin
      return It.Position = 1;
   end At_Start;

   function At_End (It : in Iterator) return Boolean is
   begin
      return It.Position = It.It_List.Count;
   end At_End;

   function Off_Right (It : in Iterator) return Boolean is
   begin
      return It.Off_R;
   end Off_Right;

   function Off_Left (It : in Iterator) return Boolean is
   begin
      return It.Off_L;
   end Off_Left;

   procedure Replace (It : in out Iterator; E : Element) is
   begin
      It.Curr.E.all := E;
   end Replace;

   procedure Delete (It : in out Iterator) is
      N : List_Node_Ptr := It.Curr;
   begin
      if It.It_List.Count = 0 or It.Curr = null then
         raise List_Error;
      end if;

      if N = It.It_List.First then
         It.It_List.First := It.It_List.First.Next;
      end if;

      if N = It.It_List.Last then
         It.It_List.Last := It.It_List.Last.Prev;
      end if;

      if N.Prev /= null then
         N.Prev.Next := N.Next;
      end if;

      if N.Next /= null then
         N.Next.Prev := N.Prev;
      end if;

      It.Curr := N.Next;

      if It.Curr = null then
         It.Off_R := True;
      end if;

      Free (N.E);
      Free (N);
      It.It_List.Count := It.It_List.Count - 1;

   end Delete;

   procedure Insert (It : in out Iterator; E : in Element) is
   begin
      if Off_Left (It) then
         raise List_Error;
      elsif Off_Right (It) then
         Append (It.It_List.all, E);
      elsif At_Start (It) then
         Insert (It.It_List.all, E);
      else
         declare
            New_Node : List_Node_Ptr;
         begin
            New_Node :=
              new List_Node'(new Element'(E), It.Curr, It.Curr.Prev);
            It.Curr.Prev := New_Node;
            New_Node.Prev.Next := New_Node;
            It.It_List.Count := It.It_List.Count + 1;
            It.Position := It.Position + 1;
         end;
      end if;
   end Insert;

   procedure Insert_After (It : in out Iterator; E : in Element) is
   begin
      if Off_Right (It) then
         raise List_Error;
      else
         Next (It);
         Insert (It, E);
      end if;
   end Insert_After;

   function Get_Mark (It : in Iterator) return Mark is
   begin
      return Mark (It);
   end Get_Mark;

   function Get_Element (M : in Mark) return Element is
   begin
      if M.Curr = null then
         raise List_Error;
      else
         return M.Curr.E.all;
      end if;
   end Get_Element;

   procedure Set_Element (M : in Mark; E : in Element) is
   begin
      if M.Curr = null then
         raise List_Error;
      else
         M.Curr.E.all := E;
      end if;
   end Set_Element;


   procedure Goto_Mark (It : in out Iterator; M : in Mark) is
   begin
      It := Iterator (M);
   end Goto_Mark;

   function Off_Right (M : in Mark) return Boolean is
   begin
      return M.Off_R;
   end Off_Right;

   function Off_Left (M : in Mark) return Boolean is
   begin
      return M.Off_L;
   end Off_Left;

   function Image (L : List; Element_Image : Imager) return String is

      function Img (It : access Iterator; Index : Positive) return String is
         S : constant String :=
           Positive'Image (Index) & ':' &
           Element_Image (Current (It.all));
      begin
         if At_End (It.all) then
            return S;
         else
            Next (It.all);
            return S & ' ' & Img (It, Index + 1);
         end if;
      end Img;

      It   : aliased Iterator := Get_Start (L);

   begin
      return Img (It'Access, 1);
   end Image;

   procedure Apply (L : List; A : Application) is
      P : List_Node_Ptr := L.Lst.First;
   begin
      for I in 1 .. L.Lst.Count loop
         A (P.E.all);
         P := P.Next;
      end loop;
   end Apply;

end WL.Lists.Generic_List;
