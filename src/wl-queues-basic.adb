with Ada.Unchecked_Deallocation;

package body WL.Queues.Basic is

   type Queue_Element_Record is
      record
         Contents : Element;
         Next     : Queue_Element;
         Priority : Natural;
      end record;

   procedure Free is
      new Ada.Unchecked_Deallocation
     (Queue_Element_Record, Queue_Element);

   function Empty (Q : Queue) return Boolean is
   begin
      return Q.First = null;
   end Empty;

   procedure Add (Q : in out Queue; Elmt : in Element) is
      Store : constant Queue_Element :=
        new Queue_Element_Record'(Elmt, null, 0);
   begin
      if Q.First = null then
         Q.First := Store;
      else
         Q.Last.Next := Store;
      end if;
      Q.Last := Store;
   end Add;

   procedure Add
     (Q        : in out Queue;
      Elmt     : in Element;
      Priority : in Natural)
   is
      E, Store : Queue_Element;
   begin
      if Priority = 0 or else Q.First = null or else
        Priority <= Q.Last.Priority
      then
         Add (Q, Elmt);
      elsif Priority > Q.First.Priority then
         Q.First := new Queue_Element_Record'(Elmt, Q.First, Priority);
      else
         E := Q.First;
         while E.Next.Priority > Priority loop
            E := E.Next;
         end loop;
         Store := new Queue_Element_Record'(Elmt, E.Next, Priority);
         E.Next := Store;
      end if;
   end Add;

   procedure Remove (Q : in out Queue; Result : out Element) is
      First : Queue_Element := Q.First;
   begin
      if Empty (Q) then
         raise Empty_Queue;
      end if;

      Result := Q.First.Contents;
      if Q.First = Q.Last then
         Q.First := null;
         Q.Last := null;
      else
         Q.First := Q.First.Next;
      end if;
      Free (First);
   end Remove;

   function Get_Queue_Elements (Q : Queue_Element) return Array_Of_Elements is
   begin
      if Q = null then
         declare
            Result : Array_Of_Elements (1 .. 0);
         begin
            return Result;
         end;
      else
         return Q.Contents & Get_Queue_Elements (Q.Next);
      end if;
   end Get_Queue_Elements;

   function Get_Elements (Q : Queue) return Array_Of_Elements is
   begin
      return Get_Queue_Elements (Q.First);
   end Get_Elements;

   procedure Initialize (Q : in out Queue) is
      pragma Unreferenced (Q);
   begin
      null;
   end Initialize;

   procedure Finalize (Q : in out Queue) is
      E1, E2 : Queue_Element;
   begin
      E1 := Q.First;
      while E1 /= null loop
         E2 := E1.Next;
         Free (E1);
         E1 := E2;
      end loop;
   end Finalize;

   procedure Adjust (Q : in out Queue) is
      E : Queue_Element := Q.First;
   begin
      Q.First := null;
      Q.Last := null;
      while E /= null loop
         Add (Q, E.Contents, E.Priority);
         E := E.Next;
      end loop;
   end Adjust;

end WL.Queues.Basic;
