with Ada.Unchecked_Deallocation;

package body WL.Tables.Hash_Table is

   procedure Free is
      new Ada.Unchecked_Deallocation (List_Element, List_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation
     (Table_Record, Table_Record_Access);

   Free_Routine : Free_Element_Routine := null;

   function Is_Prime (N : Positive) return Boolean is
   begin
      if N < 4 then
         return True;
      elsif N mod 2 + N mod 3 + N mod 5 + N mod 7 = 0 then
         return False;
      else
         for I in 11 .. N / 2 loop
            if N mod I = 0 then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Prime;
   pragma Unreferenced (Is_Prime);

   function Partial_Hash
     (Size    : Natural;
      Next    : Character;
      Current : Natural := 0)
   return Natural
   is
   begin
      return (64 * Current + Character'Pos (Next)) mod Size;
   end Partial_Hash;

   function Partial_Hash
     (T       : Table;
      Next    : Character;
      Current : Natural := 0)
   return Natural
   is
      pragma Unreferenced (T);
   begin
      return Partial_Hash (Default_Size, Next, Current);
   end Partial_Hash;

   function Hash (S : String; T : Table) return Natural is
      pragma Unreferenced (T);
      Result : Natural := 0;
   begin
      for I in S'Range loop
         Result := Partial_Hash (Default_Size, S (I), Result);
      end loop;
      return Result;
   end Hash;

   function Hash (S : String; Size : Natural) return Natural is
      Result : Natural := 0;
   begin
      for I in S'Range loop
         Result := Partial_Hash (Size, S (I), Result);
      end loop;
      return Result;
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out Table) is
   begin
      T.Table_Data := new Table_Record (Default_Size - 1);
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (T : in out Table) is
   begin
      T.Table_Data := new Table_Record'(T.Table_Data.all);
   end Adjust;

   --------------
   -- Finalize --
   --------------
   procedure Finalize (T : in out Table) is
   begin
      Clear (T);
      Free (T.Table_Data);
   end Finalize;

   ------------
   -- Insert --
   ------------
   procedure Insert (T : in out Table; E : in Element_Type) is
      Code : constant Natural := Hash (Get_Key (E), T);
   begin
      T.Table_Data.Contents (Code) :=
        new List_Element'(E, T.Table_Data.Contents (Code));

   end Insert;

   ----------------
   -- Clear_List --
   ----------------
   procedure Clear_List (List : in out List_Ptr) is
   begin
      if List /= null then
         Clear_List (List.Next);
         if Free_Routine /= null then
            Free_Routine (List.Element);
         end if;
         Free (List);
      end if;
   end Clear_List;

   -----------
   -- Clear --
   -----------
   procedure Clear (T : in out Table) is
   begin
      if T.Table_Data /= null then
         for I in T.Table_Data.Contents'Range loop
            Clear_List (T.Table_Data.Contents (I));
         end loop;
      end if;
      Free (T.Table_Data);
      Initialize (T);
   end Clear;

   ------------
   -- Delete --
   ------------
   procedure Delete (T : in out Table; E : in Element_Type) is
      Code       : constant Natural := Hash (Get_Key (E), T);
      This, That : List_Ptr;
   begin
      if T.Table_Data.Contents (Code) = null then
         raise No_Delete_Target;
      end if;

      This := T.Table_Data.Contents (Code);
      if T.Table_Data.Contents (Code).Element = E then
         T.Table_Data.Contents (Code) :=
           T.Table_Data.Contents (Code).Next;
         Free (This);
      else
         while This.Next /= null and then This.Next.Element /= E loop
            This := This.Next;
         end loop;
         if This.Next /= null then
            That := This.Next;
            This.Next := This.Next.Next;
            Free (That);
         else
            raise No_Delete_Target;
         end if;
      end if;

   end Delete;


   -----------
   -- First --
   -----------
   function First (T : in Table; K : in String) return Position is

   begin
      return First (T, Hash (K, T), K);
   end First;

   -----------
   -- First --
   -----------
   function First (T : in Table;
                   Hash : in Natural;
                   K : in String)
                  return Position
   is
      Result : Position;
   begin
      Result.For_Table := T.Table_Data;
      Result.Current := T.Table_Data.Contents (Hash);
      if Result.Current = null then
         return Result;
      end if;

      while Result.Current /= null and then Get_Key
        (Result.Current.Element) /= K loop
         Result.Current := Result.Current.Next;
      end loop;

      return Result;

   end First;


   ----------
   -- Next --
   ----------
   function Next (P : in Position; K : in String) return Position is
      Result : Position := P;
   begin
      if Not_Found (Result) then
         return Result;
      else
         Result.Current := Result.Current.Next;
         while Result.Current /= null and then Get_Key
           (Result.Current.Element) /= K loop
            Result.Current := Result.Current.Next;
         end loop;
         return Result;
      end if;
   end Next;

   ------------
   -- Exists --
   ------------
   function Exists (T : in Table; K : in String) return Boolean is
   begin
      return Found (First (T, K));
   end Exists;

   ------------
   -- Exists --
   ------------
   function Exists (T : in Table;
                    Hash : in Natural;
                    K : in String)
                   return Boolean
   is
      Elmt : List_Ptr := T.Table_Data.Contents (Hash);
   begin
      while Elmt /= null and then Get_Key (Elmt.Element) /= K loop
         Elmt := Elmt.Next;
      end loop;
      return Elmt /= null;
   end Exists;


   --------------
   -- Set_Size --
   --------------
   procedure Set_Size (T : in out Table; Size : in Positive) is
   begin
      null;

   --       Actual_Size : Positive := Size;
   --    begin -- Set_Size
   --       while not Is_Prime (Actual_Size) loop
   --          Actual_Size := Actual_Size + 1;
   --       end loop;

   --       declare
   --          New_Table  : Table (Actual_Size);
   --          Pos        : Position;
   --          Saved_Free : Free_Element_Routine;
   --       begin
   --          Pos := First (T);
   --          while Found (Pos) loop
   --             Insert (New_Table, Contents (Pos));
   --             Pos := Next (Pos);
   --          end loop;
   --          Saved_Free := Free_Routine;
   --          Free_Routine := null;
   --          Clear (T);
   --          Free_Routine := Saved_Free;

   --          T := New_Table;
   --       end;
   end Set_Size;

   function Contents (P : Position) return Element_Type is
   begin
      return P.Current.Element;
   end Contents;

   function First (T : in Table) return Position is
   begin
      for I in T.Table_Data.Contents'Range loop
         if T.Table_Data.Contents (I) /= null then
            return (T.Table_Data, T.Table_Data.Contents (I));
         end if;
      end loop;
      return (T.Table_Data, null);
   end First;

   function Next (Pos : Position) return Position is
      Code : Natural;
   begin
      if Pos.Current = null then
         return Pos;
      elsif Pos.Current.Next /= null then
         return (Pos.For_Table, Pos.Current.Next);
      else
         Code :=
           Hash
           (Get_Key (Pos.Current.Element), Pos.For_Table.Size + 1);
         for I in Code + 1 .. Pos.For_Table.Contents'Last loop
            if Pos.For_Table.Contents (I) /= null then
               return (Pos.For_Table, Pos.For_Table.Contents (I));
            end if;
         end loop;
      end if;
      return (Pos.For_Table, null);
   end Next;

   procedure Set_Free_Routine (Free : Free_Element_Routine) is
   begin
      Free_Routine := Free;
   end Set_Free_Routine;

   function Not_Found (Pos : Position) return Boolean is
   begin
      return not Found (Pos);
   end Not_Found;

   function Found (Pos : Position) return Boolean is
   begin
      return Pos.Current /= null;
   end Found;

end WL.Tables.Hash_Table;
