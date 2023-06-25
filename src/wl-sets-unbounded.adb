with Unchecked_Deallocation;

package body WL.Sets.Unbounded is

   Initial_Size : constant := 4;
   Growth       : constant := 8;

   procedure Free is
      new Unchecked_Deallocation (Element_List, Set_List);

   function New_Set return Set is
      use Ada.Finalization;
   begin
      return (Controlled with new Element_List (1 .. Initial_Size), 0);
   end New_Set;

   function Copy (S : Set) return Set is
      use Ada.Finalization;
      New_S : Set :=
        (Controlled with new Element_List (1 .. S.Count + Growth),
                         S.Count);
   begin
      New_S.List (1 .. New_S.Count) := S.List (1 .. S.Count);
      return New_S;
   end Copy;

   procedure Initialize (Object : in out Set) is
   begin
      Object := New_Set;
   end Initialize;

   procedure Adjust (Object : in out Set) is
      New_List : constant Set_List :=
        new Element_List (1 .. Object.Count + Growth);
   begin
      New_List.all (1 .. Object.Count) :=
        Object.List (1 .. Object.Count);
      Object.List := New_List;
   end Adjust;

   procedure Finalize (Object : in out Set) is
   begin
      Free (Object.List);
   end Finalize;

   Empty_Set : constant Set := New_Set;
   function Empty return Set is
   begin
      return Empty_Set;
   end Empty;

   function Is_Empty (S : Set) return Boolean is
   begin
      return S.Count = 0;
   end Is_Empty;

   function Elements (S : Set) return Natural is
   begin
      return S.Count;
   end Elements;

   function To_List (S : Set) return Element_List is
   begin
      return S.List (1 .. S.Count);
   end To_List;

   function Contents (S : Set) return Element is
   begin
      if Elements (S) /= 1 then
         raise Set_Error;
      end if;

      return S.List (1);

   end Contents;

   function "+" (E : Element) return Set is
      S : Set := New_Set;
   begin
      Add (S, E);
      return S;
   end "+";

   function "+" (E : Element_List) return Set is
      use Ada.Finalization;
   begin
      return (Controlled with new Element_List'(E), E'Length);
   end "+";

   function "+" (E1, E2 : Element) return Set is
      S : Set := New_Set;
   begin
      Add (S, E1);
      Add (S, E2);
      return S;
   end "+";

   function "+" (E : Element; S : Set) return Set is
      Result : Set := Copy (S);
   begin
      Add (Result, E);
      return Result;
   end "+";

   function "+" (S : Set; E : Element) return Set is
      Result : Set := Copy (S);
   begin
      Add (Result, E);
      return Result;
   end "+";

   function "+" (S1, S2 : Set) return Set is
      Result : Set := Copy (S1);
   begin
      for I in 1 .. S2.Count loop
         Add (Result, S2.List (I));
      end loop;
      return Result;
   end "+";

   function "-" (S1 : Set; S2 : Set) return Set is
      Result : Set := Copy (S1);
   begin
      for I in 1 .. S2.Count loop
         Remove (Result, S2.List (I));
      end loop;
      return Result;
   end "-";

   function "-" (S : Set; E : Element) return Set is
      Result : Set := Copy (S);
   begin
      Remove (Result, E);
      return Result;
   end "-";

   function "<=" (E : Element; S : Set) return Boolean is
   begin
      for I in 1 .. S.Count loop
         if E = S.List (I) then
            return True;
         end if;
      end loop;
      return False;
   end "<=";

   procedure Add (S : in out Set; E : Element) is
   begin
      if E <= S then
         return;
      end if;
      if S.Count = S.List.all'Length then
         declare
            New_List : Set_List :=
              new Element_List (1 .. S.Count + Growth);
         begin
            for I in 1 .. S.Count loop
               New_List (I) := S.List (I);
            end loop;
            Free (S.List);
            S.List := New_List;
         end;
      end if;
      S.Count := S.Count + 1;
      S.List (S.Count) := E;
   end Add;

   procedure Add (S1 : in out Set; S2 : Set) is
   begin
      for I in 1 .. S2.Count loop
         Add (S1, S2.List (I));
      end loop;
   end Add;

   procedure Take (S : in out Set; E : out Element) is
   begin
      E := S.List (S.Count);
      S.Count := S.Count - 1;
   end Take;

   procedure Remove (S : in out Set; E : Element) is
      Found : Boolean := False;
   begin
      for I in 1 .. S.Count loop
         if Found then
            S.List (I - 1) := S.List (I);
         elsif S.List (I) = E then
            Found := True;
         end if;
      end loop;
      if Found then
         S.Count := S.Count - 1;
      end if;
   end Remove;

   procedure Remove (S1 : in out Set; S2 : Set) is
   begin
      for I in 1 .. S2.Count loop
         Remove (S1, S2.List (I));
      end loop;
   end Remove;

   --    function Image (S : Set) return String is

   --       function Img (S : Set; Current : Positive) return String is
   --       begin
   --          if Current > S.Count then
   --             return "";
   --          elsif Current = S.Count then
   --             return Image(S.List(Current));
   --          else
   --             return Image(S.List(Current)) & ", " & Img(S, Current + 1);
   --          end if;
   --       end Img;

   --    begin
   --       return "{" & Img(S, 1) & "}";
   --    end Image;

   function Extract
     (S         : Set;
      Left      : Element;
      Condition : Compare_Function)
   return Set
   is
      S0 : Set := S;
   begin
      Extract (S0, Left, Condition);
      return S0;
   end Extract;

   procedure Extract
     (S         : in out Set;
      Left      : in Element;
      Condition : in Compare_Function)
   is
      Current : Positive := S.List.all'First;
   begin
      for I in 1 .. S.Count loop
         if Condition (S.List (I), Left) then
            if I /= Current then
               S.List (Current) := S.List (I);
            end if;
            Current := Current + 1;
         end if;
      end loop;
      S.Count := Current - 1;
   end Extract;


end WL.Sets.Unbounded;
