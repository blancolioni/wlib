package body WL.Sets.Bounded is

   function Empty return Set is
   begin
      return (others => False);
   end Empty;

   function Is_Empty (S : Set) return Boolean is
   begin
      return S = Empty;
   end Is_Empty;

   function "+" (E : Element) return Set is
      S : Set := (others => False);
   begin
      S (E) := True;
      return S;
   end "+";

   function "+" (E : Element_List) return Set is
      S : Set := (others => False);
   begin
      for I in E'Range loop
         S (E (I)) := True;
      end loop;
      return S;
   end "+";


   function "+" (E1, E2 : Element) return Set is
      S : Set := (others => False);
   begin
      S (E1) := True;
      S (E2) := True;
      return S;
   end "+";

   function "+" (E : Element; S : Set) return Set is
      S1 : Set := S;
   begin
      S1 (E) := True;
      return S1;
   end "+";

   function "+" (S : Set; E : Element) return Set is
      S1 : Set := S;
   begin
      S1 (E) := True;
      return S1;
   end "+";


   function "+" (S1, S2 : Set) return Set is
   begin
      return S1 or S2;
   end "+";

   function "-" (S1, S2 : Set) return Set is
   begin
      return S1 and S2;
   end "-";

   function "-" (S : Set; E : Element) return Set is
      S1 : Set := S;
   begin
      S1 (E) := False;
      return S1;
   end "-";

   function "<=" (E : Element; S : Set) return Boolean is
   begin
      return S (E);
   end "<=";

   procedure Take (S : in out Set; E : out Element) is
   begin
      for I in S'Range loop
         if S (I) then
            S (I) := False;
            E := I;
            return;
         end if;
      end loop;
      raise Constraint_Error;
   end Take;

   procedure Add (S : in out Set; E : Element) is
   begin
      S (E) := True;
   end Add;

   procedure Add (S1 : in out Set; S2 : Set) is
   begin
      S1 := S1 or S2;
   end Add;

   procedure Remove (S : in out Set; E : Element) is
   begin
      S (E) := False;
   end Remove;

   procedure Remove (S1 : in out Set; S2 : Set) is
   begin
      S1 := S1 and S2;
   end Remove;

   function Image (S : Set) return String is
      function Img (S : Set; Pos : Element) return String is
      begin
         if Pos = Element'Last then

            if S (Pos) then
               return Element'Image (Pos);
            else
               return "";
            end if;

         else

            if S (Pos) then
               return Element'Image (Pos) &
                 Img (S, Element'Succ (Pos));
            else
               return Img (S, Element'Succ (Pos));
            end if;

         end if;
      end Img;
   begin
      return "{" & Img (S, Element'First) & "}";
   end Image;

end WL.Sets.Bounded;
