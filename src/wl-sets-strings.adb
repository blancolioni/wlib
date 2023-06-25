package body WL.Sets.Strings is

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Set) return Boolean is
   begin
      return Table_Of_Strings.Not_Found
        (Table_Of_Strings.First (S.String_Table));
   end Is_Empty;

   ----------
   -- "<=" --
   ----------

   function "<=" (E : String; S : Set) return Boolean is
   begin
      return Table_Of_Strings.Found
        (Table_Of_Strings.First (S.String_Table, E));
   end "<=";

   ---------
   -- Add --
   ---------

   procedure Add (S : in out Set; E : String) is
   begin
      Table_Of_Strings.Insert
        (S.String_Table,
         (Text => Ada.Strings.Unbounded.To_Unbounded_String (E)));
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (S : in out Set; E : String) is
   begin
      Table_Of_Strings.Delete
        (S.String_Table,
         (Text => Ada.Strings.Unbounded.To_Unbounded_String (E)));
   end Remove;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (From : Element_Type) return String is
   begin
      return Ada.Strings.Unbounded.To_String (From.Text);
   end Get_Text;

end WL.Sets.Strings;
