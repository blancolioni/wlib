package body WL.Tables.Existence_Table is

   ------------
   -- Exists --
   ------------

   function Exists (In_Table : Table; Name : String) return Boolean is
      Pos : constant Table_Of_Strings.Position :=
        Table_Of_Strings.First (In_Table.The_Table, Name);
   begin
      return Table_Of_Strings.Found (Pos);
   end Exists;

   ------------
   -- Insert --
   ------------

   procedure Insert (To_Table : in out Table; Name : String) is
   begin
      if not Exists (To_Table, Name) then
         Table_Of_Strings.Insert
           (To_Table.The_Table,
            Ada.Strings.Unbounded.To_Unbounded_String (Name));
      end if;
   end Insert;

end WL.Tables.Existence_Table;
