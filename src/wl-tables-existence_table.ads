with Ada.Strings.Unbounded;

with WL.Tables.Hash_Table;

package WL.Tables.Existence_Table is

   type Table is private;

   procedure Insert (To_Table : in out Table; Name : String);

   function Exists (In_Table : Table; Name : String) return Boolean;

private

   package Table_Of_Strings is
      new WL.Tables.Hash_Table
     (Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded.To_String);

   type Table is
      record
         The_Table : Table_Of_Strings.Table;
      end record;

end WL.Tables.Existence_Table;
