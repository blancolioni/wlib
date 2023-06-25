with Ada.Strings.Unbounded;
with WL.Tables.Hash_Table;
--  what a giveaway!
pragma Elaborate_All (WL.Tables.Hash_Table);

package WL.Sets.Strings is

   pragma Elaborate_Body;

   type Set is limited private;
   --  sorry about the limited bit

   function Is_Empty (S : Set) return Boolean;

   function "<=" (E : String; S : Set) return Boolean;

   procedure Add (S : in out Set; E : String);
   procedure Remove (S : in out Set; E : String);

private
   type Element_Type is
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Get_Text (From : Element_Type) return String;

   package Table_Of_Strings is
      new WL.Tables.Hash_Table (Element_Type, Get_Text);

   type Set is
      record
         String_Table : Table_Of_Strings.Table;
      end record;

end WL.Sets.Strings;
