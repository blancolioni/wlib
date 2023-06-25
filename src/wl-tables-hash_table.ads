with Ada.Finalization;

generic
   type Element_Type is private;
   with function Get_Key (E : Element_Type) return String;
package WL.Tables.Hash_Table is

   pragma Elaborate_Body;

   Table_Error  : exception;

   Default_Size : constant := 709;

   type Table is private;
   type Position is private;

   function Found (Pos : Position) return Boolean;
   function Not_Found (Pos : Position) return Boolean;

   procedure Insert (T : in out Table; E : in Element_Type);
   procedure Delete (T : in out Table; E : in Element_Type);
   function First (T : in Table; K : in String) return Position;
   function Next (P : in Position; K : in String) return Position;

   function Partial_Hash
     (T       : Table;
      Next    : Character;
      Current : Natural := 0)
   return Natural;

   function First (T : in Table; Hash : in Natural; K : in String)
                  return Position;

   function Exists (T : in Table; K : in String) return Boolean;

   function Exists (T : in Table; Hash : in Natural; K : in String)
                   return Boolean;

   function Contents (P : Position) return Element_Type;

   procedure Set_Size (T : in out Table; Size : in Positive);

   procedure Clear (T : in out Table);

   function First (T : in Table) return Position;
   function Next (Pos : Position) return Position;

   type Free_Element_Routine is
     access procedure
     (Element : in out Element_Type);

   procedure Set_Free_Routine (Free : Free_Element_Routine);

private
--  Linked list implementation
   type List_Element;
   type List_Ptr is access List_Element;
   type List_Element is
      record
         Element : Element_Type;
         Next    : List_Ptr;
      end record;

   type Chained_Table is array (Natural range <>) of List_Ptr;
   type Chained_Table_Access is access Chained_Table;

   type Table_Record (Size : Natural) is
      record
         Contents : Chained_Table (0 .. Size);
      end record;

   type Table_Record_Access is access Table_Record;

   type Table is
     new Ada.Finalization.Controlled with
      record
         Table_Data : Table_Record_Access;
      end record;

   procedure Initialize (T : in out Table);
   procedure Finalize (T : in out Table);
   procedure Adjust (T : in out Table);

   type Position is
      record
         For_Table : Table_Record_Access;
         Current   : List_Ptr;
      end record;

end WL.Tables.Hash_Table;
