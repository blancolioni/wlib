with Ada.Strings.Unbounded;
with Ada.Text_IO;

with WL.Tables.Hash_Table;
with WL.Trace;

package body WL.Allocation is

   Trace_Allocations : constant Boolean := False;

   Allocated : Natural := 0;

   type Allocation_Entry_Record is
      record
         Name     : Ada.Strings.Unbounded.Unbounded_String;
         Quantity : Natural := 0;
         Size     : Natural := 0;
      end record;

   type Allocation_Entry is access Allocation_Entry_Record;

   function Get_Name (A : Allocation_Entry) return String;

   package Table_Of_Allocations is
      new WL.Tables.Hash_Table (Allocation_Entry, Get_Name);

   Allocation_Table : Table_Of_Allocations.Table;

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Size    : Natural;
                       Message : String)
   is
      Bytes : constant Natural := (Size - 1) / 8 + 1;
   begin
      if Trace_Allocations then
         WL.Trace.Put_Line ("[" & Message & "] Allocating" &
                            Natural'Image (Bytes) & " bytes");
      end if;
      Allocated := Allocated + Bytes;
      declare
         use Table_Of_Allocations;
         Pos : constant Position := First (Allocation_Table, Message);
      begin
         if Found (Pos) then
            Contents (Pos).Quantity := Contents (Pos).Quantity + 1;
            Contents (Pos).Size     := Contents (Pos).Size + Bytes;
         else
            Insert (Allocation_Table,
                    new Allocation_Entry_Record'
                    (Ada.Strings.Unbounded.To_Unbounded_String (Message),
                     1,
                     Bytes));
         end if;
      end;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Size    : Natural;
                         Message : String)
   is
      Bytes : constant Natural := (Size - 1) / 8 + 1;
   begin
      if Trace_Allocations then
         WL.Trace.Put_Line ("[" & Message & "] Deallocating" &
                            Natural'Image (Bytes) & " bytes");
      end if;
      Allocated := Allocated - Bytes;
      declare
         use Table_Of_Allocations;
         Pos : constant Position := First (Allocation_Table, Message);
      begin
         Contents (Pos).Quantity := Contents (Pos).Quantity - 1;
         Contents (Pos).Size := Contents (Pos).Size - Bytes;
      end;
   end Deallocate;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (A : Allocation_Entry) return String is
   begin
      return Ada.Strings.Unbounded.To_String (A.Name);
   end Get_Name;

   ------------
   -- Report --
   ------------

   procedure Report is
      use Table_Of_Allocations;
      Pos : Position := First (Allocation_Table);
   begin
      while Found (Pos) loop
         Ada.Text_IO.Put_Line (Get_Name (Contents (Pos)) &
                               ":" &
                               Natural'Image (Contents (Pos).Quantity) &
                               Natural'Image (Contents (Pos).Size));
         Pos := Next (Pos);
      end loop;
   end Report;

end WL.Allocation;
