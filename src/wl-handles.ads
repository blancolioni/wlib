private with Ada.Finalization;

generic
   type Item_Type (<>) is private;
package WL.Handles is

   type Handle_Type is tagged private;

   Null_Handle : constant Handle_Type;

   type Item_Access is access all Item_Type;

   function Create
     (Item : Item_Type)
     return Handle_Type;

   function Get
     (Handle : Handle_Type) return Item_Type;

   function Set
     (Handle : Handle_Type) return Item_Access;

private

   type Node_Type;
   type Node_Access is access Node_Type;

   type Handle_Type is
     new Ada.Finalization.Controlled with record
        Node : Node_Access;
     end record;

   overriding procedure Adjust (Handle : in out Handle_Type);

   overriding procedure Finalize (Handle : in out Handle_Type);

   Null_Handle : constant Handle_Type :=
     (Ada.Finalization.Controlled with Node => null);

end WL.Handles;
