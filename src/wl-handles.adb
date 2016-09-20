with Ada.Unchecked_Deallocation;

package body WL.Handles is

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Item_Type, Item_Access);

   type Node_Type is
      limited record
         Item  : Item_Access;
         Count : Natural;
         Next  : Node_Access;
      end record;

   Free_List : Node_Access;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Handle : in out Handle_Type) is
   begin
      if Handle.Node /= null then
         Handle.Node.Count := Handle.Node.Count + 1;
      end if;
   end Adjust;

   ------------
   -- Create --
   ------------

   function Create
     (Item : Item_Type)
      return Handle_Type
   is

      Node : Node_Access;

   begin

      if Free_List = null then

         Node := Node_Access'(new Node_Type);

      else

         Node := Free_List;
         Free_List := Free_List.Next;

         Node.Next := null;

      end if;

      Node.Count := 1;
      Node.Item := new Item_Type'(Item);

      return (Ada.Finalization.Controlled with Node);

   end Create;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Handle : in out Handle_Type) is
   begin

      if Handle.Node /= null then
         Handle.Node.Count := Handle.Node.Count - 1;

         if Handle.Node.Count = 0 then

            Free (Handle.Node.Item);
            Handle.Node.Next := Free_List;
            Free_List := Handle.Node;

         end if;
      end if;
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get
     (Handle : Handle_Type) return Item_Type is
   begin
      return Handle.Node.Item.all;
   end Get;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Handle : Handle_Type)
      return Boolean
   is
   begin
      return Handle.Node = null;
   end Is_Null;

   ---------
   -- Set --
   ---------

   function Set
     (Handle : Handle_Type) return Item_Access is
   begin
      return Handle.Node.Item;
   end Set;

end WL.Handles;
