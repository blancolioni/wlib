package body WL.Handles is

   type Node_Type is
      limited record
         Item  : aliased Item_Type;
         Count : Natural;
         Next  : Node_Access;
      end record;

   Free_List : Node_Access;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Handle : in out Handle_Type) is
   begin
      Handle.Node.Count := Handle.Node.Count + 1;
   end Adjust;

   ------------
   -- Create --
   ------------

   function Create return Handle_Type is

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

      return (Ada.Finalization.Controlled with Node);

   end Create;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Handle : in out Handle_Type) is
   begin

      Handle.Node.Count := Handle.Node.Count - 1;

      if Handle.Node.Count = 0 then

         Handle.Node.Next := Free_List;
         Free_List := Handle.Node;

      end if;

   end Finalize;

   ---------
   -- Get --
   ---------

   function Get
     (Handle : Handle_Type) return Item_Type is
   begin
      return Handle.Node.Item;
   end Get;

   ---------
   -- Set --
   ---------

   function Set
     (Handle : Handle_Type) return Item_Access is
   begin
      return Handle.Node.Item'Access;
   end Set;

end WL.Handles;
