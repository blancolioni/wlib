generic
   type Node is private;
   with procedure Finalize_Node (N : in out Node);
package WL.Trees.Generic_Trees is

   pragma Elaborate_Body;

   Tree_Error : exception;

   type Tree is private;

   type Direction is (Left, Right);
   function "-" (Item : Direction) return Direction;

   function Empty_Tree return Tree;

   function Is_Empty (Item : Tree) return Boolean;
   function Is_Leaf (Item : Tree) return Boolean;

   function Get_Node (Item : Tree) return Node;

   function Get_Child (Item : Tree; Index : Positive) return Tree;

   function Get_Child_Count (Item : Tree) return Natural;

   function Get_Edge_Child (Item : Tree; Edge : Direction) return Tree;

   function Get_Sibling (Item : Tree; Sibling : Direction) return Tree;

   function Get_Parent (Item : Tree) return Tree;

   procedure Add_Child
     (To_Tree : in Tree;
      Child   : in Tree;
      Edge    : in Direction := Right);

   procedure Add_Sibling
     (To_Tree     : in Tree;
      New_Sibling : in Tree;
      Side        : in Direction := Right);

   function New_Tree (With_Node : Node) return Tree;

   function "=" (Left, Right : Tree) return Boolean;

   procedure Free_Tree (Item : in out Tree);

private
   type Tree_Record;
   type Tree_Access is access Tree_Record;

   type Tree is   --  new WL.Reference_Counter.Reference_Controlled with
      record
         T : Tree_Access;
      end record;

   procedure Finalize_Reference (T : in out Tree);

end WL.Trees.Generic_Trees;
