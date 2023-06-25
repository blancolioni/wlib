with Ada.Unchecked_Deallocation;

package body WL.Trees.Generic_Trees is

   type Tree_Neighbour is array (Direction) of Tree;

   type Tree_Record is
      record
         N           : Node;
         Parent      : Tree;
         Sibling     : Tree_Neighbour;
         Children    : Tree_Neighbour;
         Child_Count : Natural := 0;
      end record;

   procedure Set_Sibling
     (For_Tree : in Tree;
      Side     : in Direction;
      To       : in Tree);

   procedure Set_Child
     (For_Tree  : in Tree;
      Side      : in Direction;
      New_Child : in Tree);

   procedure Set_Parent (For_Tree : in Tree; Parent : in Tree);

   ---------
   -- "-" --
   ---------

   function "-" (Item : Direction) return Direction is
   begin
      case Item is
         when Left =>
            return Right;
         when Right =>
            return Left;
      end case;
   end "-";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Tree) return Boolean is
   begin
      return Left.T.N = Right.T.N;
   end "=";

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (To_Tree : in Tree;
      Child   : in Tree;
      Edge    : in Direction := Right)
   is
   begin
      if Is_Leaf (To_Tree) then
         To_Tree.T.Children := (others => Child);
         Child.T.Sibling := (others => Empty_Tree);
      else
         declare
            Old_Edge : constant Tree := Get_Edge_Child (To_Tree, Edge);
         begin
            Set_Sibling (Old_Edge, Edge, Child);
            Set_Sibling (Child, -Edge, Old_Edge);
            Set_Child (To_Tree, Edge, Child);
         end;
      end if;
      Set_Parent (Child, To_Tree);
   end Add_Child;

   -----------------
   -- Add_Sibling --
   -----------------

   procedure Add_Sibling
     (To_Tree     : in Tree;
      New_Sibling : in Tree;
      Side        : in Direction := Right)
   is
   begin
      Set_Sibling (New_Sibling, Side, Get_Sibling (To_Tree, Side));

      if Is_Empty (Get_Sibling (To_Tree, Side)) then
         Set_Sibling (To_Tree, Side, New_Sibling);
         Set_Child (Get_Parent (To_Tree), Side, New_Sibling);
      else
         Set_Sibling
           (Get_Sibling (To_Tree, Side), -Side, New_Sibling);
         Set_Sibling (To_Tree, Side, New_Sibling);
      end if;

      Set_Sibling (New_Sibling, -Side, To_Tree);

      Set_Parent (New_Sibling, Get_Parent (To_Tree));

   end Add_Sibling;

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree return Tree is
      Result : Tree;
   begin
      return Result;
   end Empty_Tree;

   ------------------------
   -- Finalize_Reference --
   ------------------------

   procedure Finalize_Reference (T : in out Tree) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Tree_Record, Tree_Access);
   begin
      if T.T /= null then
         Finalize_Node (T.T.N);
         Free (T.T);
      end if;
   end Finalize_Reference;

   ---------------
   -- Free_Tree --
   ---------------

   procedure Free_Tree (Item : in out Tree) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Tree_Record, Tree_Access);
      Child   : Tree := Get_Edge_Child (Item, Left);
      Current : Tree;
   begin
      while not Is_Empty (Child) loop
         Current := Child;
         Child := Get_Sibling (Child, Right);
         Free_Tree (Current);
      end loop;
      Finalize_Node (Item.T.N);
      Free (Item.T);
   end Free_Tree;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Item : Tree; Index : Positive) return Tree is
   begin
      if Index > Get_Child_Count (Item) then
         raise Tree_Error;
      end if;
      declare
         Result : Tree := Get_Edge_Child (Item, Left);
      begin
         for I in 1 .. Index - 1 loop
            Result := Get_Sibling (Result, Right);
         end loop;
         return Result;
      end;
   end Get_Child;

   ---------------------
   -- Get_Child_Count --
   ---------------------

   function Get_Child_Count (Item : Tree) return Natural is
   begin
      return Item.T.Child_Count;
   end Get_Child_Count;

   --------------------
   -- Get_Edge_Child --
   --------------------

   function Get_Edge_Child (Item : Tree; Edge : Direction) return Tree is
   begin
      return Item.T.Children (Edge);
   end Get_Edge_Child;

   --------------
   -- Get_Node --
   --------------

   function Get_Node (Item : Tree) return Node is
   begin
      return Item.T.N;
   end Get_Node;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Item : Tree) return Tree is
   begin
      return Item.T.Parent;
   end Get_Parent;

   -----------------
   -- Get_Sibling --
   -----------------

   function Get_Sibling (Item : Tree; Sibling : Direction) return Tree is
   begin
      return Item.T.Sibling (Sibling);
   end Get_Sibling;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Item : Tree) return Boolean is
   begin
      return Item.T = null;
   end Is_Empty;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (Item : Tree) return Boolean is
   begin
      return Item.T.Child_Count = 0;
   end Is_Leaf;

   --------------
   -- New_Tree --
   --------------

   function New_Tree (With_Node : Node) return Tree is
      Result : Tree;
   begin
      Result.T := new Tree_Record;
      Result.T.N := With_Node;
      return Result;
   end New_Tree;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (For_Tree  : in Tree;
      Side      : in Direction;
      New_Child : in Tree)
   is
   begin
      For_Tree.T.Children (Side) := New_Child;
   end Set_Child;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (For_Tree : in Tree; Parent : in Tree) is
   begin
      if For_Tree.T.Parent.T /= null then
         For_Tree.T.Parent.T.Child_Count :=
           For_Tree.T.Parent.T.Child_Count - 1;
      end if;
      For_Tree.T.Parent := Parent;
      Parent.T.Child_Count := Parent.T.Child_Count + 1;
   end Set_Parent;

   -----------------
   -- Set_Sibling --
   -----------------

   procedure Set_Sibling
     (For_Tree : in Tree;
      Side     : in Direction;
      To       : in Tree)
   is
   begin
      For_Tree.T.Sibling (Side) := To;
   end Set_Sibling;


end WL.Trees.Generic_Trees;
