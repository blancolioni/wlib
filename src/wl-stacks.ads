with WL.Arrays.Dynamic;

generic
   type Stack_Element is private;
package WL.Stacks is

   Stack_Error : exception;


   type Stack is limited private;

   procedure Push (To : in out Stack; Item : in Stack_Element);

   procedure Pop (From : in out Stack; Item : out Stack_Element);

   function Top (Of_Stack : Stack) return Stack_Element;

   function Is_Empty (S : Stack) return Boolean;
   function Is_Full (S : Stack) return Boolean;

   procedure Clear (S : in out Stack);

   function Size (S : in Stack) return Natural;

private

   package Array_Of_Stack_Elements is
      new WL.Arrays.Dynamic (Positive, Stack_Element);

   type Stack is
      record
         Elements : Array_Of_Stack_Elements.Dynamic_Array;
         Top      : Natural := 0;
      end record;

end WL.Stacks;
