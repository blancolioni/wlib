with Ada.Finalization;

generic
   type Element is private;
package WL.Queues.Basic is

   pragma Elaborate_Body;

   type Queue is private;

   function Empty (Q : Queue) return Boolean;

   procedure Add (Q : in out Queue; Elmt : in Element);

   procedure Add
     (Q        : in out Queue;
      Elmt     : in Element;
      Priority : in Natural);

   procedure Remove (Q : in out Queue; Result : out Element);

   type Array_Of_Elements is array (Positive range <>) of Element;
   function Get_Elements (Q : Queue) return Array_Of_Elements;

private
   type Queue_Element_Record;
   type Queue_Element is access Queue_Element_Record;

   type Queue is new Ada.Finalization.Controlled with
      record

         First, Last : Queue_Element;
      end record;

   procedure Initialize (Q : in out Queue);
   procedure Finalize (Q : in out Queue);
   procedure Adjust (Q : in out Queue);

end WL.Queues.Basic;
