private with Ada.Containers.Vectors;

generic
   type Key_Type is private;
   type Element_Type is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package WL.Heaps is

   type Heap is tagged private;

   function Is_Empty
     (Container : Heap)
      return Boolean;

   procedure Insert
     (Container : in out Heap;
      Key       : Key_Type;
      Element   : Element_Type);

   procedure Replace
     (Container : in out Heap;
      Key       : Key_Type;
      Element   : Element_Type);

   function Maximum_Element
     (Container : Heap)
      return Element_Type;

   procedure Delete_Maximum
     (Container : in out Heap);

private

   type Heap_Element is
      record
         Key : Key_Type;
         Element : Element_Type;
      end record;

   package Heap_Vectors is
     new Ada.Containers.Vectors (Positive, Heap_Element);

   type Heap is tagged
      record
         Vector : Heap_Vectors.Vector;
      end record;

end WL.Heaps;
