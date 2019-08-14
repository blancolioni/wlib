private with Ada.Containers.Vectors;

package WL.Bit_Vectors is

   type Bit is mod 2;

   type Bit_Array is array (Positive range <>) of Bit;

   type Count_Type is range 0 .. 2 ** 63 - 1;

   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;

   type Vector is tagged private;

   function Length
     (Container : Vector)
      return Count_Type;

   function Last_Index
     (Container : Vector)
      return Count_Type;

   function Element
     (Container : Vector;
      Index     : Index_Type)
      return Bit;

   function Slice
     (Container : Vector;
      From, To  : Index_Type)
      return Bit_Array;

   procedure Append
     (Container : in out Vector;
      Value     : Boolean);

   procedure Append
     (Container : in out Vector;
      Value     : Bit);

   procedure Append
     (Container : in out Vector;
      Values    : Bit_Array);

private

   type Element_Type is mod 2 ** 32;

   package Vectors is
     new Ada.Containers.Vectors (Positive, Element_Type);

   type Vector is tagged
      record
         Length   : Count_Type := 0;
         Elements : Vectors.Vector;
      end record;

   function Length
     (Container : Vector)
      return Count_Type
   is (Container.Length);

   function Last_Index
     (Container : Vector)
      return Count_Type
      renames Length;

end WL.Bit_Vectors;
