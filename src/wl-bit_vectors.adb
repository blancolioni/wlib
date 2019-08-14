--------------------
-- WL.Bit_Vectors --
--------------------

package body WL.Bit_Vectors is

   procedure Split
     (Index      : Index_Type;
      Byte_Index : out Positive;
      Bit_Index  : out Natural);

   function Is_Set
     (Container : Vector;
      Index     : Index_Type)
      return Boolean;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector; Value : Boolean) is
      Byte_Index : Positive;
      Bit_Index  : Natural;
   begin
      Split (Container.Last_Index + 1, Byte_Index, Bit_Index);

      if Container.Elements.Last_Index < Byte_Index then
         Container.Elements.Append (0);
      end if;

      if Value then
         declare
            Element : Element_Type renames Container.Elements (Byte_Index);
         begin
            Element := Element or 2 ** Bit_Index;
         end;
      end if;

      Container.Length := Container.Length + 1;

   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector; Value : Bit) is
   begin
      Container.Append (Value /= 0);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector; Values : Bit_Array) is
   begin
      for Bit of Values loop
         Container.Append (Bit);
      end loop;
   end Append;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      Index     : Index_Type)
      return Bit
   is
   begin
      return Boolean'Pos (Container.Is_Set (Index));
   end Element;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Container : Vector;
      Index     : Index_Type)
      return Boolean
   is
      Byte_Index : Positive;
      Bit_Index  : Natural;
      Element    : Element_Type;
   begin
      Split (Index, Byte_Index, Bit_Index);
      Element := Container.Elements.Element (Byte_Index);
      return (Element and 2 ** Bit_Index) /= 0;
   end Is_Set;

   -----------
   -- Slice --
   -----------

   function Slice (Container : Vector; From, To : Index_Type) return Bit_Array
   is
   begin
      return Result : Bit_Array (1 .. Natural (To - From + 1)) do
         declare
            Index : Index_Type := From;
         begin
            for Bit of Result loop
               Bit := Container.Element (Index);
               Index := Index + 1;
            end loop;
         end;
      end return;
   end Slice;

   -----------
   -- Split --
   -----------

   procedure Split
     (Index      : Index_Type;
      Byte_Index : out Positive;
      Bit_Index  : out Natural)
   is
   begin
      Byte_Index := Natural ((Index - 1) / Element_Type'Size) + 1;
      Bit_Index := Natural ((Index - 1) mod Element_Type'Size);
   end Split;

end WL.Bit_Vectors;
