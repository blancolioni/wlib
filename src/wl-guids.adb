with Ada.Numerics.Discrete_Random;

package body WL.Guids is

   package Element_Random is
     new Ada.Numerics.Discrete_Random (Element);

   Gen : Element_Random.Generator;

   subtype Element_String is String (1 .. 8);

   function To_Hex (E : Element) return Element_String;

   --------------
   -- New_Guid --
   --------------

   function New_Guid return Guid is
   begin
      return G : Guid do
         for E of G.Es loop
            E := Element_Random.Random (Gen);
         end loop;
      end return;
   end New_Guid;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (E : Element) return Element_String is
      Ds : constant String := "0123456789abcedf";
      It : Element := E;
   begin
      return H : Element_String do
         for Ch of reverse H loop
            Ch := Ds (Natural (It mod 16) + 1);
            It := It / 16;
         end loop;
      end return;
   end To_Hex;

   ---------------
   -- To_String --
   ---------------

   function To_String (Id : Guid) return String is
      Images : array (Id.Es'Range) of Element_String;
   begin
      for I in Images'Range loop
         Images (I) := To_Hex (Id.Es (I));
      end loop;
      return Images (1) & "-" & Images (2) (1 .. 4)
        & "-" & Images (2) (5 .. 8) & "-"
        & Images (3) (1 .. 4) & "-"
        & Images (3) (5 .. 8) & Images (4);
   end To_String;

end WL.Guids;
