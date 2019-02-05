with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body WL.Numerics.Generic_Dimensioned_Values is

   package Fns is new Ada.Numerics.Generic_Elementary_Functions (Real);

   function "abs" (U : Length_Vector) return Length is
      Zero : constant Length_Vector (U'Range) := (others => 0.0);
   begin
      return Distance (U, Zero);
   end "abs";

   --------------
   -- Distance --
   --------------

   function Distance (U, V : Length_Vector) return Length is
      Sum : Length := 0.0;
   begin
      for I in U'Range loop
         Sum := Sum + (U (I) - V (I)) ** 2;
      end loop;
      return Length (Fns.Sqrt (Real (Sum)));
   end Distance;

   -----------
   -- Image --
   -----------

   function Image (L : Length) return String is
      package Length_IO is new Ada.Text_IO.Float_IO (Length);
      S : String (1 .. 20) := (others => ' ');
   begin
      Length_IO.Put (S, L, 10, 0);
      return Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
   end Image;

end WL.Numerics.Generic_Dimensioned_Values;
