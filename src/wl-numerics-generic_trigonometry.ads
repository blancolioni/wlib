private with Ada.Numerics;

generic
   type Real is digits <>;
package WL.Numerics.Generic_Trigonometry is

   subtype Signed_Unit_Real is Real range -1.0 .. 1.0;

   type Angle is private;

   Pi : constant Angle;

   function "+" (X, Y : Angle) return Angle;
   function "-" (X, Y : Angle) return Angle;

   function Sin (Theta : Angle) return Signed_Unit_Real;
   function Cos (Theta : Angle) return Signed_Unit_Real;
   function Tan (Theta : Angle) return Real;

   function Arcsin (Y : Signed_Unit_Real) return Angle;
   function Arccos (Y : Signed_Unit_Real) return Angle;

   function Arctan (Y : Real;
                    X : Real := 1.0)
                    return Angle;

   function From_Radians (Radians : Real) return Angle;
   function From_Degrees (Degrees : Real) return Angle;

   function To_Radians (Theta : Angle) return Real;
   function To_Degrees (Theta : Angle) return Real;

   function Image (X : Angle) return String;

private

   type Angle is new Real range -Ada.Numerics.Pi .. Ada.Numerics.Pi;

   Pi : constant Angle := Angle (Ada.Numerics.Pi);

end WL.Numerics.Generic_Trigonometry;
