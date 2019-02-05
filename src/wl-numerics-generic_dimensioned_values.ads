generic
   type Real is digits <>;
package WL.Numerics.Generic_Dimensioned_Values is

   type Time is private;

   function Seconds (X : Duration) return Time;
   function Seconds (X : Real) return Time;
   function Minutes (X : Real) return Time;
   function Hours (X : Real) return Time;

   type Mass is private;

   function Kilograms (X : Real) return Mass;
   function Tonnes (X : Real) return Mass;

   function "+" (Left, Right : Mass) return Mass;
   function "-" (Left, Right : Mass) return Mass;

   function "*" (Left : Real; Right : Mass) return Mass;
   function "*" (Left : Mass; Right : Real) return Mass;

   type Length is private;

   function ">" (Left, Right : Length) return Boolean;
   function ">=" (Left, Right : Length) return Boolean;
   function "<" (Left, Right : Length) return Boolean;
   function "<=" (Left, Right : Length) return Boolean;

   function Metres (X : Real) return Length;
   function Kilometres (X : Real) return Length;

   function Get_Metres (L : Length) return Real;

   function "+" (Left, Right : Length) return Length;
   function "-" (Left, Right : Length) return Length;
   function "*" (Left : Real; Right : Length) return Length;
   function "*" (Left : Length; Right : Real) return Length;

   function Image (L : Length) return String;

   type Length_Vector is array (Positive range <>) of Length;

   function "abs" (U : Length_Vector) return Length;
   function Distance (U, V : Length_Vector) return Length;

   type Speed is private;

   function Zero return Speed;

   function Kilometres_Per_Hour
     (X : Real)
      return Speed;

   function Metres_Per_Second
     (X : Real)
      return Speed;

   function ">" (Left, Right : Speed) return Boolean;
   function ">=" (Left, Right : Speed) return Boolean;
   function "<" (Left, Right : Speed) return Boolean;
   function "<=" (Left, Right : Speed) return Boolean;

   function "+" (Left, Right : Speed) return Speed;
   function "-" (Left, Right : Speed) return Speed;

   function "/" (Left  : Length;
                 Right : Time)
                 return Speed;

   function Get_Metres_Per_Second (X : Speed) return Real;
   function Get_Kilometres_Per_Hour (X : Speed) return Real;

   type Acceleration is private;

   function Metres_Per_Second_Per_Second
     (X : Real)
      return Acceleration;

   function "+" (Left, Right : Acceleration) return Acceleration;
   function "-" (Left, Right : Acceleration) return Acceleration;

   function "/" (Left : Speed;
                 Right : Time)
                 return Acceleration;

   function "/" (Left : Speed;
                 Right : Acceleration)
                 return Time;

   function "*" (Left  : Acceleration;
                 Right : Time)
                 return Speed;

   function To_Length
     (U : Speed;
      A : Acceleration;
      T : Time)
      return Length;

   type Force is private;

   function Newtons (X : Real) return Force;

   function "+" (Left, Right : Force) return Force;
   function "-" (Left, Right : Force) return Force;

   function "*" (Left : Real; Right : Force) return Force;
   function "*" (Left : Force; Right : Real) return Force;

   function "*"
     (Left  : Mass;
      Right : Acceleration)
      return Force;

   function "/"
     (Left  : Force;
      Right : Mass)
      return Acceleration;

private

   type Time is new Real;

   function Seconds (X : Real) return Time
   is (Time (X));

   function Seconds (X : Duration) return Time
   is (Time (X));

   function Minutes (X : Real) return Time
   is (Time (X * 60.0));

   function Hours (X : Real) return Time
   is (Time (X * 3600.0));

   type Mass is new Real;

   overriding function "+" (Left, Right : Mass) return Mass
   is (Mass (Real (Left) + Real (Right)));

   overriding function "-" (Left, Right : Mass) return Mass
   is (Mass (Real (Left) - Real (Right)));

   function "*" (Left : Real; Right : Mass) return Mass
   is (Mass (Left) * Right);

   function "*" (Left : Mass; Right : Real) return Mass
   is (Left * Mass (Right));

   function Kilograms (X : Real) return Mass
   is (Mass (X));

   function Tonnes (X : Real) return Mass
   is (Mass (X * Real'(1000.0)));

   type Length is new Real;

   overriding function "+" (Left, Right : Length) return Length
   is (Length (Real (Left) + Real (Right)));

   overriding function "-" (Left, Right : Length) return Length
   is (Length (Real (Left) - Real (Right)));

   function Metres (X : Real) return Length
   is (Length (X));

   function Kilometres (X : Real) return Length
   is (Length (X * Real'(1000.0)));

   function "*" (Left : Real; Right : Length) return Length
   is (Length (Left) * Right);

   function "*" (Left : Length; Right : Real) return Length
   is (Left * Length (Right));

   function Get_Metres (L : Length) return Real
   is (Real (L));

   type Speed is new Real;

   overriding function "+" (Left, Right : Speed) return Speed
   is (Speed (Real (Left) + Real (Right)));

   overriding function "-" (Left, Right : Speed) return Speed
   is (Speed (Real (Left) - Real (Right)));

   function "/"
     (Left  : Length;
      Right : Time)
      return Speed
   is (Speed (Real (Left) / Real (Right)));

   function Zero return Speed is (0.0);

   function Kilometres_Per_Hour
     (X : Real)
      return Speed
   is (Kilometres (X) / Hours (1.0));

   function Metres_Per_Second
     (X : Real)
      return Speed
   is (Speed (X));

   function Get_Metres_Per_Second (X : Speed) return Real
   is (Real (X));

   function Get_Kilometres_Per_Hour (X : Speed) return Real
   is (Real (X) * 3.6);

   type Acceleration is new Real;

   function Metres_Per_Second_Per_Second
     (X : Real)
      return Acceleration
   is (Acceleration (X));

   overriding function "+" (Left, Right : Acceleration) return Acceleration
   is (Acceleration (Real (Left) + Real (Right)));

   overriding function "-" (Left, Right : Acceleration) return Acceleration
   is (Acceleration (Real (Left) - Real (Right)));

   function "*" (Left  : Acceleration;
                 Right : Time)
                 return Speed
   is (Speed (Real (Left) * Real (Right)));

   function "/" (Left  : Speed;
                 Right : Time)
                 return Acceleration
   is (Acceleration (Real (Left) / Real (Right)));

   function "/" (Left  : Speed;
                 Right : Acceleration)
                 return Time
   is (Time (Real (Left) / Real (Right)));

   function To_Length
     (U : Speed;
      A : Acceleration;
      T : Time)
      return Length
   is (Length (Real (U) * Real (T) + 0.5 * Real (A) * Real (T) ** 2));

   type Force is new Real;

   function Newtons (X : Real) return Force
   is (Force (X));

   function "*" (Left : Real; Right : Force) return Force
   is (Force (Left) * Right);

   function "*" (Left : Force; Right : Real) return Force
   is (Left * Force (Right));

   function "*"
     (Left  : Mass;
      Right : Acceleration)
      return Force
   is (Force (Real (Left) * Real (Right)));

   function "/"
     (Left  : Force;
      Right : Mass)
      return Acceleration
   is (Acceleration (Real (Left) / Real (Right)));

   overriding function "+" (Left, Right : Force) return Force
   is (Force (Real (Left) + Real (Right)));

   overriding function "-" (Left, Right : Force) return Force
   is (Force (Real (Left) - Real (Right)));

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

end WL.Numerics.Generic_Dimensioned_Values;
