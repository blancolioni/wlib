with Ada.Numerics.Generic_Real_Arrays;

generic
   type Real is digits <>;
   with package Real_Arrays is
     new Ada.Numerics.Generic_Real_Arrays (Real);
package WL.Numerics.CG is

   type Target_Function is access
     function (X : Real_Arrays.Real_Vector) return Real;

   type Deriviative_Function is access
     function (X : Real_Arrays.Real_Vector) return Real_Arrays.Real_Vector;

   procedure Minimize
     (F          : Target_Function;
      DF         : Deriviative_Function;
      Tolerance  : Real;
      P          : in out Real_Arrays.Real_Vector;
      Iterations : out Natural;
      F_Value    : out Real);

end WL.Numerics.CG;
