with Ada.Numerics.Generic_Real_Arrays;
with Ada.Text_IO;

with WL.Numerics.CG;

package body CG_Test is

   package Long_Float_Arrays is
     new Ada.Numerics.Generic_Real_Arrays (Long_Float);

   package My_CG is
      new WL.Numerics.CG (Long_Float, Long_Float_Arrays);

   function F (Xs : Long_Float_Arrays.Real_Vector) return Long_Float;

   function Fd (Xs : Long_Float_Arrays.Real_Vector)
                return Long_Float_Arrays.Real_Vector;

   function F (Xs : Long_Float_Arrays.Real_Vector) return Long_Float is
      pragma Unreferenced (Xs);
   begin
      return 0.0;
   end F;

   function Fd (Xs : Long_Float_Arrays.Real_Vector)
               return Long_Float_Arrays.Real_Vector
   is
      Result : Long_Float_Arrays.Real_Vector := Xs;
   begin
      Result := (others => 0.0);
      return Result;
   end Fd;

   ----------
   -- Test --
   ----------

   procedure Test is
      P : Long_Float_Arrays.Real_Vector := (1.0, 1.0);
      Its : Positive := 10;
      F_Value : Long_Float;
   begin

      My_CG.Minimize (F          => F'Access,
                      DF         => Fd'Access,
                      Tolerance  => 1.0e-12,
                      P          => P,
                      Iterations => Its,
                      F_Value    => F_Value);
      Ada.Text_IO.Put_Line (F_Value'Img);
   end Test;

end CG_Test;
