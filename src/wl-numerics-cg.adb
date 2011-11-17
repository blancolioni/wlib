package body WL.Numerics.CG is

   Max_Iterations : constant := 200;
   Epsilon        : constant := 1.0e-18;

   procedure Minimize_Line
     (F    : in     Target_Function;
      P    : in out Real_Arrays.Real_Vector;
      XI   : in out Real_Arrays.Real_Vector;
      FRet :    out Real);

   procedure Mnbrak
     (AX, BX     : in out Real;
      CX         :    out Real;
      FA, FB, FC : out Real;
      Func       : not null access function (X : Real) return Real);

   procedure Brent (AX, BX, CX : Real;
                    Tol        : Real;
                    Xmin       :    out Real;
                    Ret        :    out Real;
                    F          : not null access
                      function (X : Real) return Real);

   procedure Swap (X, Y : in out Real);

   function Sign (X, Y : Real) return Real;

   procedure Shift_3 (A, B, C : in out Real;
                      D       : Real);

   -----------
   -- Brent --
   -----------

   procedure Brent (AX, BX, CX : Real;
                    Tol        : Real;
                    Xmin       :    out Real;
                    Ret        :    out Real;
                    F          : not null access
                      function (X : Real) return Real)
   is
      Max_Iterations : constant := 100;
      CGold          : constant := 0.3819660;
      Epsilon        : constant Real := 1.0 / (10.0 ** Real'Digits);
      Zeps           : constant Real := Epsilon * 1.0e-3;
      A, B           : Real;
      D              : Real := 0.0;
      ETemp          : Real;
      FU, FV, FW, FX : Real;
      P, Q, R        : Real;
      Tol1, Tol2     : Real;
      U, V, W, X, XM : Real;
      E              : Real := 0.0;
   begin

      if AX < CX then
         A := AX;
         B := CX;
      else
         A := CX;
         B := AX;
      end if;

      X := BX;
      W := BX;
      V := BX;

      FX := F (X);
      FW := FX;
      FV := FX;

      for I in 1 .. Max_Iterations loop

         XM := 0.5 * (A + B);
         Tol1 := Tol * (abs X) + Zeps;
         Tol2 := 2.0 * Tol1;
         if abs (X - XM) <= Tol2 - 0.5 * (B - A) then
            Xmin := X;
            Ret := FX;
            return;
         end if;

         if abs E > Tol1 then

            R := (X - W) * (FX - FV);
            Q := (X - V) * (FX - FW);
            P := (X - V) * Q - (X - W) * R;
            Q := 2.0 * (Q - R);
            if Q > 0.0 then
               P := -P;
            end if;
            Q := abs Q;
            ETemp := E;
            E := D;
            if abs P >= abs (0.5 * Q * ETemp)
              or else P <= Q * (A - X)
              or else P > Q * (B - X)
            then
               if X >= XM then
                  E := A - X;
               else
                  E := B - X;
               end if;
               D := CGold * E;
            else
               D := P / Q;
               U := X + D;
               if U - A < Tol2 or else B - U < Tol2 then
                  D := Sign (Tol1, XM - X);
               end if;
            end if;
         else
            if X >= XM then
               E := A - X;
            else
               E := B - X;
            end if;
            D := CGold * E;
         end if;

         if abs D >= Tol1 then
            U := X + D;
         else
            U := X + Sign (Tol1, D);
         end if;

         FU := F (U);

         if FU <= FX then
            if U >= X then
               A := X;
            else
               B := X;
            end if;
            Shift_3 (V, W, X, U);
            Shift_3 (FV, FW, FX, FU);
         else
            if U < X then
               A := U;
            else
               B := U;
            end if;
            if FU < FW or else W = X then
               V := W;
               W := U;
               FV := FW;
               FW := FU;
            elsif FU <= FV or else V = X or else V = W then
               V := U;
               FV := FU;
            end if;
         end if;

      end loop;

      raise Constraint_Error with "too many iterations in brent";

   end Brent;


   --------------
   -- Minimize --
   --------------

   procedure Minimize
     (F          : Target_Function;
      DF         : Deriviative_Function;
      Tolerance  : Real;
      P          : in out Real_Arrays.Real_Vector;
      Iterations : out Natural;
      F_Value    : out Real)
   is
      use Real_Arrays;
      XI : Real_Vector := DF (P);
      G  : Real_Vector := -XI;
      H  : Real_Vector := G;
      FP : Real       := F (P);
   begin
      XI := G;

      for It in 1 .. Max_Iterations loop

         Iterations := It;

         Minimize_Line (F, P, XI, F_Value);

         if 2.0 * abs (F_Value - FP) <=
           Tolerance * (abs F_Value + abs FP + Epsilon)
         then
            return;
         end if;

         FP := F_Value;
         XI := DF (P);

         declare
            DGG : Real := 0.0;
            GG  : Real := 0.0;
            GAM : Real;
         begin
            for J in P'Range loop
               GG := GG + G (J) ** 2;
               DGG := DGG + (XI (J) + G (J)) * XI (J);
            end loop;

            if GG = 0.0 then
               return;
            end if;

            GAM := DGG / GG;

            G := -XI;
            XI := G + GAM * H;
            H  := XI;
         end;

      end loop;

      raise Constraint_Error with
        "CG: too many iterations";

   end Minimize;

   -------------------
   -- Minimize_Line --
   -------------------

   procedure Minimize_Line
     (F    : in     Target_Function;
      P    : in out Real_Arrays.Real_Vector;
      XI   : in out Real_Arrays.Real_Vector;
      FRet :    out Real)
   is
      use Real_Arrays;
      Tolerance  : constant := 1.0e-8;
      AX         : Real := 0.0;
      XX         : Real := 1.0;
      Xmin       : Real;
      FX, FB, FA : Real;
      BX         : Real;

      function F1Dim (X : Real) return Real;

      -----------
      -- F1Dim --
      -----------

      function F1Dim (X : Real) return Real is
         XT : Real_Vector (P'Range);
      begin
         for I in XT'Range loop
            XT (I) := P (I) + X * XI (I);
         end loop;
         return F (XT);
      end F1Dim;

   begin
      Mnbrak (AX, XX, BX, FA, FX, FB, F1Dim'Access);
      Brent (AX, XX, BX, Tolerance, Xmin, FRet, F1Dim'Access);

      for I in XI'Range loop
         XI (I) := XI (I) * Xmin;
         P (I) := P (I) + XI (I);
      end loop;

   end Minimize_Line;

   ------------
   -- Mnbrak --
   ------------

   procedure Mnbrak
     (AX, BX     : in out Real;
      CX         :    out Real;
      FA, FB, FC : out Real;
      Func       : not null access function (X : Real) return Real)
   is
      Gold   : constant := 1.618034;
      Glimit : constant := 100.0;
      Tiny   : constant := 1.0e-20;
      Ulim   : Real;
      U, R, Q, FU : Real;
   begin
      FA := Func (AX);
      FB := Func (BX);
      if FB > FA then
         Swap (AX, BX);
         Swap (FB, FA);
      end if;

      CX := BX + Gold * (BX - AX);
      FC := Func (CX);

      while FB > FC loop
         R := (BX - AX) * (FB - FC);
         Q := (BX - CX) * (FB - FA);
         U := BX - ((BX - CX) * Q - (BX - AX) * R) /
           (2.0 * Sign (Real'Max (abs (Q - R), Tiny), Q - R));
         Ulim := BX + Glimit * (CX - BX);
         if (BX - U) * (U - CX) > 0.0 then

            FU := Func (U);
            if FU < FC then
               AX := BX;
               BX := U;
               FA := FB;
               FB := FU;
               return;
            elsif FU > FB then
               CX := U;
               FC := FU;
               return;
            end if;
            U := CX + Gold * (CX - BX);
            FU := Func (U);
         elsif (CX - U) * (U - Ulim) > 0.0 then
            FU := Func (U);
            if FU < FC then
               Shift_3 (BX, CX, U, U + Gold * (U - CX));
               Shift_3 (FB, FC, FU, Func (U));
            end if;
         elsif (U - Ulim) * (Ulim - CX) >= 0.0 then
            U := Ulim;
            FU := Func (U);
         else
            U := CX + Gold * (CX - BX);
            FU := Func (U);
         end if;
         Shift_3 (AX, BX, CX, U);
         Shift_3 (FA, FB, FC, FU);
      end loop;

   end Mnbrak;

   -------------
   -- Shift_3 --
   -------------

   procedure Shift_3 (A, B, C : in out Real;
                      D       : Real)
   is
   begin
      A := B;
      B := C;
      C := D;
   end Shift_3;

   ----------
   -- Sign --
   ----------

   function Sign (X, Y : Real) return Real is
   begin
      if Y >= 0.0 then
         return abs X;
      else
         return -(abs X);
      end if;
   end Sign;

   ----------
   -- Swap --
   ----------

   procedure Swap (X, Y : in out Real) is
      T : constant Real := X;
   begin
      X := Y;
      Y := T;
   end Swap;

end WL.Numerics.CG;
