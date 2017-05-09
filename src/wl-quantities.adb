with Ada.Strings.Fixed;

package body WL.Quantities is

   Local_Random_Unit_Float : Random_Unit_Float;

   function Significant_Digits_Image (Item : Float;
                                      Sig  : Positive)
                                     return String;

   ------------
   -- Around --
   ------------

   function Around (X          : Quantity_Type;
                    Inflection : Unit_Float := 0.1;
                    Shape      : Distribution_Type := Linear)
                    return Quantity_Type
   is
      Factor : Unit_Float := 1.0;
   begin
      if Local_Random_Unit_Float = null then
         return X;
      end if;

      case Shape is
         when Linear =>
            Factor :=
              1.0 - Inflection + Local_Random_Unit_Float.all * 2.0 * Factor;
         when Quadratic =>
            Factor :=
              1.0 - Inflection + Local_Random_Unit_Float.all * 2.0 * Factor;
         when Bell =>
            Factor :=
              1.0 - Inflection + Local_Random_Unit_Float.all * 2.0 * Factor;
      end case;

      return Quantity_Type (Float (X) * Factor);
   end Around;

   -----------
   -- Image --
   -----------

   function Image (Item : Quantity_Type) return String is

      Factors    : constant array (1 .. 3) of Float :=
        (1.0E9, 1.0E6, 1.0E3);
      Extensions : constant String := "GMK";
   begin
      for I in Factors'Range loop
         if Float (Item) > Factors (I) then
            return Significant_Digits_Image (Float (Item) / Factors (I), 3) &
              (1 => Extensions (I));
         end if;
      end loop;

      return Ada.Strings.Fixed.Trim
        (Quantity_Type'Image (Item), Ada.Strings.Left);

   end Image;

   ---------
   -- Max --
   ---------

   function Max (Left, Right : Quantity_Type) return Quantity_Type is
   begin
      return Quantity_Type'Max (Left, Right);
   end Max;

   ---------
   -- Min --
   ---------

   function Min (Left, Right : Quantity_Type) return Quantity_Type is
   begin
      return Quantity_Type'Min (Left, Right);
   end Min;

   -----------
   -- Scale --
   -----------

   function Scale
     (X      : Quantity_Type;
      Factor : Float)
      return Quantity_Type
   is
   begin
      return Quantity_Type (Float (X) * Factor);
   end Scale;

   ----------------
   -- Scale_Down --
   ----------------

   function Scale_Down
     (Value       : Quantity_Type;
      Numerator   : Quantity_Type;
      Denominator : Quantity_Type)
      return Quantity_Type
   is
   begin
      return Value * Numerator / Denominator;
   end Scale_Down;

   ---------------------------
   -- Set_Random_Unit_Float --
   ---------------------------

   procedure Set_Random_Unit_Float
     (Fn : Random_Unit_Float)
   is
   begin
      Local_Random_Unit_Float := Fn;
   end Set_Random_Unit_Float;

   ------------------------------
   -- Significant_Digits_Image --
   ------------------------------

   function Significant_Digits_Image (Item : Float;
                                      Sig  : Positive)
                                     return String
   is
      Result    : String (1 .. Sig);
      Point     : Natural := 0;
      Acc       : Float := Item;
      Boundary  : constant Float := 10.0**Sig;
   begin
      if Item < 1.0 / Boundary then
         return "0.00";
      end if;

      if abs Item >= Boundary then
         return Ada.Strings.Fixed.Trim (Integer'Image (Integer (Item)),
                                        Ada.Strings.Left);
      else
         while abs Acc * 10.0 < Boundary loop
            Acc := Acc * 10.0;
            Point := Point + 1;
         end loop;

         Result :=
           Ada.Strings.Fixed.Trim (Integer'Image (Integer (Acc - 0.5)),
                                   Ada.Strings.Left);
         if Point < Sig then
            if Point = 0 then
               return Result;
            else
               declare
                  Before : constant String :=
                             Result (1 .. Result'Last - Point);
                  After  : constant String :=
                             Result (Result'Last - Point + 1 .. Result'Last);
               begin
                  if (for all Ch of After => Ch = '0') then
                     return Before;
                  else
                     return Before & "." & After;
                  end if;
               end;
            end if;
         else
            declare
               Zeroes : constant String (1 .. Point - Sig) :=
                 (others => '0');
            begin
               return "0." & Zeroes & Result;
            end;
         end if;
      end if;
   end Significant_Digits_Image;

   --------------
   -- To_Float --
   --------------

   function To_Float (Value : Quantity_Type) return Float is
   begin
      return Float (Value);
   end To_Float;

   ----------------
   -- To_Natural --
   ----------------

   function To_Natural (Value : Quantity_Type) return Natural is
   begin
      return Natural (Value);
   end To_Natural;

   -----------------
   -- To_Quantity --
   -----------------

   function To_Quantity (Value : Float) return Quantity_Type is
   begin
      return Quantity_Type (Value);
   end To_Quantity;

   ----------
   -- Unit --
   ----------

   function Unit return Quantity_Type is
   begin
      return 1;
   end Unit;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Quantity_Type is
   begin
      if Image = "" then
         return Zero;
      elsif Image (Image'First) = '~' then
         return Around (Value (Image (Image'First + 1 .. Image'Last)));
      elsif Image (Image'Last) = 'K' then
         return Quantity_Type'Value (Image (Image'First .. Image'Last - 1))
           * 1E3;
      elsif Image (Image'Last) = 'M' then
         return Quantity_Type'Value (Image (Image'First .. Image'Last - 1))
           * 1E6;
      elsif Image (Image'Last) = 'G' then
         return Quantity_Type'Value (Image (Image'First .. Image'Last - 1))
           * 1E9;
      else
         return Quantity_Type'Value (Image);
      end if;
   end Value;

   ----------
   -- Zero --
   ----------

   function Zero return Quantity_Type is
   begin
      return 0;
   end Zero;

end WL.Quantities;
