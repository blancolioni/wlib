package WL.Quantities is

   subtype Unit_Float is Float range 0.0 .. 1.0;

   type Quantity_Type is private;

   function Zero return Quantity_Type;
   function Unit return Quantity_Type;

   function To_Quantity (Value : Float) return Quantity_Type;
   function To_Float (Value : Quantity_Type) return Float;
   function To_Natural (Value : Quantity_Type) return Natural;

   function Image (Item : Quantity_Type) return String;
   function Value (Image : String) return Quantity_Type;

   function Show (Item : Quantity_Type) return String;

   function "*" (Left, Right : Quantity_Type) return Quantity_Type;
   function "/" (Left, Right : Quantity_Type) return Quantity_Type;
   function "+" (Left, Right : Quantity_Type) return Quantity_Type;
   function "-" (Left, Right : Quantity_Type) return Quantity_Type;
   function "<" (Left, Right : Quantity_Type) return Boolean;
   function ">" (Left, Right : Quantity_Type) return Boolean;
   function "<=" (Left, Right : Quantity_Type) return Boolean;
   function ">=" (Left, Right : Quantity_Type) return Boolean;

   function Min (Left, Right : Quantity_Type) return Quantity_Type;
   function Max (Left, Right : Quantity_Type) return Quantity_Type;

   function "abs" (X : Quantity_Type) return Quantity_Type;

   function Scale
     (X : Quantity_Type;
      Factor : Float)
      return Quantity_Type;

   function Scale_Down
     (Value       : Quantity_Type;
      Numerator   : Quantity_Type;
      Denominator : Quantity_Type)
      return Quantity_Type;

   type Random_Unit_Float is access
     function return Unit_Float;

   procedure Set_Random_Unit_Float
     (Fn : Random_Unit_Float);

   type Distribution_Type is (Linear, Quadratic, Bell);

   function Around (X          : Quantity_Type;
                    Inflection : Unit_Float := 0.1;
                    Shape      : Distribution_Type := Linear)
                    return Quantity_Type;

private

   type Quantity_Type is range 0 .. 2 ** 63 - 1;

   pragma Import (Intrinsic, "*");
   pragma Import (Intrinsic, "/");
   pragma Import (Intrinsic, "+");
   pragma Import (Intrinsic, "-");
   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

end WL.Quantities;
