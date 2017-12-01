with WL.Quantities;

package WL.Money is

   type Money_Type is private;

   function Zero return Money_Type;

   function "+" (Left, Right : Money_Type) return Money_Type;
   function "-" (Left, Right : Money_Type) return Money_Type;

   function "<"  (Left, Right : Money_Type) return Boolean;
   function ">"  (Left, Right : Money_Type) return Boolean;
   function "<=" (Left, Right : Money_Type) return Boolean;
   function ">=" (Left, Right : Money_Type) return Boolean;

   function "abs" (Left : Money_Type) return Money_Type;

   function Adjust (Money    : Money_Type;
                    Factor   : Float)
                   return Money_Type;

   function To_Money (Amount : Float) return Money_Type;
   function To_Float (Amount : Money_Type) return Float;

   function Max (X, Y : Money_Type) return Money_Type;
   function Min (X, Y : Money_Type) return Money_Type;

   function Tax (Money : Money_Type;
                 Tax   : Float)
                 return Money_Type;

   function Without_Tax (Money : Money_Type;
                         Tax   : Float)
                         return Money_Type;

   function Add_Tax (Money    : Money_Type;
                     Tax_Rate : Float)
                     return Money_Type;

   type Price_Type is private;

   function "+" (Left, Right : Price_Type) return Price_Type;
   function "-" (Left, Right : Price_Type) return Price_Type;

   function "<"  (Left, Right : Price_Type) return Boolean;
   function ">"  (Left, Right : Price_Type) return Boolean;
   function "<=" (Left, Right : Price_Type) return Boolean;
   function ">=" (Left, Right : Price_Type) return Boolean;

   function To_Price (Amount : Float) return Price_Type;
   function To_Float (Price : Price_Type) return Float;

   function Adjust_Price (Price    : Price_Type;
                          Factor   : Float)
                         return Price_Type;

   function Tax (Price   : Price_Type;
                 Tax     : Float)
                 return Price_Type;

   function Without_Tax (Price   : Price_Type;
                         Tax     : Float)
                         return Price_Type;

   function Add_Tax (Price : Price_Type;
      Tax_Rate : Float)
                     return Price_Type;

   function Max (X, Y : Price_Type) return Price_Type;
   function Min (X, Y : Price_Type) return Price_Type;

   function Total (Price    : Price_Type;
                   Quantity_Type : Quantities.Quantity_Type)
                  return Money_Type;

   function Total (Price    : Price_Type;
                   Quantity : Float)
                   return Money_Type;

   function Price (Total    : Money_Type;
                   Quantity_Type : Quantities.Quantity_Type)
                   return Price_Type;

   function Get_Quantity
     (Total_Cash : Money_Type;
      Price      : Price_Type)
      return Quantities.Quantity_Type;

   function Image (Item : Money_Type) return String;
   function Image (Item : Price_Type) return String;
   --  Raw image functions

   function Show (Item : Money_Type) return String;
   function Show (Item : Price_Type) return String;
   --  User-friendly image functions

   function Value (Image : String) return Money_Type;
   function Value (Image : String) return Price_Type;

   function Split (Amount  : Money_Type;
                   Portion : Float)
                  return Money_Type;

   function Zero return Price_Type;

   function Currency_Symbol return String;
   function Digit_Grouping_Symbol return String;
   function Decimal_Symbol return String;

   procedure Set_Image_Properties
     (Currency_Symbol       : String := "$";
      Digit_Grouping_Symbol : String := ",";
      Decimal_Symbol        : String := ".");

private

   type Money_Type is range -2 ** 63 .. 2 ** 63 - 1;
   type Price_Type is range 0 .. 2 ** 63 - 1;

   function To_Price (Amount : Float) return Price_Type
   is (Price_Type (To_Money (Amount)));

   function To_Float (Price : Price_Type) return Float
   is (To_Float (Money_Type (Price)));

   function Total (Price    : Price_Type;
                   Quantity : Float)
                   return Money_Type
   is (To_Money (To_Float (Price) * Quantity));

   pragma Import (Intrinsic, "+");
   pragma Import (Intrinsic, "-");
   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

end WL.Money;
