with Ada.Strings.Fixed;

package body WL.Money is

   Local_Currency_Symbol       : String := "$   ";
   Local_Digit_Grouping_Symbol : String := ",   ";
   Local_Decimal_Symbol        : String := ".   ";

   -------------
   -- Add_Tax --
   -------------

   function Add_Tax (Money    : Money_Type;
                     Tax_Rate : Float)
                     return Money_Type
   is
   begin
      if Money > 0 then
         return Money + Tax (Money, Tax_Rate);
      else
         return Money;
      end if;
   end Add_Tax;

   -------------
   -- Add_Tax --
   -------------

   function Add_Tax
     (Price : Price_Type;
      Tax_Rate : Float)
      return Price_Type
   is
   begin
      return Price_Type (Add_Tax (Money_Type (Price), Tax_Rate));
   end Add_Tax;

   ------------
   -- Adjust --
   ------------

   function Adjust (Money    : Money_Type;
                    Factor   : Float)
                    return Money_Type
   is
   begin
      return To_Money (To_Float (Money) * Factor);
   end Adjust;

   ------------------
   -- Adjust_Price --
   ------------------

   function Adjust_Price (Price    : Price_Type;
                          Factor   : Float)
                         return Price_Type
   is
   begin
      return Price_Type (Float (Price) * Factor);
   end Adjust_Price;

   ---------------------
   -- Currency_Symbol --
   ---------------------

   function Currency_Symbol return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Local_Currency_Symbol, Ada.Strings.Right);
   end Currency_Symbol;

   --------------------
   -- Decimal_Symbol --
   --------------------

   function Decimal_Symbol return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Local_Decimal_Symbol, Ada.Strings.Right);
   end Decimal_Symbol;

   ---------------------------
   -- Digit_Grouping_Symbol --
   ---------------------------

   function Digit_Grouping_Symbol return String is
   begin
      return Ada.Strings.Fixed.Trim
        (Local_Digit_Grouping_Symbol, Ada.Strings.Right);
   end Digit_Grouping_Symbol;

   ------------------
   -- Get_Quantity --
   ------------------

   function Get_Quantity
     (Total_Cash : Money_Type;
      Price      : Price_Type)
      return Quantities.Quantity_Type
   is
   begin
      return Quantities.To_Quantity
        (Float (Total_Cash / Money_Type (Price)));
   end Get_Quantity;

   -----------
   -- Image --
   -----------

   function Image (Item : Money_Type) return String is
   begin
      if Item < 0 then
         return "-" & Image (Price_Type (-Item));
      else
         return Image (Price_Type (Item));
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Item : Price_Type) return String is
      Base_Image : constant String :=
                     Ada.Strings.Fixed.Trim
                       (Price_Type'Image (Item / 1000),
                        Ada.Strings.Left);
      Decimal_Image : constant String :=
                        Ada.Strings.Fixed.Trim
                          (Price_Type'Image (Item mod 1000),
                           Ada.Strings.Left);
   begin
      return Result : String := Base_Image & ".000" do
         Result (Result'Last - Decimal_Image'Length + 1 .. Result'Last) :=
           Decimal_Image;
      end return;
   end Image;

   ---------
   -- Max --
   ---------

   function Max (X, Y : Money_Type) return Money_Type is
   begin
      return Money_Type'Max (X, Y);
   end Max;

   ---------
   -- Max --
   ---------

   function Max (X, Y : Price_Type) return Price_Type is
   begin
      return Price_Type'Max (X, Y);
   end Max;

   ---------
   -- Min --
   ---------

   function Min (X, Y : Money_Type) return Money_Type is
   begin
      return Money_Type'Min (X, Y);
   end Min;

   ---------
   -- Min --
   ---------

   function Min (X, Y : Price_Type) return Price_Type is
   begin
      return Price_Type'Min (X, Y);
   end Min;

   -----------
   -- Price --
   -----------

   function Price (Total    : Money_Type;
                   Quantity_Type : Quantities.Quantity_Type)
                   return Price_Type
   is
      use type Quantities.Quantity_Type;
   begin
      if Quantity_Type = Quantities.Zero then
         return Zero;
      else
         return Price_Type
           (Float (Total) / Quantities.To_Float (Quantity_Type));
      end if;
   end Price;

   --------------------------
   -- Set_Image_Properties --
   --------------------------

   procedure Set_Image_Properties
     (Currency_Symbol       : String := "$";
      Digit_Grouping_Symbol : String := ",";
      Decimal_Symbol        : String := ".")
   is
   begin
      Local_Currency_Symbol (1 .. Currency_Symbol'Length) :=
        Currency_Symbol;
      Local_Digit_Grouping_Symbol (1 .. Digit_Grouping_Symbol'Length) :=
        Digit_Grouping_Symbol;
      Local_Decimal_Symbol (1 .. Decimal_Symbol'Length) :=
        Decimal_Symbol;
   end Set_Image_Properties;

   ----------
   -- Show --
   ----------

   function Show (Item : Money_Type) return String is
   begin
      if Item < 0 then
         return "(" & Show (Price_Type (abs Item)) & ")";
      else
         return Show (Price_Type (Item));
      end if;
   end Show;

   ----------
   -- Show --
   ----------

   function Show (Item : Price_Type) return String is
      Image    : constant String :=
                   Ada.Strings.Fixed.Trim (Price_Type'Image ((Item + 5) / 10),
                                           Ada.Strings.Left);
      Currency : constant String := Currency_Symbol;

      function Group (S : String) return String
      is (if S'Length <= 3
          then S
          else Group (S (S'First .. S'Last - 3))
          & Digit_Grouping_Symbol
          & S (S'Last - 2 .. S'Last));

   begin
      if Image'Length = 1 then
         return Currency & "0" & Decimal_Symbol & "0" & Image;
      elsif Image'Length = 2 then
         return Currency & "0" & Decimal_Symbol & Image;
      else
         return Currency
           & Group (Image (Image'First .. Image'Last - 2))
           & Decimal_Symbol
           & Image (Image'Last - 1 .. Image'Last);
      end if;
   end Show;

   -----------
   -- Split --
   -----------

   function Split
     (Amount  : Money_Type;
      Portion : Float)
      return Money_Type
   is
   begin
      return Money_Type (Float (Amount) * Portion);
   end Split;

   ---------
   -- Tax --
   ---------

   function Tax (Money : Money_Type;
                 Tax   : Float)
                 return Money_Type
   is
   begin
      if Money < 0 then
         return 0;
      else
         return Money_Type (Float (Money) * Tax);
      end if;
   end Tax;

   ---------
   -- Tax --
   ---------

   function Tax (Price   : Price_Type;
                            Tax     : Float)
                            return Price_Type
   is
   begin
      return Price_Type (Float (Price) * Tax);
   end Tax;

   --------------
   -- To_Float --
   --------------

   function To_Float (Amount : Money_Type) return Float is
   begin
      return Float (Amount) / 1000.0;
   end To_Float;

   --------------
   -- To_Money --
   --------------

   function To_Money (Amount : Float) return Money_Type is
   begin
      return Money_Type (Amount * 1000.0);
   end To_Money;

   -----------
   -- Total --
   -----------

   function Total (Price  : Price_Type;
                   Quantity_Type : Quantities.Quantity_Type)
                  return Money_Type
   is
      use Quantities;
   begin
      return Money_Type (Float (Price) * To_Float (Quantity_Type));
   end Total;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Money_Type is
      Currency : constant String := Currency_Symbol;
   begin
      if Image = "" then
         return Zero;
      elsif Image (Image'First) = '~' then
         declare
            Result : constant Money_Type :=
                       Value (Image (Image'First + 1 .. Image'Last));
         begin
            return Result;
         end;
      elsif Image'Length > Currency'Length
        and then Image (Image'First .. Image'First + Currency'Length - 1)
        = Currency
      then
         return Value (Image (Image'First + Currency'Length .. Image'Last));
      elsif Image (Image'Last) = 'K' then
         return Value (Image (Image'First .. Image'Last - 1)) * 1E3;
      elsif Image (Image'Last) = 'M' then
         return Value (Image (Image'First .. Image'Last - 1)) * 1E6;
      elsif Image (Image'Last) = 'G' then
         return Value (Image (Image'First .. Image'Last - 1)) * 1E9;
      else
         return Money_Type (Float'Value (Image));
      end if;
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Price_Type is
   begin
      return To_Price (Float'Value (Image));
   end Value;

   -----------------
   -- Without_Tax --
   -----------------

   function Without_Tax
     (Money : Money_Type;
      Tax   : Float)
      return Money_Type
   is
   begin
      if Money < 0 then
         return Money;
      else
         return Money_Type (Float (Money) / (1.0 + Tax));
      end if;
   end Without_Tax;

   -----------------
   -- Without_Tax --
   -----------------

   function Without_Tax
     (Price   : Price_Type;
      Tax     : Float)
      return Price_Type
   is
   begin
      return Price_Type (Without_Tax (Money_Type (Price), Tax));
   end Without_Tax;

   ----------
   -- Zero --
   ----------

   function Zero return Money_Type is
   begin
      return 0;
   end Zero;

   ----------
   -- Zero --
   ----------

   function Zero return Price_Type is
   begin
      return 0;
   end Zero;

end WL.Money;
