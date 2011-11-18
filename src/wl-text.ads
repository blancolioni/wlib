with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;

package WL.Text is

   type Text_Replacement is private;

   procedure Add (To        : in out Text_Replacement;
                  Old_Value : in     String;
                  New_Value : in     String);

   function Replace (Text     : String;
                     Using    : Text_Replacement)
                    return String;

private

   subtype Replacement_Key is String (1 .. 32);

   package Replacement_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Replacement_Key,
        Element_Type    => String,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   type Text_Replacement is
      record
         Map : Replacement_Maps.Map;
      end record;

end WL.Text;
