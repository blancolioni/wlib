with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Hash_Case_Insensitive;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package WL.String_Maps is

   package Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Element_Type,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive,
        "="             => "=");

   type Map is new Maps.Map with null record;

   subtype Cursor is Maps.Cursor;

   function Has_Element (Position : Cursor) return Boolean
                         renames Maps.Has_Element;

   function Key (Position : Cursor) return String
                 renames Maps.Key;

   function Element (Position : Cursor) return Element_Type
                     renames Maps.Element;

   procedure Next (Position : in out Cursor)
                   renames Maps.Next;

end WL.String_Maps;
