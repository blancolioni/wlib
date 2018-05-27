with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package WL.Wide_Wide_String_Maps is

   pragma Preelaborate;

   package Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Wide_Wide_String,
        Element_Type    => Element_Type,
        Hash            => Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   type Map is new Maps.Map with null record;

   subtype Cursor is Maps.Cursor;

   Empty_Map : constant Map := Map'(Maps.Empty_Map with null record);

   function Has_Element (Position : Cursor) return Boolean
                         renames Maps.Has_Element;

   function Key (Position : Cursor) return Wide_Wide_String
                 renames Maps.Key;

   function Element (Position : Cursor) return Element_Type
                     renames Maps.Element;

   procedure Next (Position : in out Cursor)
                   renames Maps.Next;

   function Next (Position : Cursor) return Cursor
                  renames Maps.Next;

end WL.Wide_Wide_String_Maps;
