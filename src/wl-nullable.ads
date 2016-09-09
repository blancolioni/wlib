generic
   type Element_Type is private;
package WL.Nullable is

   type Nullable_Type is tagged private;

   function Has_Value (X : Nullable_Type'Class) return Boolean;
   function Value (X : Nullable_Type'Class) return Element_Type
     with Pre => X.Has_Value;

   procedure Set_Value
     (X : in out Nullable_Type'Class;
      E : Element_Type)
     with Post => X.Has_Value;

   procedure Clear_Value
     (X : in out Nullable_Type'Class)
     with Post => not X.Has_Value;

private

   type Nullable_Type is tagged
      record
         Has_Value : Boolean := False;
         Element   : Element_Type;
      end record;

end WL.Nullable;
