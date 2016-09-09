package body WL.Nullable is

   -----------------
   -- Clear_Value --
   -----------------

   procedure Clear_Value
     (X : in out Nullable_Type'Class)
   is
   begin
      X.Has_Value := False;
   end Clear_Value;

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value (X : Nullable_Type'Class) return Boolean is
   begin
      return X.Has_Value;
   end Has_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (X : in out Nullable_Type'Class;
      E : Element_Type)
   is
   begin
      X.Element := E;
   end Set_Value;

   -----------
   -- Value --
   -----------

   function Value
     (X : Nullable_Type'Class)
      return Element_Type
   is
   begin
      return X.Element;
   end Value;

end WL.Nullable;
