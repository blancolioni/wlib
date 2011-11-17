generic
   type Element_Type is private;
   type Index_Type is (<>);
   type Element_List is array (Index_Type range <>) of Element_Type;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
procedure WL.Sorts.Insertion_Sort (E : in out Element_List);
