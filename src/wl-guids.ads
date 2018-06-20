package WL.Guids is

   type Guid is private;

   function New_Guid return Guid;

   function To_String (Id : Guid) return String;

private

   type Element is mod 2 ** 32;

   Element_Count : constant := 4;

   type Element_Array is array (1 .. Element_Count) of Element;

   type Guid is
      record
         Es : Element_Array;
      end record;

end WL.Guids;
