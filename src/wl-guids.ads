with Ada.Containers;

package WL.Guids is

   type Guid is private;

   Null_Guid : constant Guid;

   function New_Guid return Guid;

   function To_String (Id : Guid) return String;

   function Hash (Id : Guid) return Ada.Containers.Hash_Type;

private

   type Element is mod 2 ** 32;

   Element_Count : constant := 4;

   type Guid is array (1 .. Element_Count) of Element;

   Null_Guid : constant Guid := (others => 0);

end WL.Guids;
