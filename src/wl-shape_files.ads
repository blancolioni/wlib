private with Ada.Containers.Vectors;
private with WL.Binary_IO;

package WL.Shape_Files is

   type File_Type is private;

   procedure Open
     (File : in out File_Type;
      Name : in     String);

   procedure Close (File : in out File_Type);

   type Shape_Class is
     (Null_Shape,
      Point_Shape,
      Polyline_Shape,
      Polygon_Shape);

   type Shape (Class : Shape_Class) is private;

   function End_Of_File
     (File : File_Type)
      return Boolean;

   function Next
     (File : in out File_Type)
      return Shape
   with Pre => not End_Of_File (File);

private

   type File_Header is
      record
         File_Code  : WL.Binary_IO.Word_32;
         Unused_1   : WL.Binary_IO.Word_32;
         Unused_2   : WL.Binary_IO.Word_32;
         Unused_3   : WL.Binary_IO.Word_32;
         Unused_4   : WL.Binary_IO.Word_32;
         Unused_5   : WL.Binary_IO.Word_32;
         Length     : WL.Binary_IO.Word_32;
         Version    : WL.Binary_IO.Word_32;
         Shape_Type : WL.Binary_IO.Word_32;
         Min_X      : Long_Float;
         Min_Y      : Long_Float;
         Max_X      : Long_Float;
         Max_Y      : Long_Float;
         Min_Z      : Long_Float;
         Max_Z      : Long_Float;
         Min_M      : Long_Float;
         Max_M      : Long_Float;
      end record;

   for File_Header use
      record
         File_Code  at 0 range 0 .. 31;
         Unused_1   at 4 range 0 .. 31;
         Unused_2   at 8 range 0 .. 31;
         Unused_3   at 12 range 0 .. 31;
         Unused_4   at 16 range 0 .. 31;
         Unused_5   at 20 range 0 .. 31;
         Length     at 24 range 0 .. 31;
         Version    at 28 range 0 .. 31;
         Shape_Type at 32 range 0 .. 31;
         Min_X at 36 range 0 .. 63;
         Min_Y at 44 range 0 .. 63;
         Max_X at 52 range 0 .. 63;
         Max_Y at 60 range 0 .. 63;
         Min_Z at 68 range 0 .. 63;
         Max_Z at 76 range 0 .. 63;
         Min_M at 84 range 0 .. 63;
         Max_M at 92 range 0 .. 63;
      end record;

   for File_Header'Size use 800;

   type File_Type is
      record
         File   : WL.Binary_IO.File_Type;
         Header : File_Header;
      end record;

   type Bounding_Rectangle is
      record
         Min_X, Min_Y, Max_X, Max_Y : Long_Float;
      end record;

   for Bounding_Rectangle'Size use 64 * 4;

   type Shape_Point is
      record
         X, Y : Long_Float;
      end record;

   for Shape_Point'Size use 128;

   package Shape_Point_Vectors is
     new Ada.Containers.Vectors (Positive, Shape_Point);

   type Shape (Class : Shape_Class) is
      record
         case Class is
            when Null_Shape =>
               null;
            when Point_Shape =>
               X, Y : Long_Float;
            when Polygon_Shape | Polyline_Shape =>
               Polygon_Bound : Bounding_Rectangle;
               Polygon_Points : Shape_Point_Vectors.Vector;
         end case;
      end record;

end WL.Shape_Files;
