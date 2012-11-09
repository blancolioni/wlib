package WL.Images is

   type Colour_Element is mod 256;

   type Colour_Type is
      record
         Red   : Colour_Element;
         Green : Colour_Element;
         Blue  : Colour_Element;
         Alpha : Colour_Element;
      end record;

   for Colour_Type'Size use 32;

   type Image_Level_Count is range 0 .. 16;
   subtype Image_Level_Index is
     Image_Level_Count range 1 .. Image_Level_Count'Last;
   Default_Image_Level : constant Image_Level_Index := 1;

   type Image_Type is tagged private;

   function Colour (Image : Image_Type;
                    X, Y  : Natural)
                    return Colour_Type;

   function Width (Image : Image_Type) return Natural;
   function Height (Image : Image_Type) return Natural;

   function Level_Count (Image : Image_Type) return Image_Level_Count;

   function Colour (Image : Image_Type;
                    Level : Image_Level_Index;
                    X, Y  : Natural)
                    return Colour_Type;

   function Width (Image : Image_Type;
                   Level : Image_Level_Index)
                   return Natural;

   function Height (Image : Image_Type;
                    Level : Image_Level_Index)
                    return Natural;

private

   type Image_Data is
     array (Natural range <>, Natural range <>) of Colour_Type;
   type Image_Data_Access is access Image_Data;

   type Image_Level_Info is
      record
         Width, Height : Natural;
         Data          : Image_Data_Access;
      end record;

   type Image_Level_Array is
     array (Image_Level_Index) of Image_Level_Info;

   type Image_Type is tagged
      record
         Num_Levels : Image_Level_Count;
         Levels     : Image_Level_Array;
      end record;

end WL.Images;
