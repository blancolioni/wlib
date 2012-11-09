package body WL.Images is

   ------------
   -- Colour --
   ------------

   function Colour
     (Image : Image_Type;
      Level : Image_Level_Index;
      X, Y  : Natural)
      return Colour_Type
   is
   begin
      return Image.Levels (Level).Data (X, Y);
   end Colour;

   ------------
   -- Colour --
   ------------

   function Colour (Image : Image_Type;
                    X, Y  : Natural)
                    return Colour_Type
   is
   begin
      return Colour (Image, Default_Image_Level, X, Y);
   end Colour;

   ------------
   -- Height --
   ------------

   function Height (Image : Image_Type;
                    Level : Image_Level_Index)
                    return Natural
   is
   begin
      return Image.Levels (Level).Height;
   end Height;

   ------------
   -- Height --
   ------------

   function Height (Image : Image_Type) return Natural is
   begin
      return Height (Image, Default_Image_Level);
   end Height;

   -----------------
   -- Level_Count --
   -----------------

   function Level_Count (Image : Image_Type) return Image_Level_Count is
   begin
      return Image.Num_Levels;
   end Level_Count;

   -----------
   -- Width --
   -----------

   function Width (Image : Image_Type;
                    Level : Image_Level_Index)
                   return Natural
   is
   begin
      return Image.Levels (Level).Width;
   end Width;

   -----------
   -- Width --
   -----------

   function Width (Image : Image_Type) return Natural is
   begin
      return Width (Image, Default_Image_Level);
   end Width;

end WL.Images;
