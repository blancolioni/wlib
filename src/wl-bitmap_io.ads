package WL.Bitmap_IO is

   type Bitmap_Type is private;

   type Colour_Element is mod 256;

   type Colour_Type is
      record
         B, G, R : Colour_Element;
         Alpha   : Colour_Element;
      end record;

   for Colour_Type'Size use 32;

   function Linear_Interpolation
     (Start_Colour  : Colour_Type;
      Finish_Colour : Colour_Type;
      Start_Value   : Integer;
      Finish_Value  : Integer;
      Value         : Integer)
      return Colour_Type;

   procedure Read (Bitmap    : out Bitmap_Type;
                   File_Name : in  String);

   procedure Write (Bitmap    : in Bitmap_Type;
                    File_Name : in String);

   procedure Close (Bitmap : in out Bitmap_Type);

   function New_Bitmap (Width, Height : Natural) return Bitmap_Type;

   function Width (Item : Bitmap_Type) return Natural;
   function Height (Item : Bitmap_Type) return Natural;
   function Depth (Item : Bitmap_Type) return Natural;
   function Colour (Item : Bitmap_Type;
                    X, Y : Natural)
                   return Colour_Type;

   procedure Set_Colour (Item   : Bitmap_Type;
                         X, Y   : Natural;
                         Colour : Colour_Type);

   function Has_Colourmap (Item : Bitmap_Type) return Boolean;
   function Colour_Index (Item : Bitmap_Type;
                          X, Y : Natural)
                         return Colour_Element;
   function Colourmap_Colour (Item  : Bitmap_Type;
                              Index : Colour_Element)
                             return Colour_Type;

   procedure Set_Vertical_Flip (Value : Boolean := True);

private

   type Bitmap_Data is
     array (Natural range <>, Natural range <>) of Colour_Type;

   pragma Pack (Bitmap_Data);

   type Bitmap_Colour_Index_Data is
     array (Natural range <>, Natural range <>) of Colour_Element;

   type Bitmap_Data_Access is access Bitmap_Data;
   type Bitmap_Colour_Index_Access is access Bitmap_Colour_Index_Data;

   type Colourmap_Type is array (Colour_Element) of Colour_Type;

   type Colourmap_Access is access Colourmap_Type;

   type Bitmap_Type is
      record
         Width, Height : Natural;
         Depth         : Natural;
         Data          : Bitmap_Data_Access;
         Indices       : Bitmap_Colour_Index_Access;
         Colourmap     : Colourmap_Access;
      end record;

end WL.Bitmap_IO;
