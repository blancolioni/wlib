package WL.Images.Bitmaps is

   type Bitmap_Image_Reader is
     new Image_Reader with private;

   overriding procedure Read
     (Reader : Bitmap_Image_Reader;
      File   : WL.Binary_IO.File_Type;
      Image  : out Image_Type'Class);

private

   type Bitmap_Image_Reader is
     new Image_Reader with
      record
         Flip_Vertical : Boolean := True;
      end record;

end WL.Images.Bitmaps;
