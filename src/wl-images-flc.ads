package WL.Images.FLC is

   type FLC_Image_Reader is
     new Image_Reader with private;

   procedure Ignore_Magic
     (Reader : in out FLC_Image_Reader'Class);

   overriding procedure Read
     (Reader : FLC_Image_Reader;
      File   : in out WL.Binary_IO.File_Type;
      Image  : out Image_Type'Class);

private

   type FLC_Image_Reader is
     new Image_Reader with
      record
         Ignore_Magic : Boolean := False;
      end record;

end WL.Images.FLC;
