with Binary_IO;                        use Binary_IO;

package body WL.Images.Bitmaps is

   type Bitmap_Magic is new String (1 .. 2);
   for Bitmap_Magic'Size use 16;

   type Bitmap_Header is
      record
         File_Size   : Word_32;
         Creator_1   : Word_16;
         Creator_2   : Word_16;
         Data_Start  : Word_32;
      end record;

   for Bitmap_Header'Size use 96;

   type Bitmap_Information_Header is
      record
         Header_Size       : Word_32;
         Width             : Word_32;
         Height            : Word_32;
         Num_Planes        : Word_16;
         Bits_Per_Pixel    : Word_16;
         Compression       : Word_32;
         Image_Size        : Word_32;
         Horizontal_Res    : Word_32;
         Vertical_Res      : Word_32;
         Colourmap_Size    : Word_32;
         Important_Colours : Word_32;
      end record;

   for Bitmap_Information_Header'Size use 40 * 8;

   ------------------
   -- Write_Bitmap --
   ------------------

   procedure Write_Bitmap (Image : Image_Type'Class;
                           Path  : String;
                           Level : Image_Level_Index :=
                             Default_Image_Level)
   is
      File           : Binary_IO.File_Type;
      Magic          : constant Bitmap_Magic := "BM";
      Header         : Bitmap_Header;
      Info_Header    : Bitmap_Information_Header;
   begin
      Binary_IO.Create (File, Binary_IO.Out_File, Path);
      Binary_IO.Write (File, 2, Magic'Address);
      Header.File_Size := 16#36# +
        32 * Word_32 (Image.Width (Level)) * Word_32 (Image.Height (Level));
      Header.Creator_1 := 16#424C#;
      Header.Creator_2 := 16#414F#;
      Header.Data_Start := 16#00000036#;
      Binary_IO.Write (File, Header'Size / 8, Header'Address);

      Info_Header.Header_Size := 16#0000_0028#;
      Info_Header.Width       := Word_32 (Image.Width (Level));
      Info_Header.Height      := Word_32 (Image.Height (Level));
      Info_Header.Num_Planes  := 1;
      Info_Header.Bits_Per_Pixel := 32;
      Info_Header.Compression    := 0;
      Info_Header.Image_Size     :=
        32 * Word_32 (Image.Width (Level)) * Word_32 (Image.Height (Level));
      Info_Header.Horizontal_Res := 2835;
      Info_Header.Vertical_Res   := 2835;
      Info_Header.Colourmap_Size := 0;
      Info_Header.Important_Colours := 0;

      Write (File, Info_Header'Size / 8, Info_Header'Address);

      for Y in 0 .. Image.Height (Level) - 1 loop
         for X in 0 .. Image.Width (Level) - 1 loop
            declare
               Colour : constant Colour_Type :=
                          WL.Images.Colour (Image, Level, X, Y);
            begin
               Write (File, Word_8 (Colour.Blue));
               Write (File, Word_8 (Colour.Green));
               Write (File, Word_8 (Colour.Red));
               Write (File, Word_8 (Colour.Alpha));
            end;
         end loop;
      end loop;

      Close (File);
   end Write_Bitmap;

end WL.Images.Bitmaps;
