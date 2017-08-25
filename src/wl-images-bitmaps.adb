package body WL.Images.Bitmaps is

   use WL.Binary_IO;

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

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Reader : Bitmap_Image_Reader;
      File   : in out WL.Binary_IO.File_Type;
      Image  : out Image_Type'Class)
   is
      Magic          : Bitmap_Magic;
      Header         : Bitmap_Header;
      Info_Header    : Bitmap_Information_Header;
      Row_Size       : Word_32;
      BPP            : Word_32;
      Used           : array (Word_8) of Natural;
      Color_Map      : array (Word_8) of Image_Color;

      procedure Fix_Color (Color : in out Image_Color);

      ---------------
      -- Fix_Color --
      ---------------

      procedure Fix_Color (Color : in out Image_Color) is
         R : constant Color_Element := Color.Blue;
         G : constant Color_Element := Color.Green;
         B : constant Color_Element := Color.Red;
         A : constant Color_Element := Color.Alpha;
      begin
         Color := (R, G, B, A);
      end Fix_Color;

   begin
      Copy (File, 0, 2, Magic'Address);
      if Magic /= "BM" then
         raise Constraint_Error with
           "bad magic number: " & String (Magic);
      end if;

      Copy (File, 2, Header'Size / 8, Header'Address);
      Copy (File, 14, Info_Header'Size / 8, Info_Header'Address);

      BPP := Word_32 (Info_Header.Bits_Per_Pixel);

      Image.Create (Pixel_X_Count (Info_Header.Width),
                    Pixel_Y_Count (Info_Header.Height));

      if BPP <= 8 then
         Copy (File, 14 + Info_Header.Header_Size,
               2 ** Natural (BPP) * 4,
               Color_Map'Address);
         Used := (others => 0);
         for I in Word_8 range 0 .. 2 ** Natural (BPP) - 1 loop
            Fix_Color (Color_Map (I));
         end loop;

      end if;

      Row_Size := Info_Header.Width * BPP / 8;
      if Row_Size mod 4 /= 0 then
         Row_Size := Row_Size + 4 - Row_Size mod 4;
      end if;

      for Y in 0 .. Info_Header.Height - 1 loop
         for X in 0 .. Info_Header.Width - 1 loop
            declare
               Row_Offset   : constant Word_32 :=
                                Header.Data_Start
                                  + Word_32 (Y) * Row_Size;
               Col_Offset   : constant Word_32 :=
                                Word_32 (X) * BPP / 8;
               Bit_Offset   : constant Natural :=
                                Natural (X * BPP mod 8);
               Colour       : Image_Color := (0, 0, 0, 0);
               Image_X      : constant Pixel_X_Range :=
                                Pixel_X_Range (X + 1);
               Image_Y      : constant Pixel_Y_Range :=
                                (if Reader.Flip_Vertical
                                 then Pixel_Y_Range (Info_Header.Height - Y)
                                 else Pixel_Y_Range (Y + 1));
            begin
               if BPP >= 24 then
                  Copy (File, Row_Offset + Col_Offset, BPP / 8,
                        Colour'Address);
                  Fix_Color (Colour);
                  Image.Set_Color (Image_X, Image_Y, Colour);
               else
                  declare
                     Index : Word_8;
                  begin
                     Read (File, Index, Row_Offset + Col_Offset);
                     if BPP < 8 then
                        Index :=
                          Index / (2 ** Bit_Offset) mod (2 ** Natural (BPP));
                     end if;

                     Used (Index) := Used (Index) + 1;
                     Image.Set_Color (Image_X, Image_Y,
                                      Color_Map (Index));
                  end;
               end if;
            end;
         end loop;
      end loop;

   end Read;

end WL.Images.Bitmaps;
