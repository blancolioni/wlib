with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

package body WL.Images.DDS is

   type Word_32_Array is array (Word_32 range <>) of Word_32;
   --  type Word_8_Array is array (Word_32 range <>) of Word_8;

   type D3D_Pixel_Format is
      record
         Size           : Word_32;
         Flags          : Word_32;
         Four_CC        : Word_32;
         RGB_Bit_Count  : Word_32;
         R_Bit_Mask     : Word_32;
         G_Bit_Mask     : Word_32;
         B_Bit_Mask     : Word_32;
         Alpha_Bit_Mask : Word_32;
      end record;

   type D3D_Caps_2 is
      record
         Caps_1 : Word_32;
         Caps_2 : Word_32;
         Reserved_1 : Word_32;
         Reserved_2 : Word_32;
      end record;

   type D3D_Surface_Desc_2 is
      record
         Size        : Word_32;
         Flags       : Word_32;
         Height      : Word_32;
         Width       : Word_32;
         Pitch_Or_Linear_Size : Word_32;
         Depth                : Word_32;
         Mip_Map_Count        : Word_32;
         Reserved             : Word_32_Array (0 .. 10);
         Pixel_Format         : D3D_Pixel_Format;
         Caps                 : D3D_Caps_2;
         Reserved_2           : Word_32;
      end record;

   Magic_DDS : constant := 16#20534444#;

--     Pixel_Flag_Alpha_Pixels     : constant := 16#0001#;
--     Pixel_Flag_Alpha            : constant := 16#0002#;
   Pixel_Flag_Four_CC          : constant := 16#0004#;
--     Pixel_Flag_Palette_Indexd_8 : constant := 16#0020#;
   Pixel_Flag_RGB              : constant := 16#0040#;
--     Pixel_Flag_Luminance        : constant := 16#2_0000#;

   ID_DXT1                     : constant := 16#31545844#;
   ID_DXT3                     : constant := 16#33545844#;
   ID_DXT5                     : constant := 16#35545844#;

   --------------
   -- Read_DDS --
   --------------

   procedure Read_DDS
     (Image : out Image_Type'Class;
      Path  : in  String)
   is
      File : File_Type;
      Header : D3D_Surface_Desc_2;
   begin
      Open (File, In_File, Path);

      declare
         Magic : Word_32;
      begin
         Read (File, Magic);
         if Magic /= Magic_DDS then
            raise Constraint_Error
              with Path & " is not a DDS file";
         end if;
      end;

      Read (File, Header'Size, Header'Address);

      declare
         Num_Bytes  : Word_32 := Header.Width * Header.Height;
         Block_Size : Word_32 := 16;
         W, H       : Word_32;
         Format_RGBA_8 : Boolean := False;
         RGB_Bits      : Word_32;
         Level : Image_Level_Index := 1;
      begin
         if (Header.Pixel_Format.Flags and Pixel_Flag_Four_CC) /= 0 then
            if Header.Pixel_Format.Four_CC = ID_DXT1 then
               Block_Size := 8;
            elsif Header.Pixel_Format.Four_CC = ID_DXT3 then
               null;
            elsif Header.Pixel_Format.Four_CC = ID_DXT5 then
               null;
            else
               raise Constraint_Error with
                 "unknown compression format";
            end if;
         elsif (Header.Pixel_Format.Flags and Pixel_Flag_RGB) /= 0 then
            Format_RGBA_8 := True;
            RGB_Bits := Header.Pixel_Format.RGB_Bit_Count;
            Num_Bytes := Num_Bytes * RGB_Bits / 8;
         else
            raise Constraint_Error with
              "unknown DDS format";
         end if;

         if not Format_RGBA_8 then
            raise Constraint_Error with
              "unknown format (but we only do rgb(a)";
         end if;

         W := Header.Width;
         H := Header.Height;

         Image.Num_Levels :=
           Image_Level_Count
             (Word_32'Max (Header.Mip_Map_Count, 1));

         Ada.Text_IO.Put_Line ("Mipmap levels:" & Header.Mip_Map_Count'Img);

         for I in 1 .. Image.Num_Levels loop
            if Format_RGBA_8 then
               Num_Bytes := W * H * RGB_Bits / 8;
            else
               Num_Bytes := ((W + 3) / 4) * ((H + 3) / 4) * Block_Size;
            end if;

            Image.Levels (Level).Width  := Natural (W);
            Image.Levels (Level).Height := Natural (H);

            declare
               Index  : Word_32 := 0;
            begin

               if Format_RGBA_8 then
                  Image.Levels (Level).Data :=
                    new Image_Data
                      (0 .. Natural (W) - 1, 0 .. Natural (H) - 1);
                  for Y in 0 .. H - 1 loop
                     for X in 0 .. W - 1 loop
                        declare
                           R, G, B, A : Word_8;
                        begin
                           Read (File, B);
                           Read (File, G);
                           Read (File, R);
                           if RGB_Bits = 32 then
                              Read (File, A);
                           else
                              A := 255;
                           end if;
                           Image.Levels (Level).Data
                             (Natural (X), Natural (Y)) :=
                             (Red    => Colour_Element (R),
                              Green  => Colour_Element (G),
                              Blue   => Colour_Element (B),
                              Alpha  => Colour_Element (A));
                           Index := Index + RGB_Bits / 8;
                        end;
                     end loop;
                  end loop;
               end if;
            end;

            W := Word_32'Max (W / 2, 1);
            H := Word_32'Max (H / 2, 1);
            Level := Level + 1;

         end loop;
      end;

      Close (File);

   end Read_DDS;

end WL.Images.DDS;
