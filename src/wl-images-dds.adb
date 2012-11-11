with Ada.Directories;
with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

package body WL.Images.DDS is

   type Word_32_Array is array (Word_32 range <>) of Word_32;
   type Word_8_Array is array (Word_32 range <>) of Word_8;

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

   type Internal_Format_Type is (RGBA_8, DXT_1, DXT_3, DXT_5);

   procedure Copy_DXT_Block
     (Format : Internal_Format_Type;
      Block  : Word_8_Array;
      Dest   : Image_Data_Access;
      X, Y   : Natural);

   function From_Colour_16
     (Colour : Word_16;
      Alpha  : Boolean)
      return Colour_Type;

   function Interpolate (Colour_1, Colour_2 : Colour_Type;
                         Ratio              : Float)
                         return Colour_Type;

   --------------------
   -- Copy_DXT_Block --
   --------------------

   procedure Copy_DXT_Block
     (Format : Internal_Format_Type;
      Block  : Word_8_Array;
      Dest   : Image_Data_Access;
      X, Y   : Natural)
   is
      Colour      : array (0 .. 3) of Colour_Type;
   begin
      case Format is
         when DXT_1 =>
            Colour (0) :=
              From_Colour_16 (Word_16 (Block (Block'First))
                              + 256 * Word_16 (Block (Block'First + 1)),
                              False);
            Colour (1) :=
              From_Colour_16 (Word_16 (Block (Block'First + 2))
                              + 256 * Word_16 (Block (Block'First + 3)),
                              False);
            Colour (2) :=
              Interpolate (Colour (0), Colour (1), 2.0 / 3.0);
            Colour (3) :=
              Interpolate (Colour (0), Colour (1), 1.0 / 3.0);

            for I in 0 .. 3 loop
               for J in 0 .. 3 loop
                  declare
                     Index : constant Natural :=
                               Natural
                                 ((Block (Word_32 (I) + Block'First + 4)
                                  / (2 ** (J * 2))) mod 4);
                     C     : constant Colour_Type := Colour (Index);
                  begin
                     Dest (X + J, Y + I) := C;
                  end;
               end loop;
            end loop;
         when DXT_3 =>
            Copy_DXT_Block (DXT_1, Block (Block'First + 8 .. Block'Last),
                            Dest, X, Y);
            for I in Word_32 range 0 .. 7 loop
               declare
                  Alpha_Byte : constant Word_8 :=
                                 Block (Block'First + I);
                  Alpha_1    : constant Colour_Element :=
                                 Colour_Element (Alpha_Byte / 16);
                  Alpha_2    : constant Colour_Element :=
                                 Colour_Element (Alpha_Byte mod 16);
                  DY : constant Natural := Natural (I / 2);
                  DX : constant Natural := Natural (I mod 2);
               begin
                  Dest (X + 2 * DX, Y + DY).Alpha := Alpha_1 * 16;
                  Dest (X + 2 * DX + 1, Y + DY).Alpha := Alpha_2 * 16;
               end;
            end loop;

         when others =>
            raise Constraint_Error with
            Format'Img & " not supported";
      end case;

   end Copy_DXT_Block;

   --------------------
   -- From_Colour_16 --
   --------------------

   function From_Colour_16
     (Colour : Word_16;
      Alpha  : Boolean)
      return Colour_Type
   is
      R, G, B, A : Word_16;
   begin
      B := 8 * (Colour mod 32);
      if Alpha then
         G := 8 * ((Colour / 32) mod 32);
         R := 8 * ((Colour / 32 / 32) mod 32);
         A := (if Colour >= 32768 then 255 else 0);
      else
         G := 4 * ((Colour / 32) mod 64);
         R := 8 * ((Colour / 32 / 64) mod 32);
         A := 255;
      end if;
      return (Red   => Colour_Element (R),
              Green => Colour_Element (G),
              Blue  => Colour_Element (B),
              Alpha => Colour_Element (A));
   end From_Colour_16;

   -----------------
   -- Interpolate --
   -----------------

   function Interpolate (Colour_1, Colour_2 : Colour_Type;
                         Ratio              : Float)
                         return Colour_Type
   is
      function Inter (E1, E2 : Colour_Element) return Colour_Element;

      -----------
      -- Inter --
      -----------

      function Inter (E1, E2 : Colour_Element) return Colour_Element is
      begin
         return Colour_Element (Float (E1) * Ratio +
                                  Float (E2) * (1.0 - Ratio));
      end Inter;

   begin
      return (Red => Inter (Colour_1.Red, Colour_2.Red),
              Green => Inter (Colour_1.Green, Colour_2.Green),
              Blue  => Inter (Colour_1.Blue, Colour_2.Blue),
              Alpha => Inter (Colour_1.Alpha, Colour_2.Alpha));
   end Interpolate;

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
         Num_Bytes       : Word_32 := Header.Width * Header.Height;
         Block_Size      : Word_32 := 16;
         W, H            : Word_32;
         Internal_Format : Internal_Format_Type;
         RGB_Bits        : Word_32;
         Level           : Image_Level_Index := 1;
      begin
         Ada.Text_IO.Put (Ada.Directories.Simple_Name (Path) & ": ");
         if (Header.Pixel_Format.Flags and Pixel_Flag_Four_CC) /= 0 then
            if Header.Pixel_Format.Four_CC = ID_DXT1 then
               Block_Size := 8;
               Internal_Format := DXT_1;
               Ada.Text_IO.Put_Line ("DXT1");
            elsif Header.Pixel_Format.Four_CC = ID_DXT3 then
               Internal_Format := DXT_3;
               Ada.Text_IO.Put_Line ("DXT3");
            elsif Header.Pixel_Format.Four_CC = ID_DXT5 then
               Ada.Text_IO.Put_Line ("DXT5");
               Internal_Format := DXT_5;
            else
               raise Constraint_Error with
                 "unknown compression format";
            end if;
         elsif (Header.Pixel_Format.Flags and Pixel_Flag_RGB) /= 0 then
            Internal_Format := RGBA_8;
            RGB_Bits := Header.Pixel_Format.RGB_Bit_Count;
            Num_Bytes := Num_Bytes * RGB_Bits / 8;
            Ada.Text_IO.Put_Line ("RGBA");
         else
            raise Constraint_Error with
              "unknown DDS format";
         end if;

         W := Header.Width;
         H := Header.Height;

         Image.Num_Levels :=
           Image_Level_Count
             (Word_32'Max (Header.Mip_Map_Count, 1));

         Ada.Text_IO.Put_Line ("Mipmap levels:" & Header.Mip_Map_Count'Img);

         for I in 1 .. Image.Num_Levels loop
            case Internal_Format is
               when RGBA_8 =>
                  Num_Bytes := W * H * RGB_Bits / 8;
               when others =>
                  Num_Bytes := ((W + 3) / 4) * ((H + 3) / 4) * Block_Size;
            end case;

            Image.Levels (Level).Width  := Natural (W);
            Image.Levels (Level).Height := Natural (H);

            Image.Levels (Level).Data :=
              new Image_Data
                (0 .. Natural (W) - 1, 0 .. Natural (H) - 1);

            case Internal_Format is
               when RGBA_8 =>
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
                        end;
                     end loop;
                  end loop;
               when others =>
                  declare
                     Temp         : Word_8_Array (0 .. Num_Bytes - 1);
                     X            : Natural := 0;
                     Y            : Natural := 0;
                  begin
                     Read (File, Temp'Size, Temp'Address);
                     for I in 0 .. Num_Bytes / Block_Size - 1 loop
                        declare
                           Offset : constant Word_32 :=
                                      I * Block_Size;
                        begin
                           Copy_DXT_Block
                             (Internal_Format,
                              Temp (Offset .. Offset + Block_Size - 1),
                              Image.Levels (Level).Data,
                              X, Y);
                           X := X + 4;
                           if X >= Image.Levels (Level).Width then
                              X := 0;
                              Y := Y + 4;
                           end if;
                        end;
                     end loop;
                  end;
            end case;

            W := Word_32'Max (W / 2, 1);
            H := Word_32'Max (H / 2, 1);
            Level := Level + 1;

         end loop;
      end;

      Close (File);

   end Read_DDS;

end WL.Images.DDS;
