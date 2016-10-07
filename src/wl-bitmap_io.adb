with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body WL.Bitmap_IO is

   use WL.Binary_IO;

   procedure Free is
     new Ada.Unchecked_Deallocation (Bitmap_Data,
                                     Bitmap_Data_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Bitmap_Colour_Index_Data,
                                     Bitmap_Colour_Index_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Colourmap_Type,
                                     Colourmap_Access);

   Reversed : Boolean := False;

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

   -----------------------
   -- Adjust_Brightness --
   -----------------------

   function Adjust_Brightness
     (Colour     : Colour_Type;
      Factor     : Float)
      return Colour_Type
   is
      R : constant Float :=
            Float'Min
              (Float (Colour.R) * Factor, 255.0);
      G : constant Float :=
            Float'Min
              (Float (Colour.G) * Factor, 255.0);
      B : constant Float :=
            Float'Min
              (Float (Colour.B) * Factor, 255.0);
   begin
      return (R => Colour_Element (R),
              G => Colour_Element (G),
              B => Colour_Element (B),
              Alpha => 255);
   end Adjust_Brightness;

   -----------
   -- Close --
   -----------

   procedure Close (Bitmap : in out Bitmap_Type) is
   begin
      if Bitmap.Data /= null then
         Free (Bitmap.Data);
      end if;
      if Bitmap.Indices /= null then
         Free (Bitmap.Indices);
      end if;
      if Bitmap.Colourmap /= null then
         Free (Bitmap.Colourmap);
      end if;
   end Close;

   ------------
   -- Colour --
   ------------

   function Colour (Item : Bitmap_Type;
                    X, Y : Natural)
                   return Colour_Type
   is
   begin
      if Has_Colourmap (Item) then
         return Colourmap_Colour (Item, Colour_Index (Item, X, Y));
      else
         return Item.Data (X, Y);
      end if;
   end Colour;

   ------------------
   -- Colour_Index --
   ------------------

   function Colour_Index (Item : Bitmap_Type;
                          X, Y : Natural)
                         return Colour_Element
   is
   begin
      return Item.Indices (X, Y);
   end Colour_Index;

   ----------------------
   -- Colourmap_Colour --
   ----------------------

   function Colourmap_Colour (Item  : Bitmap_Type;
                              Index : Colour_Element)
                             return Colour_Type
   is
   begin
      return Item.Colourmap (Index);
   end Colourmap_Colour;

   -----------
   -- Depth --
   -----------

   function Depth (Item : Bitmap_Type) return Natural is
   begin
      return Item.Depth;
   end Depth;

   -------------------
   -- Has_Colourmap --
   -------------------

   function Has_Colourmap (Item : Bitmap_Type) return Boolean is
   begin
      return Item.Colourmap /= null;
   end Has_Colourmap;

   ------------
   -- Height --
   ------------

   function Height (Item : Bitmap_Type) return Natural is
   begin
      return Item.Height;
   end Height;

   --------------------------
   -- Linear_Interpolation --
   --------------------------

   function Linear_Interpolation
     (Start_Colour  : Colour_Type;
      Finish_Colour : Colour_Type;
      Start_Value   : Integer;
      Finish_Value  : Integer;
      Value         : Integer)
      return Colour_Type
   is
      function Interpolate (Start, Finish : Colour_Element)
                            return Colour_Element;

      -----------------
      -- Interpolate --
      -----------------

      function Interpolate (Start, Finish : Colour_Element)
                            return Colour_Element
      is
      begin
         return Colour_Element ((Integer (Finish) - Integer (Start)) *
                                (Value - Start_Value)
                                / (Finish_Value - Start_Value)
                               + Integer (Start));
      end Interpolate;

   begin
      if Start_Value = Finish_Value then
         return Start_Colour;
      else
         return (Interpolate (Start_Colour.B, Finish_Colour.B),
                 Interpolate (Start_Colour.G, Finish_Colour.G),
                 Interpolate (Start_Colour.R, Finish_Colour.R),
                 Interpolate (Start_Colour.Alpha, Finish_Colour.Alpha));
      end if;
   end Linear_Interpolation;

   ----------------
   -- New_Bitmap --
   ----------------

   function New_Bitmap (Width, Height : Natural) return Bitmap_Type is
      Result : constant Bitmap_Type :=
        (Width        => Width,
         Height       => Height,
         Depth        => 32,
         Data         => new Bitmap_Data (0 .. Width - 1, 0 .. Height - 1),
         Indices      => null,
         Colourmap    => null);
   begin
      Result.Data.all := (others => (others => (0, 0, 0, 1)));
      return Result;
   end New_Bitmap;

   ----------
   -- Read --
   ----------

   procedure Read (Bitmap    : out Bitmap_Type;
                   File_Name : in  String)
   is
      File           : File_Type;
   begin
      Open (File, In_File, File_Name);
      Read (Bitmap, File);
      Close (File);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (Bitmap  : out Bitmap_Type;
                   File    : in out WL.Binary_IO.File_Type)
   is
      Magic          : Bitmap_Magic;
      Header         : Bitmap_Header;
      Info_Header    : Bitmap_Information_Header;
      Row_Size       : Word_32;
      BPP            : Word_32;
      Used           : array (Colour_Element) of Natural;
   begin
      Copy (File, 0, 2, Magic'Address);
      if Magic /= "BM" then
         Close (File);
         raise Constraint_Error with
           "bad magic number: " & String (Magic);
      end if;

      Copy (File, 2, Header'Size / 8, Header'Address);
      Copy (File, 14, Info_Header'Size / 8, Info_Header'Address);

      Ada.Text_IO.Put_Line ("Width       : " &
                              Hex_Image (Info_Header.Width));
      Ada.Text_IO.Put_Line ("Height      : " &
                              Hex_Image (Info_Header.Height));
      Ada.Text_IO.Put_Line ("BPP         : " &
                              Hex_Image (Info_Header.Bits_Per_Pixel));
      Ada.Text_IO.Put_Line ("Compression :" &
                              Info_Header.Compression'Img);

      BPP := Word_32 (Info_Header.Bits_Per_Pixel);
      Bitmap.Depth := Natural (BPP);

      Bitmap.Width := Natural (Info_Header.Width);
      Bitmap.Height := Natural (Info_Header.Height);

      if BPP <= 8 then
         Bitmap.Indices :=
           new Bitmap_Colour_Index_Data (0 .. Bitmap.Width - 1,
                                         0 .. Bitmap.Height - 1);
         Bitmap.Colourmap := new Colourmap_Type;
         Copy (File, 14 + Info_Header.Header_Size,
               2**Natural (BPP) * 4,
               Bitmap.Colourmap.all'Address);
         Used := (others => 0);
      else

         Bitmap.Data := new Bitmap_Data (0 .. Bitmap.Width - 1,
                                         0 .. Bitmap.Height - 1);
      end if;

      Row_Size := Info_Header.Width * BPP / 8;
      if Row_Size mod 4 /= 0 then
         Row_Size := Row_Size + 4 - Row_Size mod 4;
      end if;

      for Y in 0 .. Bitmap.Height - 1 loop
         for X in 0 .. Bitmap.Width - 1 loop
            declare
               Row_Offset   : constant Word_32 :=
                                Header.Data_Start
                                  + Word_32 (Y) * Row_Size;
               Col_Offset   : constant Word_32 :=
                                Word_32 (X) * BPP / 8;
               Bit_Offset   : constant Word_32 :=
                                Word_32 (X) * BPP mod 8;
               Colour       : Colour_Type := (0, 0, 0, 0);
            begin
               if BPP >= 24 then
                  Copy (File, Row_Offset + Col_Offset, BPP / 8,
                        Colour'Address);
                  if Reversed then
                     Bitmap.Data (X, Bitmap.Height - Y - 1) := Colour;
                  else
                     Bitmap.Data (X, Y) := Colour;
                  end if;
               else
                  declare
                     W8    : Word_8;
                     Index : Colour_Element;
                  begin
                     Read (File, W8, Row_Offset + Col_Offset);
                     if BPP < 8 then
                        W8 := W8 / (2 ** Natural (Bit_Offset))
                        mod (2 ** Natural (BPP));
                     end if;
                     Index := Colour_Element (W8);
                     Used (Index) := Used (Index) + 1;
                     if Reversed then
                        Bitmap.Indices (X, Bitmap.Height - Y - 1) := Index;
                     else
                        Bitmap.Indices (X, Y) := Index;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;

      if BPP <= 8 then
         Ada.Text_IO.Put_Line ("Colour map");
         for I in Used'Range loop
            if Used (I) > 0 then
               Ada.Text_IO.Put_Line
                 (Hex_Image (Word_8 (I)) &
                    "   " &
                    Hex_Image (Word_8 (Bitmap.Colourmap (I).R)) & " " &
                    Hex_Image (Word_8 (Bitmap.Colourmap (I).G)) & " " &
                    Hex_Image (Word_8 (Bitmap.Colourmap (I).B)) & " " &
                    Colour_Element'Image (Bitmap.Colourmap (I).R) &
                    Colour_Element'Image (Bitmap.Colourmap (I).G) &
                    Colour_Element'Image (Bitmap.Colourmap (I).B) &
                    Natural'Image (Used (I)));
            end if;
         end loop;
      end if;

   end Read;

   ----------------
   -- Set_Colour --
   ----------------

   procedure Set_Colour (Item   : Bitmap_Type;
                         X, Y   : Natural;
                         Colour : Colour_Type)
   is
   begin
      Item.Data (X, Y) := Colour;
   end Set_Colour;

   -----------------------
   -- Set_Vertical_Flip --
   -----------------------

   procedure Set_Vertical_Flip (Value : Boolean := True) is
   begin
      Reversed := Value;
   end Set_Vertical_Flip;

   -----------
   -- Width --
   -----------

   function Width (Item : Bitmap_Type) return Natural is
   begin
      return Item.Width;
   end Width;

   -----------
   -- Write --
   -----------

   procedure Write (Bitmap    : in Bitmap_Type;
                    File_Name : in String)
   is
      File           : Binary_IO.File_Type;
      Magic          : constant Bitmap_Magic := "BM";
      Header         : Bitmap_Header;
      Info_Header    : Bitmap_Information_Header;
   begin
      Binary_IO.Create (File, Binary_IO.Out_File, File_Name);
      Binary_IO.Write (File, 2, Magic'Address);
      Header.File_Size := 16#36# +
        32 * Word_32 (Bitmap.Width) * Word_32 (Bitmap.Height);
      Header.Creator_1 := 16#424C#;
      Header.Creator_2 := 16#414F#;
      Header.Data_Start := 16#00000036#;
      Binary_IO.Write (File, Header'Size / 8, Header'Address);

      Info_Header.Header_Size := 16#0000_0028#;
      Info_Header.Width       := Word_32 (Bitmap.Width);
      Info_Header.Height      := Word_32 (Bitmap.Height);
      Info_Header.Num_Planes  := 1;
      Info_Header.Bits_Per_Pixel := 32;
      Info_Header.Compression    := 0;
      Info_Header.Image_Size     :=
        32 * Word_32 (Bitmap.Width) * Word_32 (Bitmap.Height);
      Info_Header.Horizontal_Res := 2835;
      Info_Header.Vertical_Res   := 2835;
      Info_Header.Colourmap_Size := 0;
      Info_Header.Important_Colours := 0;

      Write (File, Info_Header'Size / 8, Info_Header'Address);

      for Y in 0 .. Bitmap.Height - 1 loop
         for X in 0 .. Bitmap.Width - 1 loop
            declare
               Colour : constant Colour_Type :=
                          (if Bitmap.Depth >= 24
                           then Bitmap.Data (X, Y)
                           else Bitmap.Colourmap (Bitmap.Indices (X, Y)));
            begin
               Write (File, Word_8 (Colour.B));
               Write (File, Word_8 (Colour.G));
               Write (File, Word_8 (Colour.R));
               Write (File, Word_8 (Colour.Alpha));
            end;
         end loop;
      end loop;

      Close (File);

   end Write;

end WL.Bitmap_IO;
