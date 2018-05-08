with Ada.Text_IO;

package body WL.Images.FLC is

   FLI_Magic_Number : constant := 16#AF11#;
   FLC_Magic_Number : constant := 16#AF12#;

   Frame_Magic_Number : constant := 16#F1FA#;

   Color_256_Chunk : constant := 4;
   Delta_Chunk     : constant := 7;
   Color_64_Chunk  : constant := 11;
   LC_Chunk        : constant := 12;
   Black_Chunk     : constant := 13;
   Brun_Chunk      : constant := 15;
   Copy_Chunk      : constant := 16;

   ------------------
   -- Ignore_Magic --
   ------------------

   procedure Ignore_Magic
     (Reader : in out FLC_Image_Reader'Class)
   is
   begin
      Reader.Ignore_Magic := True;
   end Ignore_Magic;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Reader : FLC_Image_Reader;
      File   : in out WL.Binary_IO.File_Type;
      Image  : out Image_Type'Class)
   is
      use WL.Binary_IO;
      Size                  : Word_32;
      Magic                 : Word_16;
      Frames, Width, Height : Word_16;
      Color_Depth          : Word_16;
      Flags                 : Word_16;
      Speed                 : Word_32;
      Offset_Frame_1        : Word_32;
      Offset_Frame_2        : Word_32;

      Color_Map             : array (Word_8) of Image_Color;

      procedure Read_Frame
        (Frame          : Layer_Index);

      ----------------
      -- Read_Frame --
      ----------------

      procedure Read_Frame
        (Frame          : Layer_Index)
      is
         Start       : Word_32;
         Frame_Size  : Word_32;
         Frame_Magic : Word_16;
         Chunks      : Word_16;

         procedure Read_Chunk (Index : Positive);

         procedure Read_Brun_Chunk;
         procedure Read_Colormap (Old : Boolean);

         ---------------------
         -- Read_Brun_Chunk --
         ---------------------

         procedure Read_Brun_Chunk is
         begin
            for Y in reverse 1 .. Image.Height (Frame) loop
               declare
                  X : Pixel_X_Range := 1;
                  Packet_Count : Word_8;
               begin
                  Read (File, Packet_Count);

                  while X <= Image.Width (Frame) loop
                     declare
                        Count : Word_8;
                        Color : Word_8;
                     begin
                        Read (File, Count);
                        if Count < 128 then
                           Read (File, Color);
                           for I in 1 .. Count loop
                              exit when X > Image.Width (Frame);
                              Image.Set_Color
                                (Frame, X, Y, Color_Map (Color));
                              X := X + 1;
                           end loop;
                        else
                           while Count /= 0 loop
                              Read (File, Color);
                              Image.Set_Color
                                (Frame, X, Y, Color_Map (Color));
                              X := X + 1;
                              Count := Count + 1;
                           end loop;
                        end if;
                     end;
                  end loop;
               end;
            end loop;
         end Read_Brun_Chunk;

         ----------------
         -- Read_Chunk --
         ----------------

         procedure Read_Chunk (Index : Positive) is
            Start : constant Word_32 := Current_Offset (File);
            Size  : Word_32;
            Class : Word_16;
         begin
            Read (File, Size);
            Read (File, Class);

            case Class is
               when Color_256_Chunk =>
                  Read_Colormap (False);
               when Brun_Chunk =>
                  Read_Brun_Chunk;
               when others =>
                  Ada.Text_IO.Put_Line
                    ("warning: ignoring chunk with id"
                       & Class'Img);
            end case;

            Set_Offset (File, Start + Size);
         end Read_Chunk;

         -------------------
         -- Read_Colormap --
         -------------------

         procedure Read_Colormap (Old : Boolean) is
            Packet_Count : Word_16;
            Start        : Word_8   := 0;
         begin
            Read (File, Packet_Count);
            for I in 1 .. Packet_Count loop
               declare
                  Skip    : Word_8;
                  Count   : Word_8;
                  Low     : Word_8;
                  High    : Word_8;
                  R, G, B : Word_8;
               begin
                  Read (File, Skip);
                  Start := Start + Skip;
                  Read (File, Count);

                  Low := Start;
                  if Count = 0 then
                     High := Low - 1;
                  else
                     High := Low + Count - 1;
                  end if;

                  Ada.Text_IO.Put_Line
                    ("colormap packet:" & Low'Img & " .." & High'Img);

                  for Index in Low .. High loop
                     Read (File, R);
                     Read (File, G);
                     Read (File, B);

                     if Old then
                        R := 255 * R / 63;
                        G := 255 * G / 63;
                        B := 255 * B / 63;
                     end if;

                     Color_Map (Index) :=
                       (Red   => Color_Element (R),
                        Green => Color_Element (G),
                        Blue  => Color_Element (B),
                        Alpha => 255);
                  end loop;
               end;
            end loop;
         end Read_Colormap;

      begin
         if Frame = 1 and then Offset_Frame_1 /= 0 then
            Set_Offset (File, Offset_Frame_1);
         elsif Frame = 2 and then Offset_Frame_2 /= 0 then
            Set_Offset (File, Offset_Frame_2);
         end if;

         Start := Current_Offset (File);

         Read (File, Frame_Size);
         Ada.Text_IO.Put_Line
           ("frame" & Frame'Img & ": size =" & Frame_Size'Img);

         Read (File, Frame_Magic);

         if Frame_Magic /= Frame_Magic_Number then
            raise Constraint_Error with
              "invalid data for frame" & Frame'Img;
         end if;

         Read (File, Chunks);

         declare
            Padding : Word_32;
         begin
            Read (File, Padding);
            Read (File, Padding);
         end;

         for I in 1 .. Natural (Chunks) loop
            Read_Chunk (I);
         end loop;

         if Frame < Layer_Index (Frames) then
            Set_Offset (File, Start + Frame_Size);
         end if;

      end Read_Frame;

   begin

      Read (File, Size);
      Read (File, Magic);

      if not Reader.Ignore_Magic
        and then Magic /= FLI_Magic_Number
        and then Magic /= FLC_Magic_Number
      then
         raise Constraint_Error with
           "invalid magic number in FLC";
      end if;

      Read (File, Frames);
      Read (File, Width);
      Read (File, Height);

      if Width = 0 then
         Width := 320;
      end if;

      if Height = 0 then
         Height := 200;
      end if;

      Image.Create
        (Width  => Pixel_X_Count (Width),
         Height => Pixel_Y_Count (Height),
         Layers => Layer_Count (Frames));

      Ada.Text_IO.Put_Line ("FLC file:" & Frames'Img
                            & " frame"
                            & (if Frames = 1 then "" else "s")
                            & Width'Img & " x" & Height'Img);

      Read (File, Color_Depth);
      Read (File, Flags);

      Read (File, Speed);
      if Magic = FLI_Magic_Number then
         Speed := (if Speed = 0 then 70 else 1000 * Speed / 70);
      end if;

      if Magic = FLC_Magic_Number then
         Set_Offset (File, 80);
         Read (File, Offset_Frame_1);
         Read (File, Offset_Frame_2);
      end if;

      Set_Offset (File, 128);

      for I in 1 .. Frames loop
         Read_Frame (Layer_Index (I));
      end loop;

      Close (File);

   end Read;

end WL.Images.FLC;
