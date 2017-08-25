package body WL.Images is

   ------------
   -- Create --
   ------------

   procedure Create
     (Image  : in out Image_Type'Class;
      Width  : Pixel_X_Count;
      Height : Pixel_Y_Count;
      Layers : Layer_Count := 1)
   is
      Layer_Data : Image_Data (1 .. Width, 1 .. Height);
      Layer      : Image_Layer_Record := (Width, Height, others => <>);
   begin
      Layer.Data.Replace_Element (Layer_Data);
      for I in 1 .. Layers loop
         Image.Layers.Append (Layer);
      end loop;
   end Create;

   ----------
   -- Read --
   ----------

   procedure Read
     (Reader : Image_Reader'Class;
      Path   : String;
      Image  : out Image_Type'Class)
   is
      File : WL.Binary_IO.File_Type;
   begin
      WL.Binary_IO.Open (File, WL.Binary_IO.In_File, Path);
      Reader.Read (File, Image);
      WL.Binary_IO.Close (File);
   end Read;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Image : in out Image_Type'Class;
      X     : Pixel_X_Range;
      Y     : Pixel_Y_Range;
      Color : Image_Color)
   is
   begin
      Image.Set_Color (1, X, Y, Color);
   end Set_Color;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Image : in out Image_Type'Class;
      Layer : Layer_Index;
      X     : Pixel_X_Range;
      Y     : Pixel_Y_Range;
      Color : Image_Color)
   is
   begin
      Image.Layers (Layer).Data.Reference.Element (X, Y) := Color;
   end Set_Color;

end WL.Images;
