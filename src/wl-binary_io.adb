with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

package body WL.Binary_IO is

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Ada.Streams.Stream_Element_Array,
        Stream_Array_Access);

   procedure Flush
     (File : File_Type);

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      if File.Mode = Out_File then
         Flush (File);
      end if;

      if File.Data /= null then
         Free (File.Data);
         File.Data := null;
      end if;

   end Close;

   ----------
   -- Copy --
   ----------

   procedure Copy (File        : in     File_Type;
                   Offset      : in     Word_32;
                   Length      : in     Word_32;
                   Destination : in     System.Address)
   is
      use Ada.Streams;
      Data         : constant Stream_Array_Access := File.Data;
      Local_Buffer : Stream_Element_Array
        (0 .. Stream_Element_Count (Length) - 1);
      for Local_Buffer'Address use Destination;
   begin
      Local_Buffer := Data (Data'First + Stream_Element_Offset (Offset) ..
                              Data'First +
                                Stream_Element_Offset (Offset + Length) - 1);
   end Copy;

   ------------
   -- Create --
   ------------

   procedure Create (File : in out File_Type;
                     Mode : in     File_Mode;
                     Name : in     String)
   is
   begin
      File := (Ada.Strings.Unbounded.To_Unbounded_String (Name),
               new Ada.Streams.Stream_Element_Array (0 .. 65535),
               Mode, 0, 0);
      declare
         Stream : Ada.Streams.Stream_IO.File_Type;
      begin
         Ada.Streams.Stream_IO.Create
           (Stream, Ada.Streams.Stream_IO.Out_File, Name);
         Ada.Streams.Stream_IO.Close (Stream);
      end;

   end Create;

   --------------------
   -- Current_Offset --
   --------------------

   function Current_Offset (File : File_Type) return Word_32 is
   begin
      return File.Offset;
   end Current_Offset;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return File.Offset > Word_32 (File.Data'Last);
   end End_Of_File;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (File : File_Type)
   is
      use Ada.Streams.Stream_IO;
      Stream : Ada.Streams.Stream_IO.File_Type;
      Path   : constant String :=
                 Ada.Strings.Unbounded.To_String (File.Path);
   begin
      if Ada.Directories.Exists (Path) then
         Open (Stream, Append_File, Path);
      else
         Create (Stream, Out_File, Path);
      end if;

      Write (Stream, File.Data (0 .. File.Size));

      Close (Stream);
   end Flush;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Value : Word_32) return String is
   begin
      return Hex_Image (Word_16 (Value / 65536)) &
        Hex_Image (Word_16 (Value mod 65536));
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Value : Word_16) return String is
      Result : String (1 .. 4);
      V      : Word_16 := Value;
   begin
      for I in reverse Result'Range loop
         declare
            Digit : constant Word_16 := V mod 16;
         begin
            if Digit < 10 then
               Result (I) := Character'Val (Digit + 48);
            else
               Result (I) := Character'Val (Digit + 55);
            end if;
         end;
         V := V / 16;
      end loop;
      return Result;
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Value : Word_8) return String is
      Result : String (1 .. 2);
      V      : Word_8 := Value;
   begin
      for I in reverse Result'Range loop
         declare
            Digit : constant Word_8 := V mod 16;
         begin
            if Digit < 10 then
               Result (I) := Character'Val (Digit + 48);
            else
               Result (I) := Character'Val (Digit + 55);
            end if;
         end;
         V := V / 16;
      end loop;
      return Result;
   end Hex_Image;

   ----------
   -- Open --
   ----------

   procedure Open (File : in out File_Type;
                   Mode : in     File_Mode;
                   Name : in     String)
   is
      pragma Assert (Mode = In_File);
      use Ada.Streams;
      Length     : constant Stream_Element_Count :=
                     Stream_Element_Count (Ada.Directories.Size (Name));
      Stream     : Ada.Streams.Stream_IO.File_Type;
      Last       : Stream_Element_Offset;
   begin
      File.Data :=
        new Stream_Element_Array (0 .. Length - 1);
      File.Mode := In_File;

      Ada.Streams.Stream_IO.Open
        (Stream, Ada.Streams.Stream_IO.In_File, Name);

      Ada.Streams.Stream_IO.Read (Stream, File.Data.all, Last);
      pragma Assert (Last = Length);

      Ada.Streams.Stream_IO.Close (Stream);

   end Open;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in     File_Type;
                   Item   :    out Word_32;
                   Offset : in     Word_32)
   is
   begin
      Copy (File, Offset, 4, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in     File_Type;
                   Item   :    out Word_16;
                   Offset : in     Word_32)
   is
   begin
      Copy (File, Offset, 2, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in     File_Type;
                   Item   :    out Word_8;
                   Offset : in     Word_32)
   is
   begin
      Copy (File, Offset, 1, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in out File_Type;
                   Item   :    out Integer_32)
   is
   begin
      Read (File, Item'Size, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in out File_Type;
                   Item   :    out Integer_16)
   is
   begin
      Read (File, Item'Size, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in out File_Type;
                   Item   :    out Integer_8)
   is
   begin
      Read (File, Item'Size, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in out File_Type;
                   Item   :    out String)
   is
      X : Word_8;
   begin
      for I in Item'Range loop
         Read (File, X);
         Item (I) := Character'Val (X);
      end loop;
   end Read;

   ----------
   -- Read --
   ----------

   function Read (File   : File_Type;
                  Offset : Word_32;
                  Terminator : Character := Character'Val (0))
                  return String
   is
      Current : Word_32 := Offset;
      X       : Word_8;
      Index   : Natural := 0;
      Result  : String (1 .. 64);
   begin
      loop
         Read (File, X, Current);
         exit when X = Character'Pos (Terminator);
         Current := Current + 1;
         Index := Index + 1;
         Result (Index) := Character'Val (X);
         if Index = Result'Last then
            return Result & Read (File, Current, Terminator);
         end if;
      end loop;
      return Result (1 .. Index);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in out File_Type;
                   Item   :    out Word_32)
   is
   begin
      Read (File, Item'Size, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in out File_Type;
                   Item   :    out Word_16)
   is
   begin
      Read (File, Item'Size, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File   : in out File_Type;
                   Item   :    out Word_8)
   is
   begin
      Read (File, Item'Size, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File        : in out File_Type;
                   Size        : in     Word_32;
                   Destination : in     System.Address)
   is
      use Ada.Streams;
      Data         : constant Stream_Array_Access := File.Data;
      Unit_Size    : constant Word_32 :=
                       Size / Stream_Element'Size;
      Local_Buffer : Stream_Element_Array
        (0 .. Stream_Element_Count (Unit_Size) - 1);
      for Local_Buffer'Address use Destination;
      Start        : constant Stream_Element_Offset :=
                       Data'First + Stream_Element_Offset (File.Offset);
      Finish       : constant Stream_Element_Offset :=
                       Data'First +
                         Stream_Element_Offset (File.Offset + Unit_Size) - 1;
   begin
      Local_Buffer := Data (Start .. Finish);
      File.Offset := File.Offset + Unit_Size;
   end Read;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset (File : in out File_Type;
                         Offset : in Word_32)
   is
   begin
      File.Offset := Offset;
   end Set_Offset;

   ----------
   -- View --
   ----------

   function View
     (File : File_Type)
      return File_Type
   is
   begin
      return View_File : File_Type := File do
         View_File.Mode := In_File;
      end return;
   end View;

   -----------
   -- Write --
   -----------

   procedure Write (File   : in out File_Type;
                    Item   : in     Word_8)
   is
      use Ada.Streams;
   begin
      if File.Data'Last + 1 = File.Size then
         Flush (File);
         File.Size := 0;
      end if;
      File.Data (File.Size) := Stream_Element (Item);
      File.Size := File.Size + 1;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (File   : in out File_Type;
                    Item   : in     Word_16)
   is
   begin
      Write (File, 2, Item'Address);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (File   : in out File_Type;
                    Item   : in     Word_32)
   is
   begin
      Write (File, 4, Item'Address);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (File    : in out File_Type;
                    Length  : in     Word_32;
                    Source  : in     System.Address)
   is
      use Ada.Streams;
      Data : Stream_Element_Array (1 .. Stream_Element_Count (Length));
      for Data'Address use Source;
   begin
      for I in Data'Range loop
         Write (File, Word_8 (Data (I)));
      end loop;
   end Write;

end WL.Binary_IO;
