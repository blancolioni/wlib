with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Unchecked_Deallocation;

package body WL.Binary_IO is

   package Storage_Element_IO is
     new Ada.Sequential_IO (System.Storage_Elements.Storage_Element);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (System.Storage_Elements.Storage_Array,
        Storage_Array_Access);

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      if File.Mode = Out_File then
         declare
            use Storage_Element_IO;
            use type System.Storage_Elements.Storage_Offset;
            Seq_File   : Storage_Element_IO.File_Type;
         begin
            Open (Seq_File, Append_File, File.Path.all);

            for I in 0 .. File.Size - 1 loop
               Write (Seq_File, File.Data (I));
            end loop;

            Close (Seq_File);

         end;

      end if;

      if File.Data /= null then
         Free (Storage_Array_Access (File.Data));
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
      use System.Storage_Elements;
      Data         : constant access Storage_Array := File.Data;
      Local_Buffer : Storage_Array (0 .. Storage_Count (Length) - 1);
      for Local_Buffer'Address use Destination;
   begin
      Local_Buffer := Data (Data'First + Storage_Offset (Offset) ..
                              Data'First +
                                Storage_Offset (Offset + Length) - 1);
   end Copy;

   ------------
   -- Create --
   ------------

   procedure Create (File : in out File_Type;
                     Mode : in     File_Mode;
                     Name : in     String)
   is
   begin
      File := (new String'(Name),
               new System.Storage_Elements.Storage_Array (0 .. 65535),
               Mode, 0, 0);

      declare
         use Storage_Element_IO;
         Seq_File   : Storage_Element_IO.File_Type;
      begin
         Create (Seq_File, Out_File, File.Path.all);
         Close (Seq_File);
      end;

   end Create;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return File.Offset > Word_32 (File.Data'Last);
   end End_Of_File;

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
      pragma Unreferenced (Mode);
      use System.Storage_Elements;
      use Storage_Element_IO;
      Length     : constant Storage_Count :=
                     Storage_Count (Ada.Directories.Size (Name));
      Seq_File   : Storage_Element_IO.File_Type;
   begin
      File.Data :=
        new System.Storage_Elements.Storage_Array (0 .. Length - 1);
      File.Mode := In_File;

      Open (Seq_File, In_File, Name);

      for I in File.Data'Range loop
         Read (Seq_File, File.Data (I));
      end loop;

      Close (Seq_File);
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

   procedure Read (File   : in out File_Type;
                   Item   :    out System.Storage_Elements.Storage_Array)
   is
   begin
      Read (File, Item'Size, Item'Address);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (File    : in out File_Type;
      Item    :    out System.Storage_Elements.Storage_Array;
      Offset  : Word_32)
   is
      Current : Word_32 := Offset;
      X : Word_8;
   begin
      for I in Item'Range loop
         Read (File, X, Current);
         Item (I) := System.Storage_Elements.Storage_Element (X);
         Current := Current + 1;
      end loop;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (File        : in out File_Type;
                   Size        : in     Word_32;
                   Destination : in     System.Address)
   is
      use System.Storage_Elements;
      Data         : constant access Storage_Array := File.Data;
      Unit_Size    : constant Word_32 := Size / System.Storage_Unit;
      Local_Buffer : Storage_Array (0 .. Storage_Count (Unit_Size) - 1);
      for Local_Buffer'Address use Destination;
   begin
      Local_Buffer := Data (Data'First + Storage_Offset (File.Offset) ..
                              Data'First +
                                Storage_Offset (File.Offset + Unit_Size) - 1);
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

   -----------
   -- Write --
   -----------

   procedure Write (File   : in out File_Type;
                    Item   : in     Word_8)
   is
      use System.Storage_Elements;
   begin
      if File.Data'Last + 1 = File.Size then
         declare
            use Storage_Element_IO;
            Seq_File   : Storage_Element_IO.File_Type;
         begin
            Open (Seq_File, Append_File, File.Path.all);

            for I in 0 .. File.Size - 1 loop
               Write (Seq_File, File.Data (I));
            end loop;

            Close (Seq_File);

            File.Size := 0;
         end;
      end if;
      File.Data (File.Size) := Storage_Element (Item);
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
      use System.Storage_Elements;
      Data : Storage_Array (1 .. Storage_Count (Length));
      for Data'Address use Source;
   begin
      for I in Data'Range loop
         Write (File, Word_8 (Data (I)));
      end loop;
   end Write;

end WL.Binary_IO;
