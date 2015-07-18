with System.Storage_Elements;

package WL.Binary_IO is

   type File_Type is private;

   type File_Mode is (In_File, Out_File);

   procedure Open (File : in out File_Type;
                   Mode : in     File_Mode;
                   Name : in     String);

   procedure Create (File : in out File_Type;
                     Mode : in     File_Mode;
                     Name : in     String);

   procedure Close (File : in out File_Type);

   type Word_32 is mod 2 ** 32 with Size => 32;
   type Word_16 is mod 2 ** 16 with Size => 16;
   type Word_8 is mod 2 ** 8 with Size => 8;

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1 with Size => 32;
   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1 with Size => 16;
   type Integer_8 is range -2 ** 7 .. 2 ** 7 - 1 with Size => 8;

   procedure Read (File   : in out File_Type;
                   Item   :    out Word_32);

   procedure Read (File   : in out File_Type;
                   Item   :    out Word_16);

   procedure Read (File   : in out File_Type;
                   Item   :    out Word_8);

   procedure Read (File   : in out File_Type;
                   Item   :    out Integer_32);

   procedure Read (File   : in out File_Type;
                   Item   :    out Integer_16);

   procedure Read (File   : in out File_Type;
                   Item   :    out Integer_8);

   procedure Read (File   : in out File_Type;
                   Item   :    out String);

   procedure Read (File   : in out File_Type;
                   Item   :    out System.Storage_Elements.Storage_Array);

   procedure Read
     (File    : in out File_Type;
      Item    :    out System.Storage_Elements.Storage_Array;
      Offset  : Word_32);

   procedure Read (File        : in out File_Type;
                   Size        : in     Word_32;
                   Destination : in     System.Address);

   procedure Read (File   : in     File_Type;
                   Item   :    out Word_32;
                   Offset : in     Word_32);

   procedure Read (File   : in     File_Type;
                   Item   :    out Word_16;
                   Offset : in     Word_32);

   procedure Read (File   : in     File_Type;
                   Item   :    out Word_8;
                   Offset : in     Word_32);

   function Read (File   : File_Type;
                  Offset : Word_32;
                  Terminator : Character := Character'Val (0))
                  return String;

   procedure Write (File   : in out File_Type;
                    Item   : in     Word_32);

   procedure Write (File   : in out File_Type;
                    Item   : in     Word_16);

   procedure Write (File   : in out File_Type;
                    Item   : in     Word_8);

   procedure Copy (File        : in     File_Type;
                   Offset      : in     Word_32;
                   Length      : in     Word_32;
                   Destination : in     System.Address);

   procedure Write (File   : in out File_Type;
                    Length  : in     Word_32;
                    Source  : in     System.Address);

   procedure Set_Offset (File : in out File_Type;
                         Offset : in Word_32);

   function Hex_Image (Value : Word_32) return String;
   function Hex_Image (Value : Word_16) return String;
   function Hex_Image (Value : Word_8) return String;

   function End_Of_File (File : File_Type) return Boolean;

private

   type Storage_Array_Access is
     access all System.Storage_Elements.Storage_Array;

   type File_Type is
      record
         Path   : access String;
         Data   : Storage_Array_Access;
         Mode   : File_Mode;
         Size   : System.Storage_Elements.Storage_Count;
         Offset : Word_32 := 0;
      end record;

end WL.Binary_IO;
