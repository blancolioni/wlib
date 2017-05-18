with Ada.Text_IO;

with System.Storage_Elements;

package body WL.Shape_Files is

   Debug_Shapes : constant Boolean := False;

   type Record_Header is
      record
         Number     : WL.Binary_IO.Word_32;
         Length     : WL.Binary_IO.Word_32;
         Shape_Type : WL.Binary_IO.Word_32;
      end record;

   for Record_Header'Size use 3 * 32;

   function To_Shape_Class
     (Value : WL.Binary_IO.Word_32)
      return Shape_Class;

   procedure Change_Endian
     (Elements : in out System.Storage_Elements.Storage_Array);

   -------------------
   -- Change_Endian --
   -------------------

   procedure Change_Endian
     (Elements : in out System.Storage_Elements.Storage_Array)
   is
      use System, System.Storage_Elements;
      Temp : constant Storage_Array := Elements;
   begin
      for I in Elements'Range loop
         Elements (Elements'Last - I + Elements'First) := Temp (I);
      end loop;
   end Change_Endian;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      WL.Binary_IO.Close (File.File);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File
     (File : File_Type)
      return Boolean
   is
   begin
      return File.Shapes.Is_Empty
        and then WL.Binary_IO.End_Of_File (File.File);
   end End_Of_File;

   ----------
   -- Next --
   ----------

   function Next
     (File : in out File_Type)
      return Shape_Type
   is
   begin
      if File.Shapes.Is_Empty then
         declare
            use System.Storage_Elements;
            Header : Record_Header;
            Buffer : Storage_Array (0 .. 11);
            for Buffer'Address use Header'Address;
         begin
            WL.Binary_IO.Read (File.File, Header'Size, Header'Address);
            Change_Endian (Buffer (0 .. 3));
            Change_Endian (Buffer (4 .. 7));
            if Debug_Shapes then
               Ada.Text_IO.Put_Line
                 ("record" & Header.Number'Img
                  & " length" & Header.Length'Img
                  & " shape "
                  & Shape_Class'Image (To_Shape_Class (Header.Shape_Type)));
            end if;

            declare
               Shape : Shape_Type (To_Shape_Class (Header.Shape_Type));
            begin
               case Shape.Class is
               when Null_Shape =>
                  null;
               when Point_Shape =>
                  WL.Binary_IO.Read (File.File, 64, Shape.X'Address);
                  WL.Binary_IO.Read (File.File, 64, Shape.Y'Address);
               when Polygon_Shape | Polyline_Shape =>
                  WL.Binary_IO.Read (File.File, 256,
                                     Shape.Polygon_Bound'Address);
                  declare
                     use WL.Binary_IO;
                     Num_Parts, Num_Points : WL.Binary_IO.Word_32;
                  begin
                     WL.Binary_IO.Read (File.File, Num_Parts);
                     WL.Binary_IO.Read (File.File, Num_Points);
                     if Debug_Shapes then
                        Ada.Text_IO.Put_Line
                          ("num parts =" & Num_Parts'Img
                           & "; num points =" & Num_Points'Img);
                     end if;

                     declare
                        Skips : array (1 .. Num_Parts) of Word_32;
                        Pts   : array (1 .. Num_Points) of Shape_Point;
                     begin
                        for I in 1 .. Num_Parts loop
                           WL.Binary_IO.Read (File.File, Skips (I));
                        end loop;
                        for I in 1 .. Num_Points loop
                           declare
                              Pt  : Shape_Point;
                           begin
                              WL.Binary_IO.Read (File.File, 128, Pt'Address);
                              Pts (I) := Pt;
                           end;
                        end loop;

                        for I in 1 .. Num_Parts loop
                           declare
                              S : Shape_Type (Shape.Class);
                              Lo : constant Word_32 :=
                                     Skips (I) + 1;
                              Hi : constant Word_32 :=
                                     (if I < Num_Parts
                                      then Skips (I + 1)
                                      else Num_Points);
                           begin
                              for J in Lo .. Hi loop
                                 S.Polygon_Points.Append (Pts (J));
                              end loop;
                              File.Shapes.Append (S);
                           end;
                        end loop;
                     end;
                  end;

               end case;
            end;
         end;
      end if;

      declare
         Result : constant Shape_Type := File.Shapes.First_Element;
      begin
         File.Shapes.Delete_First;
         return Result;
      end;

   end Next;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Name : in     String)
   is
   begin
      File.Header := new File_Header;

      declare
         Buffer : System.Storage_Elements.Storage_Array (0 .. 99);
         for Buffer'Address use File.Header.all'Address;

      begin
         WL.Binary_IO.Open
           (File => File.File,
            Mode => WL.Binary_IO.In_File,
            Name => Name);
         WL.Binary_IO.Read (File.File, File_Header'Size,
                            File.Header.all'Address);
         Change_Endian (Buffer (0 .. 3));
         Change_Endian (Buffer (24 .. 27));

         Ada.Text_IO.Put_Line ("File length:" & File.Header.Length'Img);
         Ada.Text_IO.Put_Line ("File contents: "
                               & Shape_Class'Image
                                 (To_Shape_Class (File.Header.Shape_Type)));
      end;

   end Open;

   -----------------
   -- Point_Count --
   -----------------

   function Point_Count (Shape : Shape_Type) return Natural is
   begin
      return Shape.Polygon_Points.Last_Index;
   end Point_Count;

   -------------
   -- Point_X --
   -------------

   function Point_X (Shape : Shape_Type;
                     Index : Positive)
                     return Long_Float
   is
   begin
      return Shape.Polygon_Points.Element (Index).X;
   end Point_X;

   -------------
   -- Point_Y --
   -------------

   function Point_Y (Shape : Shape_Type;
                     Index : Positive)
                     return Long_Float
   is
   begin
      return Shape.Polygon_Points.Element (Index).Y;
   end Point_Y;

   --------------------
   -- To_Shape_Class --
   --------------------

   function To_Shape_Class
     (Value : WL.Binary_IO.Word_32)
      return Shape_Class
   is
   begin
      case Value is
         when 0 =>
            return Null_Shape;
         when 1 =>
            return Point_Shape;
         when 3 =>
            return Polyline_Shape;
         when 5 =>
            return Polygon_Shape;
         when others =>
            raise Constraint_Error with
              "bad shape type:" & Value'Img;
      end case;
   end To_Shape_Class;

end WL.Shape_Files;
