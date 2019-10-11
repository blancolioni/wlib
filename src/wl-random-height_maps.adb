package body WL.Random.Height_Maps is

   type Frequency_Table_Type is array (Positive range <>) of Natural;

   type Height_Table_Type is array (Natural range <>) of Integer;

   procedure Generate_Random_Heights
     (Heights    : in out Height_Array;
      Max_Height : Positive);

   procedure Smooth_Heights
     (Heights : in out Height_Array;
      Passes  : Natural;
      Neighbours  :     not null access
        function (Index : Positive) return Neighbour_Array);

   function Create_Frequency_Table
     (Heights    : Height_Array;
      Max_Height : Natural)
      return Frequency_Table_Type;

   function Create_Height_Table
     (Frequency_Table : Frequency_Table_Type;
      Frequencies     : Frequency_Map)
      return Height_Table_Type;

   procedure Transform_Heights
     (Heights : in out Height_Array;
      Table   : Height_Table_Type);

   ----------------------------
   -- Create_Frequency_Table --
   ----------------------------

   function Create_Frequency_Table
     (Heights    : Height_Array;
      Max_Height : Natural)
      return Frequency_Table_Type
   is
   begin
      return Table : Frequency_Table_Type (1 .. Max_Height) :=
        (others => 0)
      do
         for H of Heights loop
            Table (H) := Table (H) + 1;
         end loop;
      end return;
   end Create_Frequency_Table;

   -------------------------
   -- Create_Height_Table --
   -------------------------

   function Create_Height_Table
     (Frequency_Table : Frequency_Table_Type;
      Frequencies     : Frequency_Map)
      return Height_Table_Type
   is
      Local : Frequency_Map := Frequencies;
      Sum   : Natural := 0;
   begin

      for F of Frequency_Table loop
         Sum := Sum + F;
      end loop;

      declare
         Total      : Natural := 0;
      begin
         for F of Local loop
            Total := Total + F;
         end loop;

         for F of Local loop
            F := F * Sum / Total;
         end loop;
         for I in Local'First + 1 .. Local'Last loop
            Local (I) := Local (I) + Local (I - 1);
         end loop;
      end;

      declare
         Current : Integer := Frequencies'First;
         Total   : Natural := 0;
      begin

         return Table : Height_Table_Type (Frequency_Table'Range) do
            for I in Frequency_Table'Range loop
               Total := Total + Frequency_Table (I);
               if Current < Local'Last
                 and then Total > Local (Current)
               then
                  Current := Current + 1;
               end if;
               Table (I) := Current;
--                 declare
--                    use Ada.Text_IO;
--                 begin
--                    Put (I'Image);
--                    Set_Col (8);
--                    Put (Natural'Image (Frequency_Table (I)));
--                    Set_Col (16);
--                    Put (Total'Image);
--                    Set_Col (24);
--                    Put (Natural'Image (Total * 100 / Sum)
--                         & "%");
--                    Set_Col (32);
--                    Put (Natural'Image (Local (Current)));
--                    Set_Col (40);
--                    Put (Current'Image);
--                    New_Line;
--                 end;

            end loop;
         end return;
      end;

   end Create_Height_Table;

   -------------------------
   -- Generate_Height_Map --
   -------------------------

   procedure Generate_Height_Map
     (Heights     : out Height_Array;
      Frequencies : Frequency_Map;
      Smoothing   :     Natural;
      Neighbours  :     not null access function
        (Index : Positive) return Neighbour_Array)
   is
      Max_Height : constant Natural := Frequencies'Length * 100;
   begin
      Generate_Random_Heights (Heights, Max_Height);

      Smooth_Heights (Heights, Smoothing, Neighbours);

      declare
         Frequency_Table : constant Frequency_Table_Type :=
           Create_Frequency_Table (Heights, Max_Height);
         Height_Table    : constant Height_Table_Type :=
           Create_Height_Table (Frequency_Table, Frequencies);
      begin
         Transform_Heights (Heights, Height_Table);
      end;

   end Generate_Height_Map;

   -----------------------------
   -- Generate_Random_Heights --
   -----------------------------

   procedure Generate_Random_Heights
     (Heights    : in out Height_Array;
      Max_Height : Positive)
   is
   begin
      for H of Heights loop
         H := WL.Random.Random_Number (0, Max_Height - 1);
      end loop;
   end Generate_Random_Heights;

   --------------------
   -- Smooth_Heights --
   --------------------

   procedure Smooth_Heights
     (Heights     : in out Height_Array;
      Passes      : Natural;
      Neighbours  :     not null access
        function (Index : Positive) return Neighbour_Array)
   is
   begin
      for Pass_Index in 1 .. Passes loop
         declare
            Old : constant Height_Array := Heights;
         begin
            for I in Heights'Range loop
               declare
                  T  : Natural := Old (I);
                  Ns : constant Neighbour_Array := Neighbours (I);
               begin
                  for N of Ns loop
                     T := T + Old (N);
                  end loop;
                  Heights (I) := T / (Ns'Length + 1);
               end;
            end loop;
         end;
      end loop;
   end Smooth_Heights;

   -----------------------
   -- Transform_Heights --
   -----------------------

   procedure Transform_Heights
     (Heights : in out Height_Array;
      Table   : Height_Table_Type)
   is
   begin
      for H of Heights loop
         H := Table (H);
      end loop;
   end Transform_Heights;

end WL.Random.Height_Maps;
