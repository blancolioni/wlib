with Ada.Unchecked_Deallocation;

with WL.Allocation;
with WL.Trace;

package body WL.Caches is

   subtype Cache_Block_Index is Natural range 0 .. Cache_Block_Size - 1;

   type Cache_Block_Record (Leaf : Boolean);
   type Cache_Block is access Cache_Block_Record;

   type Cache_Entry is
      record
         Data    : Cached_Data;
         Handle  : Cached_Data_Handle;
      end record;

   function Null_Cache_Entry return Cache_Entry;
   function Is_Null (Item : Cache_Entry) return Boolean;

   type Array_Of_Sub_Blocks is array (Cache_Block_Index) of Cache_Block;
   type Array_Of_Elements   is array (Cache_Block_Index) of Cache_Entry;

   type Cache_Block_Record (Leaf : Boolean) is
      record
         Count : Natural     := 0;
         Next  : Cache_Block := null;
         case Leaf is
            when True =>
               Elements : Array_Of_Elements;
            when False =>
               Divider    : Data_Index := 0;
               Sub_Blocks : Array_Of_Sub_Blocks := (others => null);
         end case;
      end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Cache_Block_Record, Cache_Block);

   procedure Free is
      new Ada.Unchecked_Deallocation (Cached_Data_Record, Cached_Data_Handle);

   type Free_Block_Array is array (Boolean) of Cache_Block;

   type Cache_Record is
      record
         --  Store       : Backing_Store;
         Top         : Cache_Block;
         Max_Index   : Data_Index               := 0;
         Size        : Natural;
         Max_Size    : Natural;
         Depth       : Natural                  := 0;
         Hits        : Natural                  := 0;
         Misses      : Natural                  := 0;
         Free_Blocks : Free_Block_Array;
         LRU         : List_Of_References.List;
         Used        : List_Of_References.List;
         Allocations : Natural                  := 0;
      end record;

   Free_Handle_List : List_Of_Cached_Data_Handles.List;

   Debug_Cache      : Boolean := False;

   function Get_Cache_Entry (Cache    : Cache_Type;
                             Index    : Data_Index)
                            return Cache_Entry;

   procedure Remove_Cache_Entry (Cache : Cache_Type;
                                 Index : Data_Index);

   procedure Insert_Cache_Entry (Cache     : Cache_Type;
                                 New_Entry : Cache_Entry);

   function Allocate_Block (Cache : Cache_Type;
                            Leaf  : Boolean)
                           return Cache_Block;

   procedure Ensure_Size (Cache : Cache_Type;
                          Max   : Data_Index);

   procedure Trace_Message (Message : String);

   --------------------
   -- Allocate_Block --
   --------------------

   function Allocate_Block (Cache : Cache_Type;
                            Leaf  : Boolean)
                           return Cache_Block
   is
      Result : Cache_Block;
   begin
      if Cache.Free_Blocks (Leaf) = null then
         Result := new Cache_Block_Record (Leaf);
         WL.Allocation.Allocate (Result.all'Size, "cache-block");
         Cache.Allocations := Cache.Allocations + 1;
      else
         Result := Cache.Free_Blocks (Leaf);
         Cache.Free_Blocks (Leaf) := Cache.Free_Blocks (Leaf).Next;
         Result.Next := null;
      end if;
      return Result;
   end Allocate_Block;

   -----------
   -- Close --
   -----------

   procedure Close (Cache : in out Cache_Type) is

      procedure Free_Blocks (Top : in out Cache_Block) is
      begin
         if Top /= null then

            if Top.Leaf then
               for I in Top.Elements'Range loop
                  --  Release (Top.Elements (I).Data);
                  Free (Top.Elements (I).Handle);
               end loop;
            else
               for I in Top.Sub_Blocks'Range loop
                  Free_Blocks (Top.Sub_Blocks (I));
               end loop;
            end if;


            WL.Allocation.Deallocate (Top.all'Size, "cache-block");
            Free (Top);
         end if;
      end Free_Blocks;

   begin
      declare
         use List_Of_References;
         It : Iterator;
      begin
         --  Flush (Cache);
         It := Get_Start (Cache.Used);
         while not Off_Right (It) loop
            Remove_Cache_Entry (Cache, Current (It));
            Next (It);
         end loop;

         It := Get_Start (Cache.LRU);
         while not Off_Right (It) loop
            Remove_Cache_Entry (Cache, Current (It));
            Next (It);
         end loop;
      end;

      for I in Cache.Free_Blocks'Range loop
         declare
            It : Cache_Block := Cache.Free_Blocks (I);
            Next : Cache_Block;
         begin
            while It /= null loop
               Next := It.Next;
               WL.Allocation.Deallocate (It.all'Size, "cache-block");
               Free (It);
               It := Next;
            end loop;
         end;
      end loop;

      Free_Blocks (Cache.Top);

      declare
         procedure Free is
            new Ada.Unchecked_Deallocation (Cache_Record, Cache_Type);
      begin
         WL.Allocation.Deallocate (Cache.all'Size, "cache");
         Free (Cache);
      end;
   end Close;

   ------------------
   -- Create_Cache --
   ------------------

   function Create_Cache ( --  Store        : Backing_Store;
                          Maximum_Size : Natural)
                         return Cache_Type
   is
      Result : Cache_Type;
   begin
      Result := new Cache_Record;
      WL.Allocation.Allocate (Result.all'Size, "cache");
      --  Result.Store    := Store;
      Result.Max_Size  := Maximum_Size;
      Result.Max_Index := 0;
      List_Of_References.New_List (Result.LRU);
      return Result;
   end Create_Cache;

   ------------------
   -- Enable_Debug --
   ------------------

   procedure Enable_Debug (Enable : Boolean) is
   begin
      Debug_Cache := Enable;
   end Enable_Debug;

   -----------------
   -- Ensure_Size --
   -----------------

   procedure Ensure_Size (Cache : Cache_Type;
                          Max   : Data_Index)
   is
   begin
      while Cache.Max_Index < Max loop
         if Cache.Top = null then
            Cache.Top := Allocate_Block (Cache, True);
            Cache.Max_Index := Data_Index (Cache_Block_Size - 1);
            Cache.Depth := 1;
         else
            declare
               New_Cache : constant Cache_Block :=
                 Allocate_Block (Cache, False);
            begin
               New_Cache.Count := 1;
               New_Cache.Divider := Cache.Max_Index + 1;
               New_Cache.Sub_Blocks (0) := Cache.Top;
               Cache.Top := New_Cache;
            end;
            Cache.Max_Index :=
              Data_Index (Cache_Block_Size) * (Cache.Max_Index + 1) - 1;
            Cache.Depth := Cache.Depth + 1;
         end if;
      end loop;
   end Ensure_Size;

   -----------
   -- Flush --
   -----------

--     procedure Flush (Cache : in Cache_Type) is
--        use List_Of_References;
--        It : Iterator := Get_Start (Cache.Used);
--     begin
--        while not Off_Right (It) loop
--           declare
--              Ent : constant Cache_Entry :=
--                Get_Cache_Entry (Cache, Current (It));
--           begin
--              if Ent.Handle.Dirty then
--                 Store (Cache.Store, Ent.Data);
--                 Ent.Handle.Dirty := False;
--              end if;
--           end;
--           Next (It);
--        end loop;
--     end Flush;

   ---------------------
   -- Get_Cache_Entry --
   ---------------------

   function Get_Cache_Entry (Cache    : Cache_Type;
                             Index    : Data_Index)
                            return Cache_Entry
   is
      Block     : Cache_Block := Cache.Top;
      Sub_Index : Data_Index  := Index;
   begin

      if Index > Cache.Max_Index then
--           Trace_Message ("Cache miss: Index =" & Index'Img &
--                              "; Cache.Max_Index =" & Cache.Max_Index'Img);
         Cache.Misses := Cache.Misses + 1;
         return Null_Cache_Entry;
      end if;

      while Block /= null and then not Block.Leaf loop
         declare
            Divider : constant Data_Index := Block.Divider;
         begin
            Block :=
              Block.Sub_Blocks (Cache_Block_Index (Sub_Index / Divider));
            Sub_Index := Sub_Index mod Divider;
         end;
      end loop;
      if Block /= null then
         if not Is_Null (Block.Elements (Cache_Block_Index (Sub_Index))) then
            Cache.Hits := Cache.Hits + 1;
         else
--              Trace_Message ("Cache miss (null element):" & Index'Img);
            Cache.Misses := Cache.Misses + 1;
         end if;
         return Block.Elements (Cache_Block_Index (Sub_Index));
      else
--         Trace_Message ("Cache miss (null block):" & Index'Img);
         Cache.Misses := Cache.Misses + 1;
         return Null_Cache_Entry;
      end if;
   end Get_Cache_Entry;

   --------------------------
   -- Get_Cache_Statistics --
   --------------------------

   procedure Get_Cache_Statistics (Cache  : in     Cache_Type;
                                   Blocks :    out Natural;
                                   Pages  :    out Natural;
                                   Hits   :    out Natural;
                                   Misses :    out Natural)
   is
   begin
      Pages  := Cache.Size;
      Hits   := Cache.Hits;
      Misses := Cache.Misses;
      Blocks := Cache.Allocations;
   end Get_Cache_Statistics;

   -----------
   -- Fetch --
   -----------

   procedure Fetch (From       : in out Cache_Type;
                    Location   : in     Data_Index;
                    Success    :    out Boolean;
                    Result     :    out Cached_Data;
                    Handle     :    out Cached_Data_Handle)
   is
      Found_Entry : constant Cache_Entry := Get_Cache_Entry (From, Location);
   begin
      if Is_Null (Found_Entry) then
         Success := False;
         Handle  := null;
      else
         Result  := Found_Entry.Data;
         Handle  := Found_Entry.Handle;
         Success := True;
         Reference (Handle);

      end if;

   end Fetch;

   ------------------------
   -- Insert_Cache_Entry --
   ------------------------

   procedure Insert_Cache_Entry (Cache     : Cache_Type;
                                 New_Entry : Cache_Entry)
   is
      Block  : Cache_Block;
      Index  : Data_Index := New_Entry.Handle.Location;
   begin
      Ensure_Size (Cache, Index);
      Block := Cache.Top;
      while not Block.Leaf loop
         declare
            Divider    : constant Data_Index := Block.Divider;
            Sub_Index  : constant Cache_Block_Index :=
              Cache_Block_Index (Index / Divider);
         begin
            if Block.Sub_Blocks (Sub_Index) = null then
               if Divider = Data_Index (Cache_Block_Size) then
                  Block.Sub_Blocks (Sub_Index) :=
                    Allocate_Block (Cache, True);
               else
                  Block.Sub_Blocks (Sub_Index) :=
                    Allocate_Block (Cache, False);
                  Block.Sub_Blocks (Sub_Index).Divider :=
                    Divider / Data_Index (Cache_Block_Size);
               end if;
               Block.Count := Block.Count + 1;
            end if;
            Block := Block.Sub_Blocks (Sub_Index);
            Index := Index mod Divider;
         end;
      end loop;

      declare
         Block_Index : constant Cache_Block_Index :=
           Cache_Block_Index (Index);
      begin
         pragma Assert (Is_Null (Block.Elements (Block_Index)));
         Block.Elements (Block_Index) := New_Entry;
      end;

      Block.Count := Block.Count + 1;
      Cache.Size  := Cache.Size + 1;

   end Insert_Cache_Entry;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Item : Cache_Entry) return Boolean is
   begin
      return Item.Handle = null;
   end Is_Null;

   ----------------------
   -- Null_Cache_Entry --
   ----------------------

   function Null_Cache_Entry return Cache_Entry is
      Result : Cache_Entry;
   begin
      Result.Handle := null;
      return Result;
   end Null_Cache_Entry;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Handle : Cached_Data_Handle) is
   begin
--        Ada.Text_IO.Put_Line ("Referencing:" &
--                              File_Index'Image
--                              (Get_File (Info.Location)) &
--                              Page_Index'Image
--                              (Get_Page (Info.Location)));
      if Handle.References = 0 then
         if Debug_Cache then
            Trace_Message ("Restoring" &
                               Data_Index'Image (Handle.Location));
         end if;
         if Handle.Dropped then
            Trace_Message ("  dropped");
            Handle.Dropped := False;
         else
            List_Of_References.Delete (Handle.LRU);
         end if;
         List_Of_References.Append (Handle.From_Cache.Used, Handle.Location);
         Handle.LRU := List_Of_References.Get_End (Handle.From_Cache.Used);
      end if;
      Handle.References := Handle.References + 1;
      if Debug_Cache then
         Trace_Message ("Reference" &
                            Data_Index'Image (Handle.Location) &
                            ": reference count =" &
                            Natural'Image (Handle.References));
      end if;

   end Reference;

   ---------------------
   -- Reference_Count --
   ---------------------

   function Reference_Count (Handle : Cached_Data_Handle) return Natural is
   begin
      return Handle.References;
   end Reference_Count;

   ------------------------
   -- Remove_Cache_Entry --
   ------------------------

   procedure Remove_Cache_Entry (Cache : Cache_Type;
                                 Index : Data_Index)
   is
      Block      : Cache_Block := Cache.Top;
      Path       : array (1 .. Cache.Depth) of Cache_Block;
      Sub_Index  : Data_Index := Index;
      Index_Path : array (1 .. Cache.Depth) of Cache_Block_Index;
      Path_Count : Natural := 0;
   begin
      Trace_Message ("cache-remove: " &
                              Data_Index'Image (Index));

      while not Block.Leaf loop
         Path_Count := Path_Count + 1;
         Path (Path_Count) := Block;
         declare
            Divider      : constant Data_Index := Block.Divider;
            Block_Index  : constant Cache_Block_Index :=
              Cache_Block_Index (Sub_Index / Divider);
         begin
            Index_Path (Path_Count) := Block_Index;
            Block := Block.Sub_Blocks (Block_Index);
            Sub_Index := Sub_Index mod Divider;
         end;
      end loop;

      declare
         Old : Cache_Entry renames
           Block.Elements (Cache_Block_Index (Sub_Index));
      begin
         List_Of_Cached_Data_Handles.Append (Free_Handle_List, Old.Handle);
         Old := Null_Cache_Entry;
      end;

      Block.Count := Block.Count - 1;

      while Path_Count > 0 and Block.Count = 0 loop
         Block.Next := Cache.Free_Blocks (Block.Leaf);
         Cache.Free_Blocks (Block.Leaf) := Block;
         Block := Path (Path_Count);
         Block.Sub_Blocks (Index_Path (Path_Count)) := null;
         Block.Count := Block.Count - 1;
         Path_Count := Path_Count - 1;
      end loop;

   end Remove_Cache_Entry;

   ----------------------
   -- Reset_Statistics --
   ----------------------

   procedure Reset_Statistics (Cache : in Cache_Type) is
   begin
      Cache.Hits        := 0;
      Cache.Misses      := 0;
   end Reset_Statistics;

   ------------------
   -- Set_Accessed --
   ------------------

   procedure Set_Accessed (Handle : Cached_Data_Handle) is
   begin
      Handle.Last_Access := Ada.Calendar.Clock;
   end Set_Accessed;

   ---------------
   -- Set_Dirty --
   ---------------

   procedure Set_Dirty (Handle : Cached_Data_Handle) is
   begin
      Handle.Dirty := True;
   end Set_Dirty;

   -----------
   -- Store --
   -----------

   procedure Store (To     : in out Cache_Type;
                    Handle :    out Cached_Data_Handle;
                    Item   : in     Cached_Data)
   is
      Location    : constant Data_Index := Get_Index (Item);
      Found_Entry : Cache_Entry := Get_Cache_Entry (To, Location);
   begin

      Trace_Message ("cache-store: " &
                              Data_Index'Image (Get_Index (Item)));

      if not Is_Null (Found_Entry) then
         Remove_Cache_Entry (To, Location);
      end if;

      while To.Size >= To.Max_Size loop
         Trace_Message ("cache: removing excess entries");
         exit when List_Of_References.Empty (To.LRU);
         declare
            use List_Of_References;
            It   : Iterator := Get_Start (To.LRU);
            Item : constant Data_Index := Current (It);
         begin
            Remove_Cache_Entry (To, Item);
            Delete (It);
            To.Size := To.Size - 1;
         end;
      end loop;

      if List_Of_Cached_Data_Handles.Empty (Free_Handle_List) then
         Handle   := new Cached_Data_Record;
         WL.Allocation.Allocate (Handle.all'Size, "cache-data-handle");
      else
         declare
            use List_Of_Cached_Data_Handles;
            It : Iterator := Get_Start (Free_Handle_List);
         begin
            Handle := Current (It);
            Delete (It);
         end;
      end if;

      Handle.Last_Access := Ada.Calendar.Clock;
      Handle.From_Cache  := To;
      Handle.Location    := Location;
      Handle.References  := 1;
      Handle.Cached      := True;
      Handle.Dirty       := False;
      List_Of_References.Append (To.Used, Location);
      Handle.LRU := List_Of_References.Get_End (To.Used);

      Found_Entry := (Item, Handle);
         --  WL.Allocation.Allocate (Found_Entry.all'Size, "cache-entry");

      Insert_Cache_Entry (To, Found_Entry);

   end Store;

   -------------------
   -- Trace_Message --
   -------------------

   procedure Trace_Message (Message : String) is
   begin
      if Debug_Cache then
         WL.Trace.Put_Line (Message);
      end if;
   end Trace_Message;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Handle : Cached_Data_Handle) is
   begin

--        Ada.Text_IO.Put_Line ("Unreferencing:" &
--                              File_Index'Image
--                              (Get_File (Handle.Location)) &
--                              Page_Index'Image
--                              (Get_Page (Handle.Location)));

      if Handle = null then
         Trace_Message ("Error: Unreferencing null info");
      end if;

      if Debug_Cache then
         Trace_Message ("Unreference " &
                            Data_Index'Image (Handle.Location) &
                            ": reference count =" &
                            Natural'Image (Handle.References));
      end if;

      Handle.References := Handle.References - 1;

      if Handle.References = 0 then
         if Debug_Cache then
            Trace_Message ("Removing: " &
                               Data_Index'Image (Handle.Location));
         end if;

--           if Handle.Dirty then
--              declare
--                 Ent : constant Cache_Entry :=
--                   Get_Cache_Entry (Handle.From_Cache, Handle.Location);
--              begin
--                 Store (Handle.From_Cache.Store, Ent.Data);
--                 Handle.Dirty := False;
--              end;
--           end if;

         List_Of_References.Delete (Handle.LRU);
         List_Of_References.Append (Handle.From_Cache.LRU, Handle.Location);
         Handle.LRU := List_Of_References.Get_End (Handle.From_Cache.LRU);
      end if;

   end Unreference;

end WL.Caches;
