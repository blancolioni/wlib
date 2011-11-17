with Ada.Calendar;
with WL.Lists.Generic_List;

generic
--   type Backing_Store is private;
   type Cached_Data is private;
   type Data_Index is mod <>;
   Cache_Block_Size : Positive;

   with function Get_Index (From : Cached_Data) return Data_Index;

--   with procedure Release (Item : in out Cached_Data);

--     with procedure Fetch (From   : in     Backing_Store;
--                           Index  : in     Data_Index;
--                           Data   :    out Cached_Data);

--     with procedure Store (To   : in  Backing_Store;
--                           Data : in Cached_Data);

package WL.Caches is

   type Cached_Data_Handle is private;

   type Cache_Type is private;

   function Create_Cache ( --  Store        : Backing_Store;
                          Maximum_Size : Natural)
                         return Cache_Type;

   procedure Close (Cache : in out Cache_Type);

   procedure Fetch (From       : in out Cache_Type;
                    Location   : in     Data_Index;
                    Success    :    out Boolean;
                    Result     :    out Cached_Data;
                    Handle     :    out Cached_Data_Handle);

   procedure Store (To     : in out Cache_Type;
                    Handle :    out Cached_Data_Handle;
                    Item   : in     Cached_Data);

   procedure Set_Dirty (Handle : Cached_Data_Handle);
   procedure Set_Accessed (Handle : Cached_Data_Handle);

   procedure Reference (Handle : Cached_Data_Handle);
   procedure Unreference (Handle : Cached_Data_Handle);

   function Reference_Count (Handle : Cached_Data_Handle) return Natural;

   --  procedure Flush (Cache : in Cache_Type);

   procedure Get_Cache_Statistics (Cache  : in     Cache_Type;
                                   Blocks :    out Natural;
                                   Pages  :    out Natural;
                                   Hits   :    out Natural;
                                   Misses :    out Natural);

   procedure Reset_Statistics (Cache : in Cache_Type);

   procedure Enable_Debug (Enable : Boolean);

private

   type Cache_Record;
   type Cache_Type is access Cache_Record;

   package List_Of_References is
      new WL.Lists.Generic_List (Data_Index);

   type Cached_Data_Record is limited
      record
         Cached      : Boolean            := True;
         Dirty       : Boolean            := False;
         Location    : Data_Index;
         Last_Access : Ada.Calendar.Time;
         References  : Natural            := 1;
         Dropped     : Boolean            := False;
         LRU         : List_Of_References.Iterator;
         From_Cache  : Cache_Type;
      end record;

   type Cached_Data_Handle is access Cached_Data_Record;

   package List_Of_Cached_Data_Handles is
      new WL.Lists.Generic_List (Cached_Data_Handle);

end WL.Caches;
