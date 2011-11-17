--  Simple memory allocation tracker

package WL.Allocation is

   procedure Allocate (Size    : Natural;
                       Message : String);

   procedure Deallocate (Size    : Natural;
                         Message : String);

   procedure Report;

end WL.Allocation;
