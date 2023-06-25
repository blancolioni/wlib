with Ada.Finalization;

generic
   type Element is private;
package WL.Lists.Generic_List is

   pragma Preelaborate;

   List_Error : exception;

   type List is private;
   type Mark is private;
   type Iterator is private;

   procedure New_List (L : out List);
   function New_List return List;
   procedure Free_List (L : in out List);
   function Empty (L : in List) return Boolean;
   procedure Insert (L : in out List; E : Element);
   procedure Append (L : in out List; E : Element);
   function First (L : in List) return Element;
   function Last (L : in List) return Element;
   function Get (L : in List; Entry_No : Natural) return Element;

   function Copy (L : in List) return List;

   function Get_Start (L : in List) return Iterator;
   function Get_End (L : in List) return Iterator;
   function Get_Entry (L : in List; Entry_No : Natural) return Iterator;

   function Head (L : in List) return Element;
   function Tail (L : in List) return List;

   function Length (L : in List) return Natural;

   function Current (It : in Iterator) return Element;
   function Position (It : in Iterator) return Positive;
   procedure Goto_Start (It : in out Iterator);
   procedure Goto_End (It : in out Iterator);
   procedure Next (It : in out Iterator);
   procedure Prev (It : in out Iterator);
   function Next (It : in Iterator) return Iterator;
   function Prev (It : in Iterator) return Iterator;
   procedure Goto_Entry (It : in out Iterator; Ent : Natural);

   function At_Start (It : in Iterator) return Boolean;
   function At_End (It : in Iterator) return Boolean;
   function Off_Right (It : in Iterator) return Boolean;
   function Off_Left (It : in Iterator) return Boolean;

   procedure Replace (It : in out Iterator; E : Element);
   procedure Delete (It : in out Iterator);
   procedure Insert (It : in out Iterator; E : in Element);

   procedure Insert_After (It : in out Iterator; E : in Element);

   function Get_Mark (It : in Iterator) return Mark;
   function Get_Element (M : in Mark) return Element;
   procedure Set_Element (M : in Mark; E : in Element);
   procedure Goto_Mark (It : in out Iterator; M : in Mark);
   function Off_Right (M : in Mark) return Boolean;
   function Off_Left (M : in Mark) return Boolean;

   type Imager is access function (E : in Element) return String;

   function Image (L : List; Element_Image : Imager) return String;

   type Application is access procedure (E : in out Element);
   procedure Apply (L : List; A : Application);

private
   type List_Node;
   type List_Node_Ptr is access List_Node;

   type Element_Access is access Element;
   type List_Node is
      record
         E    : Element_Access;
         Next : List_Node_Ptr;
         Prev : List_Node_Ptr;
      end record;

   type List_Record is
      record
         First, Last : List_Node_Ptr;
         Count       : Natural;
      end record;

   type List_Record_Access is access List_Record;

   type List is new Ada.Finalization.Controlled with
      record
         Lst : List_Record_Access;
      end record;
   procedure Initialize (L : in out List);
   procedure Adjust (L : in out List);
   procedure Finalize (L : in out List);

   type Iterator is
      record
         It_List      : List_Record_Access;
         Curr         : List_Node_Ptr;
         Off_R, Off_L : Boolean;
         Position     : Natural;
      end record;

   type Mark is new Iterator;

end WL.Lists.Generic_List;
