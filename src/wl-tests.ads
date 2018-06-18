private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

package WL.Tests is

   type Test_Function is access
     function return Boolean;

   type Root_Test_Type is abstract tagged private;

   procedure Add_Message
     (Test    : in out Root_Test_Type'Class;
      Message : String);

   function Execute (Test : in out Root_Test_Type) return Boolean
                     is abstract;

   type Test_Suite is tagged private;

   procedure Append
     (Suite     : in out Test_Suite'Class;
      Category  : String;
      Name      : String;
      Test      : Test_Function);

   procedure Append
     (Suite     : in out Test_Suite'Class;
      Category  : String;
      Name      : String;
      Test      : Root_Test_Type'Class);

   procedure Run_Tests
     (Suite : Test_Suite'Class);

private

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Root_Test_Type is abstract tagged
      record
         Messages : String_Lists.List;
      end record;

   type Function_Test is
     new Root_Test_Type with
      record
         Test : Test_Function;
      end record;

   overriding function Execute (Test : in out Function_Test) return Boolean;

   type Test_Record is
      record
         Category_Name : Ada.Strings.Unbounded.Unbounded_String;
         Test_Name     : Ada.Strings.Unbounded.Unbounded_String;
         Test          : access Root_Test_Type'Class;
      end record;

   package Test_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Test_Record);

   type Test_Suite is tagged
      record
         Tests : Test_Lists.List;
      end record;

end WL.Tests;
