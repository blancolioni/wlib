private with WL.String_Maps;

package WL.String_Sets is

   pragma Preelaborate;

   type Set is tagged private;

   function Is_Empty
     (Container : Set)
      return Boolean;

   function Contains
     (Container : Set;
      Element   : String)
      return Boolean;

   procedure Clear
     (Container : in out Set);

   procedure Include
     (Container : in out Set;
      Element   : String);

   procedure Delete
     (Container : in out Set;
      Element   : String);

   procedure Iterate
     (Container : Set;
      Process   : not null access
        procedure (Item : String));

private

   package Sets is new WL.String_Maps (Boolean);

   type Set is tagged
      record
         Container : Sets.Map;
      end record;

   function Contains
     (Container : Set;
      Element   : String)
      return Boolean
   is (Container.Container.Contains (Element));

   function Is_Empty
     (Container : Set)
      return Boolean
   is (Container.Container.Is_Empty);

end WL.String_Sets;
