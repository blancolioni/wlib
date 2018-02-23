package body WL.String_Sets is

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Set;
      Element   : String)
   is
   begin
      if Container.Contains (Element) then
         Container.Container.Delete (Element);
      end if;
   end Delete;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Set;
      Element   : String)
   is
   begin
      if not Container.Contains (Element) then
         Container.Container.Insert (Element, True);
      end if;
   end Insert;

end WL.String_Sets;
