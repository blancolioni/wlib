procedure WL.Sorts.Insertion_Sort (E : in out Element_List) is
begin
   for I in Index_Type'Succ (E'First) .. E'Last loop
      declare
         V : constant Element_Type := E (I);
         J : Index_Type := Index_Type'Pred (I);
         Off_Left : Boolean := False;
      begin
         while V < E (J) loop
            E (Index_Type'Succ (J)) := E (J);
            if J = E'First then
               Off_Left := True;
               exit;
            end if;
            J := Index_Type'Pred (J);
         end loop;
         if Off_Left then
            E (J) := V;
         else
            E (Index_Type'Succ (J)) := V;
         end if;
      end;
   end loop;
end WL.Sorts.Insertion_Sort;
