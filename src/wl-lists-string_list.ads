--  Convenience package for manipulating lists of strings.

with WL.Strings;
with WL.Lists.Generic_List;

package WL.Lists.String_List is
   new WL.Lists.Generic_List (WL.Strings.String_Access);
