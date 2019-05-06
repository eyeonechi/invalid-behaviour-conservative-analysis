-- contains debug functions for printing things
package Debug with SPARK_Mode is
   -- print a string
   procedure Put(Item : in String) with
     Global => null;
   
   -- print a string and add a new line 
   procedure Put_Line(Item : in String) with
     Global => null;
   
   -- print an integer (with no new line)
   procedure Put(Item : in Integer) with
     Global => null;
   
   -- new line
   procedure New_Line with
     Global => null;
   
   -- print a single character
   procedure Put(Item : in Character) with
     Global => null;
   
end Debug;
