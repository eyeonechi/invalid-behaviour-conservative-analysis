with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Debug is
   procedure Put(Item : in String) is
   begin
      Ada.Text_IO.Put(Item);
   end Put;

   procedure Put_Line(Item : in String) is
   begin
      Ada.Text_IO.Put_Line(Item);
   end Put_Line;

   procedure Put(Item : in Integer) is
   begin
      Ada.Integer_Text_IO.Put(Item => Item, Width => 0, Base => 10);
   end Put;

   procedure New_Line is
   begin
      Ada.Text_IO.New_Line;
   end New_Line;

   procedure Put(Item : in Character) is
   begin
      Ada.Text_IO.Put(Item);
   end Put;


end Debug;
