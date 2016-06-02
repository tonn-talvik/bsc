program abc;
var
   X, Y	: integer;
begin
   X := 5;
   Y := 1;
   if X + Y < 5 then
     X := X + Y * 5;
   Y := 0;
   writeln(X);
end.

