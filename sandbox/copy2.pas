program copy2;
var X, Y, Z: integer;
begin
  X := 1;
  Y := X;
  X := 5;
  Z := Y;
  writeln(X+Y+Z);
end.
