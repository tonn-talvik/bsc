program ifthen;
var
  X, Y, Z: integer;
begin
  X := 5;
  Y := 1;
  if X > Y then
  begin
    X := X / 2;
    Y := Y + 1;
    if X = Y then
      X := X - 3;
  end else
  begin
    Y := X;
  end;
  Z := X * Y;
  writeln(Z);
end.
