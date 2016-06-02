program func;
var X: integer;
function foo(Y: integer): integer;
begin
  return Y + Y;
end;
begin
  X := foo(7);
  writeln(X);
end.
