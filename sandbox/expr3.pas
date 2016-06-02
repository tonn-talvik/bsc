program expr3;
var X: integer;
function foo(Y: integer): integer;
begin
  return 7 * Y;
end;
begin
  X := 5 + foo(5 / 3) * 2;
  writeln(X);
end.
