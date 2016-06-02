program func3;
var X, Y, Z: integer;
function foo(A: integer, B: integer, C: integer): integer;
begin
  return A + B * C /2;
end;
begin
  X := 5;
  Y := 6 * Y;
  Z := 7 + Y * X;
  X := foo(X, X+Y, foo(X, Y, Z));
end.
