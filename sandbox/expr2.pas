program expr2;
var X: integer;
function foo(): integer;
begin
  return 7;
end;
begin
  X := 5 + 6 + foo();
  writeln(X);
end.
