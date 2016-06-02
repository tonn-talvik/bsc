program func4;
var X, Y, Z: integer;
function foo(): integer;
begin
  return 0;
end;
begin
  X := foo();
  if X > 0 then
    writeln("pos");
  writeln("exit");
end.
