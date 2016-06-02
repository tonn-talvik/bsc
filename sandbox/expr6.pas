program expr6;
var X: integer;
function f(): integer;
begin
  writeln("side effect");
  return 1;
end;
begin
  X := (f()/2 + f()/2) * (f()/2 + f()/2);
  writeln(X);
end.
