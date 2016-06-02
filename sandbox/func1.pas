program func1;
var X, Y, T: integer;
function foo(X: integer): integer;
var Z: integer;
begin
  Z := X * X;
  return Z;
end;
function bar(): integer;
var Y: integer;
begin
  Y := X * X;
  return Y;
end;
begin
  X := 3;
  Y := 1;
  T := foo(bar());
  writeln(X);
  writeln(Y);
  writeln(T);
end.
