program if3;
var
  X, Y: integer;
begin
  // show constant-folding and constant-propagation
  // also test for variable initializing
  if X > 5 then begin
    X := 5;
    Y := 42;
  end else begin
    writeln("variable X is not constant!");
    Y := 6 * 7;
  end;
  writeln(X);
  writeln(Y);
end. 
