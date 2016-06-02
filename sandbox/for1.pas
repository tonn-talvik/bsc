program for1;
var I, X: integer;
begin
  for I := 0 to 10 do
  begin
    if X > 5 then
      continue;
    X := X + 1;
  end;
end.
