program for2;
var I, X, Y, Z: integer;
begin
  X := 5;
  if Y > Z then
  begin
    for I := 5 to 15 do
    begin
      Z := Z * I;
      if Z < 10 then
      begin
        for Z := 1 to 10 do
        begin
          X := Z + Z * 3 + Z * 5;
          if X < Y then
          begin
            Y := Y - X;
            if Y - Z = 10 then
              Y := Y + 1
            else
              Break;
          end else begin
            Y := X * X;
          end;
        end;
      end;
      Continue;
    end;
    X := Y + Z;
  end else begin
    Z := 2 + 5;
    if Y < Z then
    begin
      X := Y / Z;
    end;
  end;
  Z := X * X * X;
end.
