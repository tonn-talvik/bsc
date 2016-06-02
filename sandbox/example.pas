program example;

type
  TComplex = record
    a: integer;
    b: integer;
  end;

var
  C1, C2: TComplex;
  L1, L2, L3: integer;
  I: integer;
  X, Y: integer;
  N, P, R: integer;

function isqrt(X: integer): integer;
begin
  writeln("undefined");
end;

begin
  C1.a := 4;
  C1.b := 3;

  X := C1.a;
  Y := C1.b;
  if X > Y then
  begin
    C2.a := 5;
    C2.b := readln();
  end else begin
    C2.a := readln();
    C2.b := 12;
  end;

  L1 := isqrt(C1.a * C1.a + C1.b * C1.b);
  L2 := isqrt(C2.a * C2.a + C2.b * C2.b);

  N := (C2.a - C1.a) * (C2.a - C1.a) + 
       (C2.b - C1.b) * (C2.b - C1.b);
  
  R := N;
  for I := 1 to 10 do
  begin
    P := R;
    R := (R + N / R) / 2;
    if R < P then
      writeln(R)
    else
      break;
  end;
  L3 := P;

  writeln(L3);
end.
