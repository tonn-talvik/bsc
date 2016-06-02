program expr4;
var A: array[5] of integer;
begin
  A[0] := 1;
  A[1] := 1;
  A[2] := A[0] + A[1];
  A[3] := A[1] + A[2];
  A[4] := A[2] + A[3];
  writeln(A[4]);
end.
