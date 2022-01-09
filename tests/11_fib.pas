program fib;
var f : int;
procedure fib(n : int);
    procedure soma();
        var tmp : int;
        begin
            fib(n-2); tmp := f;
            fib(n-1); f := f + tmp
        end.
begin
    if n <= 1 then f := n else soma()
end.

begin fib(3); writeln(f) end.
