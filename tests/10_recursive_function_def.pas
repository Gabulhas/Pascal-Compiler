program "syracuse"
var 
    batata: boolean;

procedure syracuse0(max1 : int);
var i1 : int;
    procedure length1();
    var v2,j2 : int;
        procedure step2();
        begin
        length1();
        v2 := v2+1; 
        if j2 = 2*(j2/2) then 
            j2 := j2/2;
        else
            j2 := 3*j2+1;
        end.
    begin
    v2 := 0; j2 := i1; while !(j2 = 1) do step2(); write(v2);
    end.
begin
    i1 := 1;
    while i1 <= max1 do begin length1(); i1 := i1+1; end;
end.
begin syracuse0(100); end.
