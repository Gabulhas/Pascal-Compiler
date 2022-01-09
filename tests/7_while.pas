program "while_test"

var
    a  :int; 

begin
    a:=0;
    while a < 10 do
        begin
            write(a);
            a := a + 1;
        end;
end.
