program "for_test"

var
    a  :int; 
    b :int;

begin
    read(b);
    write(b);

    for a:= 10 to b do
        begin
            write(a);
        end;
    for a:= b downto 10 do
        begin
            write(a);
        end;
    for a:= 10 to b do
        begin
            write(a);
        end;

end.
