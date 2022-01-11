program "read_val"

var
    a :int;
    b :boolean;

begin
    read(a);
    write(a);
    if a > 10 then
        b := true;
    else
        b := false;
    
    if b then
        write(1);
    else
        write(0);
end.
