program "if_and_boolean_expressions"

var
    a  :int; 
    b   :int; 
    c :boolean;

begin
    a:=4;
    b:=5;
    c:= false | (false & (a < b));
    if a > b then 
        write(a);
    if b > a then
        write(b);
    else 
        write(1);
end.
