program "arithmetic_expressions"

var
    numero  :int;
    alface  :int;
    treta   :int;
    cebola  :int;

begin
    numero:=4;
    treta := 9;
    cebola := 3 * 3;
    alface := 20;

    write(numero - treta);
    write(numero - treta - cebola);
    write(numero + alface + 5);
    write(alface / numero);
    write(9 % 2);


end.
