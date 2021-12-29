program "multiple"

var
numero : int;
x : int;
num : int;
intervalo: int;

function cebola(batata:int): int;

var
   kek: int;
begin
    write("hey");
end.

function max(num1, num2: int): int;

var
   result: int;

begin
    x :=4;
end.

procedure write_value(num: int);
begin
    write(num);
end.


begin
    write("Digite um numero: ");
    read(numero);
    if(numero >= 100) then
    begin
        if(numero <= 200) then
        begin
            write("O numero esta no intervalo entre 100 e 200");
        end;
        else begin
            write("O numero nao esta no intervalo entre 100 e 200");
        end;
    end;
    else begin
        for x := 1 to 5 do
        begin
            write("O numero nao esta no intervalo entre 100 e 200");
        end;
    end;
    if((numero < 4) & !(numero > 9) ) then
        intervalo[4] := 5;
    else
        while (numero > 5) do
            intervalo[4] := 5 * 5 + 8 - 1;

    if (numero >= 100) then x:=0;
    if (numero >= 991) then x:=9;
    else
        x:=1;

    write(cebola, 4 + 1);
    write(numero & true);

end.
