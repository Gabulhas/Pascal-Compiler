program "multiple"

var numero : int;
var x : int;
var num : int;
var intervalo: int;

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
		write("O numero nao esta no intervalo entre 100 e 200");
	end;

	intervalo:=0;	
	for x := 1 to 5 do
	begin
		write('Digite um numero: '');
		read(num);
		if(num >= 10) then
			begin
				if(num <=150)then
					begin
						intervalo := intervalo + 1;
					end;
			end;	
	end;
	write('Ao total foram digitados');
	write(intervalo);
	write('numeros no intervalo de 10 e 150');
end.
