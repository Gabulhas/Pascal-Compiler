program "micro04"

var
	x,num,intervalo: integer;

begin
	intervalo:=0;	
	for x := 1 to 5 do
	begin
		writeln('Digite um numero: '');
		readln(num);
		if(num >= 10) then
			begin
				if(num <=150)then
					begin
						intervalo := intervalo + 1;
					end;
			end;	
	end;
	writeln('Ao total foram digitados');
	writeln(intervalo);
	write('numeros no intervalo de 10 e 150');
end.
