
-- Listas de carga de cami�n - Cabecera.
CREATE TABLE F80C001 (
	F80CNUMLST char(6) NOT NULL,			-- N� de lista de carga
	F80CFECCRE datetime NOT NULL,			-- Fecha de creaci�n
	F80CFECCNF datetime NOT NULL,			-- Fecha de confirmaci�n
	F80CAGRTRP char(1) NOT NULL,			-- Agrupar por transportista (S/N)
	F80CIDENTD char(1) NOT NULL,			-- Identificar destino (S/N)
	F80CESTLST char(1) NOT NULL 			-- Estado de la lista
)
go

ALTER TABLE F80C001
       ADD PRIMARY KEY (F80CNUMLST)
go
