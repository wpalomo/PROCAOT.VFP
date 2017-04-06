
-- Listas de carga de camión - Cabecera.
CREATE TABLE F80C001 (
	F80CNUMLST char(6) NOT NULL,			-- Nº de lista de carga
	F80CFECCRE datetime NOT NULL,			-- Fecha de creación
	F80CFECCNF datetime NOT NULL,			-- Fecha de confirmación
	F80CAGRTRP char(1) NOT NULL,			-- Agrupar por transportista (S/N)
	F80CIDENTD char(1) NOT NULL,			-- Identificar destino (S/N)
	F80CESTLST char(1) NOT NULL 			-- Estado de la lista
)
go

ALTER TABLE F80C001
       ADD PRIMARY KEY (F80CNUMLST)
go
