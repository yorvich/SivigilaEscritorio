*SET STEP ON 
LOCAL vFoundedValue as Variant

*DO Form frmSearchLookupValue WITH "PRUEBAS" TO vFoundedValue 
*DO Form frmSearchLookupValue WITH "AGENTES" TO vFoundedValue 

SELECT Municipios.*,DEPTOS.nom_dep AS Departamento FROM Municipios LEFT OUTER JOIN DEPTOS ON  LEFT(Municipios.cod_mun,2) = Deptos.cod_dep;
	WHERE (!LIKE("01*",Municipios.cod_mun) AND !LIKE("11*",Municipios.cod_mun)) OR Municipios.cod_mun='11001' INTO CURSOR rsListaMunicipios  READWRITE
DO Form frmSearchLookupValue WITH "rsListaMunicipios" TO vFoundedValue 

IF !ISNULL(vFoundedValue) THEN
	MESSAGEBOX("Valor seleccionado: " + vFoundedValue, 0 + 48 + 256)
ELSE
	MESSAGEBOX("No seleccionó ningún valor", 0 + 48 + 256)
ENDIF

RELEASE vFoundedValue 
		
