*SET STEP ON 
LOCAL nRecToGoto as Number 

nRecToGoto = 0
*DO Form frmSearchRec WITH "EVENTOS_43" TO nRecToGoto 
*DO Form frmSearchRec WITH "EVENTOS_24" TO nRecToGoto 
*DO Form frmSearchRec WITH "EVENTOS_32" TO nRecToGoto 
*DO Form frmSearchRec WITH "LABORATORIOS" TO nRecToGoto 
DO Form frmSearchRec WITH "LABORATORIOS", "NUM_IDE='1234567891234567'" TO nRecToGoto 

IF nRecToGoto >0 THEN
	MESSAGEBOX("Registro seleccionado:" + ALLTRIM(STR(nRecToGoto)), 0 + 48 + 256)
ELSE
	IF nRecToGoto = -1 THEN
		MESSAGEBOX("No seleccionón ningún registro pero indicó que se debe agregar uno a los existentes", 0 + 48 + 256)
	ELSE
		MESSAGEBOX("No se encontraron registros o no seleccionó ninguno o canceló la búsqueda", 0 + 48 + 256)
	ENDIF
ENDIF

RELEASE nRecToGoto 
		
