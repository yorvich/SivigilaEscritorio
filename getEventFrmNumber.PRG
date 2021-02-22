LPARAMETERS sEventCodeOrEventName as String

SELECT ALLTRIM(formulario) AS Form,* FROM eventos WHERE cod_eve='&sEventCodeOrEventName' INTO CURSOR rsForms

IF _TALLY=0 
	SELECT ALLTRIM(formulario) AS Form,* FROM eventos WHERE UPPER(sEventCodeOrEventName) $ NOM_EVE INTO CURSOR rsForms
	IF _TALLY>0 
		SELECT EVENTOS
		BROWSE FOR UPPER(sEventCodeOrEventName) $ NOM_EVE LAST 
	ENDIF
ELSE
	SELECT EVENTOS
	BROWSE FOR cod_eve='&sEventCodeOrEventName' LAST 
ENDIF
 

