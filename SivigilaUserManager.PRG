#INCLUDE SIVIGILA.H

FUNCTION hasRight(sOperationID as String, sSivigilaUserID as String) AS Boolean 

	LOCAL bHasRight as Boolean 
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	SET PROCEDURE TO (PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger') ADDITIVE

	=SelectTable('ENTRA_03','COD_CLA',.F.)
	SEEK  PADR(sSivigilaUserID,10,' ') + UPPER(sOperationID)
	
	bHasRight = (FOUND() AND ENTRA_03.ACCESO = 'S')
	IF !bHasRight THEN 
		=showErrorMessage('No se le ha asignado permiso para ejecutar esta operación. Solicite autorización al administrador del sistema.', 0)
	ENDIF 
	
	RETURN bHasRight
ENDFUNC 

 