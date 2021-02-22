#INCLUDE SIVIGILA.H

FUNCTION showErrorMessage as Boolean
	
	LPARAMETERS sErrorMsg as String, nErrorType as Byte, nButtons as Byte
	
	* nErrorType:	2 = Error de procesamiento;
					1 = Error de validación;
					0 = Notificación
	
	* nButtons: 	0 = OK button only;
					1 = OK and Cancel buttons;
					2 = Abort, Retry, and Ignore buttons;
					3 = Yes, No, and Cancel buttons;
					4 = Yes and No buttons;
					5 = Retry and Cancel buttons
 
	IF VARTYPE(nButtons)!='N' THEN
		nButtons = 0
	ENDIF
	
	DO CASE
		CASE nErrorType = 2
			RETURN MESSAGEBOX( sErrorMsg, nButtons  + 48 + 256, MSG_PROCESSING_ERROR_TITLE)
		CASE nErrorType = 1
			RETURN MESSAGEBOX( sErrorMsg, nButtons  + 48 + 256, MSG_VALIDATION_ERROR_TITLE)
		CASE nErrorType = 0
			RETURN MESSAGEBOX( sErrorMsg, nButtons  + 48 + 256, MSG_NOTIFICATION_TITLE)
		OTHERWISE
			RETURN MESSAGEBOX( 'Se ha producido un error no identificado en el Sistema.', nButtons  + 48 + 256, MSG_UNEXPECTED_ERROR_TITLE)
	ENDCASE

ENDFUNC


FUNCTION getYesOrNo

	LPARAMETERS sQuestion as String, sTitleBar as String

	IF VARTYPE(sTitleBar )!='C'
		sTitleBar = ''
	ENDIF
	
	RETURN MESSAGEBOX('¿' + sQuestion + '?' , 4 + 32 + 256, sTitleBar )
ENDFUNC



FUNCTION showProgressMessage as Boolean
	
	LPARAMETERS sMsg as String
	*Muestra el mensaje sMsg en una ventana sin interrumpir la ejecución de cualquier proceso en segundo plano. ;
	La ventana permanecerá visible hasta que suceda un movimiento del cursor o hasta que se invoque a hideProgressMessage
	
	WAIT WINDOW ALLTRIM((sMsg)) NOWAIT
	
ENDFUNC



FUNCTION hideProgressMessage Boolean
	
	*Oculta la ventana de despliegue de mensajes, si es que hay alguna abierta
	
	WAIT CLEAR
	
ENDFUNC
