PROCEDURE ErrHandler

	PARAMETERS oException as Exception, vErrorProgram, vLineNumber
	
	IF VARTYPE(oException)='L' THEN
		sMsgError="Se ha producido el siguiente error inesperado:" + CHR(13) + CHR(10) + ;
					"Código de error: " + LTRIM(STR(ERROR())) + CHR(13) +;
					"Mensaje de error: " + MESSAGE() + CHR(13) +;
					"Componente que generó el error: " + vErrorProgram + CHR(13) +;
					"En la línea: " + vLineNumber
		MESSAGEBOX( sMsgError,0+16,"Error")
		ON ERROR
		CANCEL
	ELSE
		WITH oException
			INSERT INTO LogErrores (ErrorNo, Details, ProcedureName, ErrorMessage, UserValue, LineContents) VALUES (.ErrorNo, ;
				.Details, .Procedure, .Message, .UserValue, .LineContents)
		ENDWITH
	ENDIF	
ENDPROC


PROCEDURE cleanErrorsLogTable

	DELETE FROM LogErrores
	SELECT LogErrores
	TRY
		PACK
	CATCH 
	ENDTRY
	USE

ENDPROC


PROCEDURE showErrorLog

	USE LogErrores AGAIN IN 0 ALIAS ErrorsLogTable
	SELECT ErrorsLogTable
	IF RECCOUNT()>0 THEN
		MESSAGEBOX( 'Se encontraron errores', 0 + 48 + 256, 'Validación MIGRACIÓN SIVIGILA-->SISTEMA OBJETIVO' )
		BROWSE
	ELSE
		MESSAGEBOX( 'No hay detalles adicionales de los errores o no hay errores', 0 + 48 + 256, 'Validación MIGRACIÓN SIVIGILA-->SISTEMA OBJETIVO' )
		USE
	ENDIF
ENDPROC
