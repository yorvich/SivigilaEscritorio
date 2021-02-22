LPARAMETERS sVFPErrorMessage as String, sVFPLineOfCode as String, sVFPProgram as String, nVFPLineNumber as Number, bDoNotQuitOnError as Boolean

sErrMsg="Se ha producido el siguiente error: " + CHR(13) + CHR(10) + ;
		sVFPErrorMessage  + CHR(13) + CHR(10) + "Ejecutando: " + sVFPLineOfCode +;
		CHR(13) + CHR(10) + "En el programa: " + sVFPProgram +;
		CHR(13) + CHR(10) + "Línea número: " + ALLTRIM(STR(nVFPLineNumber))
IF LEN(sErrMsg)>=255 then
	sErrMsg = SUBSTR(sErrMsg,1,251) + '...'
ENDIF 		
WAIT sErrMsg WINDOW TIMEOUT 15
CLOSE ALL
IF !bDoNotQuitOnError 
	QUIT
ENDIF 
