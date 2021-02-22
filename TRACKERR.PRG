**********************************************************************
* Program....: TrackErr.prg
* Compiler...: Visual FoxPro 06.00.8492.00 for Windows
* Abstract...: Program called by ON ERROR to log Error Details
* ...........: ON ERROR DO TrackErr WITH SET("DATASESSION"), PROGRAM(), LINENO()
**********************************************************************
LPARAMETERS tnDSID, tcProgram, tnLineNo, sErrFilesPath
LOCAL lcNextAction, lcUserMsg, lcErrFilesPath
*** Call Logging Method


SET PROCEDURE TO SIVIGILAMessenger ADDITIVE

*Fija la ruta en donde se guardarán  los archivos de reporte de errores
IF VARTYPE(sErrFilesPath) = 'C'
	IF NOT DIRECTORY(ALLTRIM(sErrFilesPath))
 		MKDIR (sErrFilesPath)
 	ENDIF
ELSE
	sErrFilesPath = '.\'
ENDIF
 

IF VARTYPE( goErrorLog ) = "O"
	goErrorlog.cLogTo = sErrFilesPath
    goErrorLog.LogError( tnDSID, tcProgram, tnLineNo )

    *** Now check the results
    lcNextAction = goErrorLog.cNextAction       && Next Action Required
    lcUserMsg    = goErrorLog.cUserMsg         && User Mesage Text

    *** Take Whatever Action is appropriate
    *** Typically this would be to display a Message for the user
    *** And either Re-Try or Close Down
    IF !EMPTY(lcUserMsg)
    	=showErrorMessage( lcUserMsg, 0 , 1 )
    ENDIF
	&lcNextAction
ELSE
    =showErrorMessage( lcUserMsg, 0 , 1 )
ENDIF

FUNCTION shutDownSivigila
	ON ERROR DUMMY = 1
	TRY
		CLOSE ALL
	CATCH TO oException
	ENDTRY
	SET PROCEDURE TO WPLIB ADDITIVE
	WPKillCurrentProcess()
ENDFUNC
