#INCLUDE SIVIGILA.H

FUNCTION loadTxtFile(sSourceFileNameAndPath as String, sMsg as String, sTargetTablename as String, ;
					sTxtFileType as String, bDeleteDuplicates as Boolean, sNonKeyFields as string, ;
					sPostCmds as string) as Boolean 

	LOCAL bContinue as Boolean 
	
	LOCAL oAnomaliesSolver as Object 
	oAnomaliesSolver = NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.)
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
*SET STEP ON 
	bReturnedValue = .T.
	bContinue = .T.
	
	IF !(VARTYPE(sTxtFileType) = 'C' AND !EMPTY(sTxtFileType))
		sTxtFileType = 'CSV'
		sFileExtensions = 'txt'
	ELSE
		sTxtFileType = UPPER(sTxtFileType)
		sFileExtensions = 'txt'
	ENDIF
	
	IF !FILE(sSourceFileNameAndPath) THEN
		sSourceFileNameAndPath = GETFILE(sFileExtensions,'','',0,sMsg)
		IF EMPTY(sSourceFileNameAndPath) THEN
			bContinue = .F.
			bReturnedValue = .F.
		ENDIF
	ELSE
		bContinue = .T.
	ENDIF

	IF bContinue THEN
		IF !EMPTY(sSourceFileNameAndPath) THEN
			=SelectTable(sTargetTablename,,.T.)
			sOldSafety = SET("Safety")
			SET SAFETY OFF
			TRY
				ZAP
			CATCH TO oException
				DELETE ALL 
			ENDTRY 
			
			TRY
				sCmd = "APPEND FROM '" + sSourceFileNameAndPath + "'"
				DO CASE 
					CASE sTxtFileType='CSV'
						sCmd = sCmd + sTxtFileType
					CASE sTxtFileType='TAB'
						sCmd = sCmd + ' DELIMITED WITH ' + sTxtFileType
					OTHERWISE 
						sCmd = sCmd + sTxtFileType
				ENDCASE 
				&sCmd 
				
				IF VARTYPE(sPostCmds)='C' AND !EMPTY(sPostCmds) THEN 
					&sPostCmds 
				ENDIF 
				
				IF bDeleteDuplicates THEN 
					oAnomaliesSolver.sXlsFilePath = SYS(2023)
					oAnomaliesSolver.sSourceTableName = sTargetTablename
					oAnomaliesSolver.sExcludedFields = sNonKeyFields 
					oAnomaliesSolver.deleteDuplicates(.F., 'SivigilaExternalDataLoaderDeleteDuplicates')
				ENDIF 
				
				bReturnedValue = (_TALLY!=0)
			CATCH TO oException
				bReturnedValue = .F.
			FINALLY
				USE 			
			ENDTRY
			
			SET SAFETY &sOldSafety
		ELSE
			bReturnedValue = .F.
		ENDIF
	ENDIF
	RELEASE oAnomaliesSolver
	
	RETURN bReturnedValue 
ENDPROC


PROCEDURE SivigilaExternalDataLoaderDeleteDuplicates(sSourceTableName as String, sRecordsToDeleteRSName as String, sDuplicatesCountingRSName as String)
	*Este procedimiento básicamente declara que todos los registros candidatos a ser borrados, deben ser borrados; en consecuencia, su ;
	implementación es vacía por cuanto sRecordsToDeleteRSName.BORRAR=.T. por defecto
ENDPROC 