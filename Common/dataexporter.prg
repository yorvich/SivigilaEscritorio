#INCLUDE GlobalConst.H
#INCLUDE DataExporter.H

DEFINE CLASS DataExporter as Custom

sSourceTableName=.NULL.
sFilter =  .NULL.
sExportationPath = '.'
sExportedFileName = .NULL.
sExportedFileType = "XL5"
sExportingMsg = "Exportando a excel..."
sResultsMsg = .NULL.
nInitRecs = .NULL.

ErrorMessage = .NULL.
 

FUNCTION Init
	LPARAMETERS _sSourceTableName as String

	IF VARTYPE(_sSourceTableName)='C' THEN
		This.sSourceTableName=_sSourceTableName
	ENDIF
ENDFUNC


FUNCTION Destroy
	This.sSourceTableName=.NULL.
	This.nInitRecs = .NULL.
	This.sExportedFileType = "XL5"
	This.sExportedFileName = .NULL.
ENDFUNC



PROCEDURE exportToXLS

	*Exporta los registros de This.sSourceTableName a un archivo con formato XLS o CSV, dependiendo de ;
	si el número de registros en This.sSourceTableName es menor o mayor que nMaxRowsForXLS.
	
	*El archivo exportado tendrá nombre This.sSourceTableName -a menos que se indique lo contrario en this.sExportedFileName- ;
	y quedará ubicado en This.sExportationPath. ;
	
	nSelectedWorkArea=SELECT()
	sOldSafety = SET("Safety")
	SET SAFETY OFF
	
	WITH This
		bSourceTableIsOpen= USED(.sSourceTablename) 
		.UseTable(.sSourceTablename)
		SELECT (.sSourceTablename)
		
		.nInitRecs = RECCOUNT(.sSourceTableName)
		IF .nInitRecs > 0 THEN
			IF .nInitRecs > nMaxRowsForXLS THEN
				*El número de registros a exportar es de tal magnitud que se hace la exportacióon hacia un archivo CSV;
				que es leido en forma nativa por Excel 2007 o superior
				.sExportedFileType='CSV'
			ENDIF

			WAIT .sExportingMsg WINDOW NOWAIT
			IF ISNULL(.sExportedFileName ) THEN
				.sExportedFileName = .sSourceTableName 
			ENDIF
			
			IF ISNULL(.sFilter) THEN
				sCopyToCmd="COPY TO [" + This.sExportationPath + "\" + .sExportedFileName + "] TYPE " + .sExportedFileType
			ELSE
				sCopyToCmd="COPY TO [" + This.sExportationPath + "\" + .sExportedFileName + "] FOR " + .sFilter + " TYPE " + .sExportedFileType
			ENDIF
			&sCopyToCmd
			WAIT CLEAR
		ENDIF
		
	ENDWITH

	IF !bSourceTableIsOpen THEN
		SELECT (This.sSourceTablename)
		USE
	ENDIF
	SELECT (nSelectedWorkArea)
	SET SAFETY &sOldSafety

ENDPROC


PROCEDURE ShowXLSExportationResults

	LOCAL sResultsMessage as String
	
	#DEFINE sWindowTitle "Resultados de exportación"
	
	sResultsMessage= 'Número inicial de registros en la tabla: ' + ALLTRIM(STR(This.nInitRecs)) + CHR(13) 
	
	IF THIS.nInitRecs > 0 THEN
		sResultsMessage = sResultsMessage + CHR(13) +;
						  'Se ha generado un archivo en formato compatible con Excel 2007 en: ' ;
						  + CHR(13) + This.sExportationPath
	ENDIF
	
	MESSAGEBOX( sResultsMessage,  0 + 48 , sWindowTitle ) 
	This.sResultsMsg = sResultsMessage
ENDPROC


*Para ser usado solo cuando se integre el productor de datos para el Sistema Tuberculosis Web
*PROCEDURE exportToXML

	*Exporta los registros de This.sSourceTableName a un archivo con formato XLS o CSV, dependiendo de ;
	si el número de registros en This.sSourceTableName es menor o mayor que nMaxRowsForXLS.
	
	*El archivo exportado tendrá nombre This.sSourceTableName -a menos que se indique lo contrario en this.sExportedFileName- ;
	y quedará ubicado en This.sExportationPath. ;
	
*	LOCAL oExportationForm AS Object, nSelectedWorkArea AS Number

*	nSelectedWorkArea=SELECT()
	
*	DO FORM sivigilaToTargetSystem NAME oExportationForm NOSHOW
	
*	WITH sivigilaToTargetSystem 
*		.TxtPathToSIVIGILA.Value = SIVIGILA_DEFAULT_DIR
*		.StateCodeSivigilatxtbox.Value = "11"
*		.VISIBLE = .T.
*	ENDWITH
*	SELECT (nSelectedWorkArea)

*ENDPROC


HIDDEN FUNCTION UseTable

	LPARAMETERS sTableNameToUse as String
	
	*Opens sTableNameToUse in first available workarea without selecting it. ;
	Returns .T. if it was posible to open sTableNameToUse or it was already opened, otherwise returns .F.
	
	LOCAL bReturnedValue AS Boolean 
		
	bReturnedValue=.T.
	IF !USED(RIGHT(sTableNameToUse,LEN(sTableNameToUse)-RAT("\",sTableNameToUse))) THEN
		IF FILE(sTableNameToUse + ".DBF") THEN
			USE (sTableNameToUse) IN 0
		ELSE
			bReturnedValue=.F.
		ENDIF
	ENDIF
	RETURN bReturnedValue
ENDFUNC


ENDDEFINE
