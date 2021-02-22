#INCLUDE GlobalConst.H
#INCLUDE DataExporter.H

DEFINE CLASS DataExporter as Custom

sSourceTableName=.NULL.
sExportationPath = '.'
sExportedFileName = .NULL.
sExportedFileType = "XL5"
sExportingMsg = "Exportando a excel..."
sExcludeFields = .NULL.
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
	LPARAMETERS bGenerateEmptyFilesToo

	*Exporta los registros de This.sSourceTableName a un archivo con formato XLS o CSV, dependiendo de ;
	si el número de registros en This.sSourceTableName es menor o mayor que nMaxRowsForXLS.
	
	*El archivo exportado tendrá nombre This.sSourceTableName -a menos que se indique lo contrario en this.sExportedFileName- ;
	y quedará ubicado en This.sExportationPath. ;
	
	* bGenerateEmptyFilesToo en .T. ocasiona que sean creados los archivos XLS así no tengan registros.
	
	nSelectedWorkArea=SELECT()

	WITH This
		bSourceTableIsOpen= USED(.sSourceTablename) 
		.UseTable(.sSourceTablename)
		SELECT (.sSourceTablename)
		
		.nInitRecs = RECCOUNT(.sSourceTableName)
		IF .nInitRecs > 0 OR bGenerateEmptyFilesToo THEN
			IF .nInitRecs > nMaxRowsForXLS THEN
				*El número de registros a exportar es de tal magnitud que se hace la exportacióon hacia un archivo CSV;
				que es leido en forma nativa por Excel 2007 o superior
				.sExportedFileType='CSV'
			ENDIF

			WAIT .sExportingMsg WINDOW NOWAIT
			IF ISNULL(.sExportedFileName ) THEN
				.sExportedFileName = .sSourceTableName 
			ENDIF
			sCopyToCmd="COPY TO '" + This.sExportationPath + "\" + .sExportedFileName + "' TYPE " + .sExportedFileType
			IF !ISNULL(This.sExcludeFields) THEN
				sCopyToCmd = sCopyToCmd + ' FIELDS EXCEPT ' + This.sExcludeFields
			ENDIF
			*&sCopyToCmd
			TRY
				&sCopyToCmd
			CATCH TO oException
				IF oException.ErrorNo=1426 THEN
					sResultMsg="Se produjo el siguiente problema al intentar crear el archivo XLS: " +  CHR(13) +  CHR(10) +;
								"El archivo " + .sExportedFileName + " se encuentra abierto por otro aplicativo o hay insuficiencia de recursos"
				ELSE
					sResultMsg="Se produjo el siguiente problema al intentar crear el archivo XLS: " +  CHR(13) +  CHR(10) +;
								"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
								"Instrucción que produjo el problema : " + oException.Procedure + " - " + oException.LineContents + ;
									" - Línea " + ALLTRIM(STR(oException.LineNo)) +CHR(13) +;
								"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
								"Detalles del problema : " + oException.Details + CHR(13) +;
								"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) 
				ENDIF
				MESSAGEBOX( sResultMsg, 0 + 48 + 256, 'Error de exportación', 5)
			ENDTRY
			WAIT CLEAR
		ENDIF
		
	ENDWITH

	IF !bSourceTableIsOpen THEN
		SELECT (This.sSourceTablename)
		USE
	ENDIF
	SELECT (nSelectedWorkArea)


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


PROCEDURE exportToXML(nRecId as Number, bGenerateEmptyFilesToo as Boolean)

	*Exporta los registros de This.sSourceTableName hacia un archivo XML con esquema que quedarán ubicados;
	en This.sExportationPath. Los nombres de los archivos resultantes serán los indicados por This.sExportedFileName

	* nRecId es un parámetro opcional que permite exportar de This.sSourceTableName tan solo el registro que ;
	cumple la condición RECNO()= nRecId 
	
	* bGenerateEmptyFilesToo en .T. ocasiona que sean creados los archivos así no tengan registros.

	LOCAL oXMLAdapter AS XMLAdapter
	LOCAL sXSDOutputFileName as String, sXMLOutputFileName as String
	
	sXSDOutputFileName =  ADDBS(This.sExportationPath) + This.sExportedFileName + ".xsd"
	sXMLOutputFileName =  ADDBS(This.sExportationPath) + This.sExportedFileName + ".xml"
	
	oXMLAdapter = CREATEOBJECT([XMLAdapter])

	WITH oXMLAdapter
		.XMLSchemaLocation = sXSDOutputFileName
		
		*.XMLNamespace = STRCONV("http://tempuri.org/SITB.xsd",12)
		
		sSQLCmd = 'SELECT A.* FROM (SELECT *,RECNO() as nReg FROM ' + This.sSourceTableName + ') as A '
		IF VARTYPE(nRecId)='N' THEN
			sSQLCmd = sSQLCmd + ' WHERE nReg=' + STR(nRecId) 
		ENDIF
		sSQLCmd = sSQLCmd + ' INTO CURSOR rsRecsToExport'
		&sSQLCmd 
		This.nInitRecs = RECCOUNT("rsRecsToExport")
		IF This.nInitRecs > 0 OR bGenerateEmptyFilesToo THEN		
			.AddTableSchema('rsRecsToExport')

			.ToXML(sXMLOutputFileName,'',.T.)
			sXMLStream = FILETOSTR(sXMLOutputFileName)
			sXMLStream = STRTRAN(sXMLStream,This.sExportationPath,'',-1,-1,1) 
			=STRTOFILE(sXMLStream,sXMLOutputFileName)

		ENDIF
	ENDWITH
	RELEASE oXMLAdapter 
ENDPROC
 
PROCEDURE exportToDBF(bBeCummulative as Boolean )

	*Exporta los registros de This.sSourceTableName a un archivo con formato DBF (VFP 9.0).
	
	*Se supone que el archivo hacia donde hay que exportar, existe y tiene por nombre nombre This.sSourceTableName -a menos que se indique lo contrario ;
	en this.sExportedFileName- y se supone que está ubicado en This.sExportationPath. ;
	
	* bGenerateEmptyFilesToo en .T. ocasiona que sean creados los archivos DBF así no tengan registros.
	
	LOCAL bContinue as Boolean, bSourceTableIsOpen as Boolean 
	
	nSelectedWorkArea=SELECT(0)

	WITH This
		bSourceTableIsOpen= USED(.sSourceTablename) 
		.UseTable(.sSourceTablename)
		SELECT (.sSourceTablename)
		
		.nInitRecs = RECCOUNT(.sSourceTableName)
		IF .nInitRecs > 0 OR bGenerateEmptyFilesToo THEN

			WAIT .sExportingMsg WINDOW NOWAIT
			IF ISNULL(.sExportedFileName ) THEN
				.sExportedFileName = .sSourceTableName 
			ENDIF
			sCopyToCmd="COPY TO [" + This.sExportationPath + "\" + .sExportedFileName + "] WITH CDX"
			IF !ISNULL(This.sExcludeFields) THEN
				sCopyToCmd = sCopyToCmd + ' FIELDS EXCEPT ' + This.sExcludeFields
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
ENDPROC

ENDDEFINE
