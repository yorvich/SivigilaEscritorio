#INCLUDE GlobalConst.h
#INCLUDE SIVIGILA.h

#DEFINE confTableName "DD_vs_SivigilaFormsProperties"
#DEFINE SIVIGILAProjectPath "C:\Users\wilson.aguilar\Documents\wilsonaguilar\ProyectoSivigila\Desarrollo_2012_V1.1.0"


PROCEDURE addToVFPValidMethod(surveyId As Integer)

	#DEFINE sOldRemoveSpecialCharsMacroName "_car_esp"
	#DEFINE sNewRemoveSpecialCharsMacroName "&RemoveSpecialchars"
	
	ON ERROR DO ErrorHandler


	LOCAL targetFormName As String, targetForm As Object
	LOCAL sOldValidMethod As String, sNewValidMethod as String
	LOCAL nCtrlId as Integer
   
	*Abre las los nombres de controles de los formularios del estudio registrados en confTableName
	sSQLCmd = 'SELECT * FROM ' + confTableName + ' WHERE IdEstudio=' + ALLTRIM(STR(surveyId)) + ' INTO CURSOR confTable'
	&sSQLCmd
	
	SELECT confTable
	targetFormName = ALLTRIM(confTable.Nombre)
	
	*Verifica si el formulario del estudio surveyId existe en el proyecto
	IF FILE(SIVIGILAProjectPath + "\" + targetFormName + ".SCX") THEN
	  
		*Abre el formulario en modo diseño
		MODIFY FORM &targetFormName NOWAIT
		targetForm = Application.Objects(1).Objects(1)

		*Recorre los controles del formulario targetForm y agrega código al evento valid o valores a la propiedad applicableRules ;
		de tal forma que se eliminen caracteres especiales
		SCAN
			IF UPPER(ALLTRIM(confTable.ddTipo)) $ " TEXTO " THEN
				nCtrlId=confTable.CtrlId
				targetCtrl = targetForm.Controls(nCtrlId)
				IF confTable.Class = "Textbox" AND OCCURS('9',confTable.InputMask)<confTable.ddLongitud THEN
					*La máscara de entrada para el control nCtrlId no límita en forma suficiente los caracteres admisibles
					
					sOldValidMethod = targetCtrl.ReadMethod("Valid")
					IF !(sOldRemoveSpecialCharsMacroName $ sOldValidMethod ) THEN
						*El método [valid del control actulamente no elimina caracteres especiales introducidos por el usuario
						sNewValidMethod = sNewRemoveSpecialCharsMacroName + CHR(13) + CHR(10) + sOldValidMethod 
						targetCtrl.WriteMethod("Valid", sNewValidMethod )
						
						*Registra el cambio realizado al evento en la tabla de ratreo de cambios
						INSERT INTO SivigilaFormsPropertiesChangesTrace VALUES (surveyId, nCtrlId, "MethodValid", sOldValidMethod, sNewValidMethod, DATE())
					ENDIF
				ELSE
					IF confTable.Class = "Sivigilatxtboxes"  AND OCCURS('9',confTable.InputMask)<confTable.ddLongitud THEN
						*La máscara de entrada para el control nCtrlId no límita en forma suficiente los caracteres admisibles

						sOldApplicableRules = targetCtrl.ApplicableRules
						IF VARTYPE(sOldApplicableRules)="N" THEN
							sOldApplicableRules = ALLTRIM(STR(sOldApplicableRules))
						ENDIF
						IF !('5' $ sOldApplicableRules ) THEN
							*Las reglas aplicables al control no continen la que elimina caracteres especiales introducidos por el usuario;
							(regla 5)
							sNewApplicableRules = sOldApplicableRules + ',5' 
							targetCtrl.ApplicableRules = sNewApplicableRules
							
							*Registra el cambio realizado al evento en la tabla de ratreo de cambios
							INSERT INTO SivigilaFormsPropertiesChangesTrace VALUES (surveyId, nCtrlId, "ApplicableRulesProperty", sOldApplicableRules , sNewApplicableRules, DATE())
						ENDIF
						
					ENDIF
				ENDIF
			ENDIF
		ENDSCAN
	  	targetForm.SaveAs(targetFormName + "_" + ALLTRIM(CHRTRAN(DTOC(DATE()),'/','')))
	  	
	ENDIF
	ON ERROR
	
ENDPROC



PROCEDURE addToVFPValidMethodTst()

	SET SAFETY OFF
	IF !USED("FormulariosPorEstudio") THEN
		USE FormulariosPorEstudio IN 0
	ENDIF
	SELECT FormulariosPorEstudio
	SCAN 
		WAIT "Procesando " + FormulariosPorEstudio.Nombre WINDOW  NOWAIT
		DO addToVFPValidMethod WITH FormulariosPorEstudio.IdEstudio
	ENDSCAN
	WAIT CLEAR
	CLOSE ALL
	SET SAFETY ON
	
ENDPROC
   

PROCEDURE modifyVFPMethod(sRSMethodsToWrite As String, sExcludeSymbol as String)

	
	ON ERROR DO ErrorHandler


	LOCAL targetFormName As String, targetForm As Object
	LOCAL sOldValidMethod As String, sNewValidMethod as String
	LOCAL nCtrlId as Integer
	
	*Abre las descripciones de métodos o procedimientos a modificar
	sSQLSelectCmd = "SELECT * FROM " + sRSMethodsToWrite + " WHERE Activo=1 AND EnElAlcanc=1 AND ALLTRIM(Symbol) <> '" +;
					sExcludeSymbol + "' ORDER BY FileName, ProcName, Abstract INTO CURSOR rsTargetMethods"
	&sSQLSelectCmd
	
	IF _TALLY>0 THEN
		bBackupDone=.F.
		
		SELECT rsTargetMethods
		DO WHILE !EOF("rsTargetMethods")
			
			*Determina el tipo de objeto al que pertenece el método a modificar
			sObjectFileName=ALLTRIM(rsTargetMethods.Filename)
			targetFileName=LEFT(sObjectFileName,AT('.',sObjectFileName)-1)
			sMethodType=UPPER(RIGHT(sObjectFileName,3))

			*Verifica si el archivo del objeto existe en el proyecto
			IF FILE(SIVIGILAProjectPath + "\" + sObjectFileName) THEN
				
				DO CASE
					CASE sMethodType='SCX'
						*El método pertenece a un objeto Form
						
						*Abre el formulario en modo diseño
						MODIFY FORM (sObjectFileName) NOWAIT
						
						*Determina el índice del objeto tipo Formset contenedor del formulario sObjectFileName
						nObject=1
						bFormsContainerFound=.F.
						DO WHILE NOT bFormsContainerFound
							IF Application.Objects(nObject).name="Formset"
								bFormsContainerFound=.T.
							ELSE
								nObject=nObject+1
							ENDIF
						ENDDO
						targetForm = Application.Objects(nObject).Objects(1)

						*Saca un backup del fomulario a modificar
						IF !bBackupDone THEN
							targetForm.SaveAs(targetFileName + "_Backup" + ALLTRIM(CHRTRAN(DTOC(DATE()),'/','')))
							bBackupDone=.T.
						ENDIF

						*Establece los nombres del control del formulario y el evento del control que van a ser objeto de modificación
						sProcName=ALLTRIM(rsTargetMethods.ProcName)
						sTargetCtrlName=LEFT(sProcName,AT('.',sProcName)-1)
						sTargetEventName=RIGHT(sProcName,LEN(sProcName)-AT('.',sProcName))
						
						*Determina el índice correspondiente al control al que pertenece el método a modificar
						DIMENSION aFormControls(1)
						DO getFormControlsState WITH targetForm, aFormControls, .T. IN SivigilaUtilities
						nCtrlId=ASCAN(aFormControls,sTargetCtrlName,1,-1,1,15) 
						targetCtrl = targetForm.Controls(nCtrlId)
						
						*Efectua la modificación del método
						sOldValidMethod = targetCtrl.ReadMethod(sTargetEventName)
						IF ALLTRIM(rsTargetMethods.Abstract) $ sOldValidMethod  THEN
						
							*Establece las líneas de código nuevas que deben insertarse en el método a modificar
							sNewMethodLine1 = "IF goErrorlog.nErrNum = DATA_VALIDATION_ERROR" + sCrLf 
							sNewMethodLine2 = sTab + "goErrorlog.resetErrorLog()" + sCrLf 
							sNewMethodLine3 = sTab + "RETURN 0" + sCrLf 
							sNewMethodLine4 = "ENDIF" + sCrLf 
							nTabs=INT((rsTargetMethods.ColPos+1)/4)
							FOR nTab=1 TO nTabs
								sNewMethodLine1 = sTab + sNewMethodLine1
								sNewMethodLine2 = sTab + sNewMethodLine2
								sNewMethodLine3 = sTab + sNewMethodLine3
								sNewMethodLine4 = sTab + sNewMethodLine4
							NEXT nTab
							sNewMethodLines = sNewMethodLine1 + sNewMethodLine2 + sNewMethodLine3 + sNewMethodLine4
							sNewMethodLines=ALLTRIM(rsTargetMethods.Abstract) + sCrLf + sNewMethodLines
							
							*Reemplaza cada una de las línea de código rsTargetMethods.Abstract por la nuevas líneas de código
							sTargetCode=ALLTRIM(rsTargetMethods.Abstract)
							sNewValidMethod = STRTRAN(sOldValidMethod,sTargetCode, sNewMethodLines)
							targetCtrl.WriteMethod(sTargetEventName, sNewValidMethod )
							DO WHILE ALLTRIM(rsTargetMethods.FileName) = sObjectFileName AND ;
									 ALLTRIM(rsTargetMethods.ProcName) = sProcName AND ;
									 ALLTRIM(rsTargetMethods.Abstract) = sTargetCode
								SKIP
							ENDDO
							SKIP -1
						ENDIF
						
					CASE sMethodType='PRG'				
						*El método pertenece a un Programa
				ENDCASE
			ENDIF
			
			SELECT rsTargetMethods
			SKIP
			IF !EOF("rsTargetMethods") THEN
				IF sObjectFileName<>ALLTRIM(rsTargetMethods.Filename) THEN
					RELEASE WINDOWS "Form Designer"
					bBackupDone=.F.
				ENDIF
			ELSE
				RELEASE WINDOWS "Form Designer"			
			ENDIF
		ENDDO
	ENDIF
	ON ERROR
	
ENDPROC


PROCEDURE modifyVFPMethodTst()

	*SET STEP ON 
	DO modifyVFPMethod WITH "FIELD_VALIDATION_IMPACT","UPDATE"
	CLOSE ALL
	
ENDPROC
   
PROCEDURE setSIVIGILAStoreProcedures

	LPARAMETERS bErrorsFounded as Boolean
	
	#DEFINE sStoreProceduresPath ".\StoreProcedures\"
	#DEFINE S_STORE_PROCEDURES_FILENAME_PREFIX "Rules"
		
	#DEFINE sSIVIGILA_StoreProceduresFileName "SIVIGILAStoreProcedures.prg"
	#DEFINE sSIVIGILA_StoreProceduresPrefixFileName "BDSIVIGILAStoreProceduresPrefix.PRG"
	
	DIMENSION aFilesToBeConcat(1)
	

	sCurrentDefaultdir=FULLPATH('')
	CD sStoreProceduresPath 
	
	sStoreProcedureFilesSkeleton="*" + S_STORE_PROCEDURES_FILENAME_PREFIX + "*.PRG"
	bErrorsFounded=.F.
	IF ADIR(aFilesWithErrors, "*" + S_STORE_PROCEDURES_FILENAME_PREFIX + "*.ERR")=0 THEN
		
		nFilesToBeConcat = ADIR(aFilesToBeConcat, sStoreProcedureFilesSkeleton)
		DELETE FILE sSIVIGILA_StoreProceduresFileName 
		IF nFilesToBeConcat >0 THEN
			sRunCmd="RUN COPY " + sSIVIGILA_StoreProceduresPrefixFileName + " + " + "*" + S_STORE_PROCEDURES_FILENAME_PREFIX + "*.prg " + sSIVIGILA_StoreProceduresFileName 
			&sRunCmd	
		ENDIF	
	ELSE
		MESSAGEBOX("Se encontraron archivos de Store Procedures con errores de compilación. No se pudo efectuar la operación.",0+48+256)
		bErrorsFounded=.T.
	ENDIF
	
	SET DEFAULT TO &sCurrentDefaultdir
ENDPROC


PROCEDURE setSIVIGILAStoreProceduresTest
	*SET STEP ON 
	DO setSIVIGILAStoreProcedures
ENDPROC


PROCEDURE updateSIVIGILAStoreProcedures

	LOCAL bErrorsFounded as Boolean

	DO setSIVIGILAStoreProcedures WITH bErrorsFounded 
	IF !bErrorsFounded THEN
		OPEN DATABASE BDSivigila.DBC
		APPEND PROCEDURES FROM .\StoreProcedures\SIVIGILAStoreProcedures.prg OVERWRITE
		CLOSE ALL
	ENDIF	
ENDPROC


PROCEDURE resetSIVIGILARecordValidationRules

	SELECT DISTINCT DBF_NAME FROM SIVIGILADataTables INTO CURSOR rsTargetTables
	SELECT rsTargetTables
	SCAN
		sSQLAlterTableClause="ALTER TABLE " + ALLTRIM(rsTargetTables.DBF_NAME) + " DROP CHECK"
		TRY
			&sSQLAlterTableClause
		CATCH TO oException
		ENDTRY
	ENDSCAN
	CLOSE TABLES
	
ENDPROC


PROCEDURE applyVfpCmdToSIVIGILATables(sCmdToApply as String, sExceptions as String, sSourceFolder as String)

	*Ejecuta el comando o programa especificado en sCmdToApply  a cada una de las tablas del sistema Sivigila que se ;
	encuetren en Tables.SIVIGILADataTables excepto a las listadas en sExceptions -lista separada por comas-, si es que se pasa este parámetro opcional.;
	Si se pasa el parámetro opcional sSourceFolder, sCmdToApply  se aplicará a las tablas que se encuentren en esa carpeta.
	
	sOldDefault = SET("DIRECTORY")

	IF VARTYPE(sExceptions )='L' THEN
		sExceptions = ''
	ENDIF

	glDisableRules=.T.
	SELECT DISTINCT DBF_NAME FROM SIVIGILADataTables INTO CURSOR rsTargetTables

	IF VARTYPE(sSourceFolder)='C' AND !EMPTY(sSourceFolder) THEN
		SET DEFAULT TO "&sSourceFolder"
	ENDIF
*SET STEP ON 
	SELECT rsTargetTables
	SCAN
		sTargetTable=ALLTRIM(rsTargetTables.DBF_NAME)
		IF !(sTargetTable $ sExceptions )
			TRY
				USE (sTargetTable) IN 0
			CATCH TO oException
			ENDTRY

			TRY
				SELECT (sTargetTable)
				&sCmdToApply 
			CATCH TO oException
			ENDTRY
		ENDIF
	ENDSCAN
	CLOSE DATABASES ALL
	*CLOSE TABLES ALL 
	RELEASE glDisableRules=.T.
	SET DEFAULT TO &sOldDefault 

ENDPROC


PROCEDURE getSIVIGILATablesStruc

	sOldSafety=SET("Safety")
	SET SAFETY OFF
	USE SIVIGILATablesStruc EXCLUSIVE
	DELETE ALL
	PACK
	
	SELECT DISTINCT DBF_NAME FROM SIVIGILADataTables INTO CURSOR rsTargetTables
	SELECT rsTargetTables
	SCAN
		sTargetTable=ALLTRIM(rsTargetTables.DBF_NAME)
		USE (sTargetTable) IN 0
		SELECT (sTargetTable)
		COPY STRUCTURE EXTENDED TO CurrentTableStruc
		SELECT SIVIGILATablesStruc
		APPEND FROM CurrentTableStruc
		REPLACE TABLE_NAME WITH LOWER(sTargetTable) FOR EMPTY(TABLE_NAME) ALL
	ENDSCAN
	DELETE FILE CurrentTableStruc.DBF

	SELECT SIVIGILATablesStruc
	REPLACE TABLE_NAME WITH UPPER(TABLE_NAME) ALL

	CLOSE ALL
	SET SAFETY &sOldSafety
ENDPROC


PROCEDURE ErrorHandler
   
	MESSAGEBOX(MESSAGE(),0+16, "Error " + STR(ERROR()))
	ON ERROR
	CLOSE ALL
	CANCEL   
ENDPROC



PROCEDURE modifyVFPFormMethod(sRSTargetForms As String, sNewMethodTxtFileNameAndPath as String, bAddToExistent as Boolean)

	LOCAL targetFormName As String, targetForm As Object
	LOCAL sOldValidMethod As String, sNewValidMethod as String

	ON ERROR DO ErrorHandler
	
	*Abre los nombres de los formularios a modificar
	sSQLSelectCmd = "SELECT * FROM " + sRSTargetForms + " WHERE Activo=1 AND EnElAlcanc=1 AND UPPER(ClassName) = 'FORM' ORDER BY FileName, " +;
					"ProcName INTO CURSOR rsTargetForms"
	&sSQLSelectCmd
	
	IF _TALLY>0 THEN
		bBackupDone=.F.
		
		SELECT rsTargetForms
		DO WHILE !EOF("rsTargetForms")
			
			*Determina el tipo de objeto al que pertenece el método a modificar
			sObjectFileName=ALLTRIM(rsTargetForms.Filename)
			targetFileName=LEFT(sObjectFileName,AT('.',sObjectFileName)-1)
			sMethodType=UPPER(RIGHT(sObjectFileName,3))

			*Verifica si el archivo del objeto existe en el proyecto
			IF FILE(SIVIGILAProjectPath + "\" + sObjectFileName) THEN
				
				DO CASE
					CASE sMethodType='SCX'
						*El método pertenece a un objeto Form
						
						*Abre el formulario en modo diseño
						MODIFY FORM (sObjectFileName) NOWAIT
						
						*Determina el índice del objeto tipo Formset contenedor del formulario sObjectFileName
						nObject=1
						bFormsContainerFound=.F.
						DO WHILE NOT bFormsContainerFound
							IF Application.Objects(nObject).name="Formset"
								bFormsContainerFound=.T.
							ELSE
								nObject=nObject+1
							ENDIF
						ENDDO
						targetForm = Application.Objects(nObject).Objects(1)

						*Saca un backup del fomulario a modificar
						IF !bBackupDone THEN
							targetForm.SaveAs(targetFileName + "_Backup" + STRTRAN(TTOC(DATETIME(),3),":","_"))
							bBackupDone=.T.
						ENDIF

						*Establece el nombre del evento que va a ser objeto de modificación
						sProcName=ALLTRIM(rsTargetForms.ProcName)
						sTargetEventName=sProcName
						
						*Efectua la modificación del método
						sOldMethodText = targetForm.ReadMethod(sTargetEventName)
						sNewMethodText = ""
						DO txtFileToString WITH sNewMethodTxtFileNameAndPath, sNewMethodText 
						IF !EMPTY(sNewMethodText) THEN
							*Reemplaza el código del método 
							IF bAddToExistent THEN
								sNewMethodText = sOldMethodText + sNewMethodText 
							ENDIF
							targetForm.WriteMethod(sTargetEventName, sNewMethodText )
	
							UPDATE &sRSTargetForms SET OldCode = sOldMethodText, NewCode = sNewMethodText, DateChange = DATE()  ;
								WHERE Id = rsTargetForms.Id
						ENDIF
						
					CASE sMethodType='PRG'				
						*El método pertenece a un Programa
				ENDCASE
			ENDIF
			
			SELECT rsTargetForms
			SKIP
			RELEASE WINDOWS "Form Designer"			
		ENDDO
	ENDIF
	ON ERROR
	
ENDPROC


PROCEDURE modifyVFPFormMethodTst

	*SET STEP ON 
	DO modifyVFPFormMethod WITH 'formMethods','codeAddedToFormsForMinis.txt',.T.
	
ENDPROC


PROCEDURE txtFileToString

	LPARAMETERS txtFileNameAndPath AS String, sResulStr as String
	
	Local gnFileHandle,nSize,cString
	
	gnFileHandle = FOPEN(txtFileNameAndPath)
	
	* Determina el tamaño del archivo en bytes
	nSize =  FSEEK(gnFileHandle, 0, 2) 
	IF nSize <= 0
		*El archivo está vacío
		sResulStr = ""
	ELSE
		= FSEEK(gnFileHandle, 0, 0)      && Move pointer to BOF
		sResulStr = FREAD(gnFileHandle, nSize)
	ENDIF
	= FCLOSE(gnFileHandle) 
ENDPROC


PROCEDURE adhocAlterSIVIGILATables

	LPARAMETERS sExceptions as String

	IF VARTYPE(sExceptions )='L' THEN
		sExceptions = ''
	ENDIF

	SELECT DISTINCT DBF_NAME FROM SIVIGILADataTables WHERE DBF_NAME LIKE 'EVENTOS%' AND DBF_NAME != 'EVENTOS_BUS_ACT' INTO CURSOR rsTargetTables
	SELECT rsTargetTables
	SCAN
		sTargetTable=ALLTRIM(rsTargetTables.DBF_NAME)
		IF !(sTargetTable $ sExceptions )
			*sCMD = 'ALTER TABLE ' + sTargetTable + ' ALTER COLUMN est_notif n(1) CHECK defaultvaluefor_eventos_xx_est_notif("' + RIGHT(sTargetTable,2) + '")'
			sCMD = 'ALTER TABLE ' + sTargetTable + ' ALTER COLUMN est_notif n(1) DEFAULT 2'
			&sCMD
		ENDIF
	ENDSCAN
	CLOSE TABLES
	
ENDPROC


PROCEDURE updateXLSOutputSpec(sCmdToApply as String, sNewVersion as String, bOnlyUpdate as Boolean, sEventScope as String)

	*Actualiza en la tabla exportQueries los queries de exportación para aquellos eventos que usen un query ad-hoc. La actualización aplica ;
	sCmdToApply a cada registro que en la tabla exportQueries tenga un query de exportación configurado. 
	
	*Si bOnlyUpdate es .F. la actualización procede asi: crea un nuevo registro igual al más fresco que exista en la tabla exportQueries, ;
	aplica sCmdToApply al campo ExcelQuery y almacena sNewVersion en los campos Major,Minor,Build. ;
	Si bOnlyUpdate es .T. la actualización procede asi: ubica el registro más fresco que exista en la tabla exportQueries, ;
	y aplica sCmdToApply al campo ExcelQuery
	
	*Si se pasa el parámetro opcional sEventScope, solamente se tendrán en cuenta los eventos así según el valor del parámetro: ;
		'1': Individuales ;
		'3': Colectivos 
	
	SET DELETED ON
*SET STEP ON 	
	IF ALINES(aNewVersion,sNewVersion,15,'.')>0 THEN 
		sMajor = aNewVersion[1]
		sMinor = aNewVersion[2]
		sBuild = aNewVersion[3]
	ENDIF 
	
	IF EMPTY(sEventScope) THEN 
		SELECT *, RECNO() as recId FROM exportQueries WHERE !EMPTY(ExcelQuery) ORDER BY ShortName INTO CURSOR rsTargets
	ELSE
		DO CASE 
			CASE sEventScope = '1'
				*Solo se debe actualizar eventos individuales
				DO listaEventosIndividualesSeguimientos.qpr
				SELECT COD_EVE, 'E' + PADL(ALLTRIM(FORMULARIO),2,'0') AS ShortName FROM EVENTOS WHERE TIP_NOT=&sEventScope INTO CURSOR rsTargetEvents NOFILTER
				SELECT *, RECNO() as recId FROM exportQueries WHERE !EMPTY(ExcelQuery) AND ShortName in (SELECT ShortName FROM rsTargetEvents) ORDER BY ShortName INTO CURSOR rsTargets
				
			CASE sEventScope = '3'
				*Solo se debe actualizar eventos colectivos
		ENDCASE
	ENDIF
	
	DO WHILE !EOF()
		bContinue = .T.
		sCurrentShortName = rsTargets.ShortName
		DO WHILE bContinue
			IF rsTargets.ShortName = sCurrentShortName
				SKIP
			ELSE
				bContinue = .F.
			ENDIF
		ENDDO
		SKIP -1
		IF !bOnlyUpdate THEN 
			sSQLCmd = 'INSERT INTO ExportQueries SELECT * FROM ExportQueries WHERE RECNO()=' +  ALLTRIM(STR(rsTargets.recId))
			&sSQLCmd 
		ENDIF
				
		SELECT ExportQueries 
		IF !bOnlyUpdate THEN 
			GO BOTTOM
		ELSE
			GO  (rsTargets.RecID)
		ENDIF 
		&sCmdToApply 
		IF !bOnlyUpdate THEN 
			REPLACE Major WITH VAL(sMajor), Minor WITH VAL(sMinor), Build WITH VAL(sBuild) IN ExportQueries 
		ENDIF 
		
		SELECT rsTargets
		SKIP
	ENDDO
	SET DELETED OFF
	
	CLOSE TABLES
	
ENDPROC


PROCEDURE getInOutQueries 
	*Construye un recordset de nombre rsExportQueries "casi identico" a la tabla ExportQueries pero con la diferencia de que adiciona una columna ;
	de nombre CurrentQuery, cuyos valor indican si la fila i-ésima es la que actualmente se usa en la última versión del Sivigila para producir ;
	archivos de recepción/exportación.
	
	LOCAL sTableShortName AS String
	LOCAL sQryFilePtr as Byte
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	SET PROCEDURE TO PlainsLib ADDITIVE

	SET DELETED ON
	
	=SelectTable('EVENTOS','COD',.F.)
	
	*Construye el recordset de nombre rsExportQueries
	SELECT *, 0 AS CurrentQuery, RECNO() as nRec FROM ExportQueries WHERE Activo INTO CURSOR rsExportQueries READWRITE 
	
	*Marca las filas de producción de archivos utilizadas en eventos individuales
	DO listaEventosIndividualesSeguimientos.qpx
	SET RELATION TO COD_EVE INTO EVENTOS
	SCAN
		sTableShortName = 'E' + PADL(ALLTRIM(EVENTOS.FORMULARIO),2,'0')
		sCurrentEventCode = EVENTOS.COD_EVE
		sQryFilePtr = getInOutQueryFile(sTableShortName,nCurrentMajor_Internal,nCurrentMinor_Internal,nCurrentBuild_Internal)		
		SELECT rsExportQueries
		LOCATE FOR nRec = sQryFilePtr
		REPLACE CurrentQuery WITH 1
	ENDSCAN
	SET RELATION TO 
	sTableShortName = 'PTE'
	sQryFilePtr = getInOutQueryFile(sTableShortName,nCurrentMajor_Internal,nCurrentMinor_Internal,nCurrentBuild_Internal)		
	SELECT rsExportQueries
	LOCATE FOR nRec = sQryFilePtr
	REPLACE CurrentQuery WITH 1

	*Marca las filas de producción de archivos utilizadas en eventos colectivos
	SELECT COD_EVE,NOM_EVE FROM EVENTOS WHERE ACTIVO AND (Eventos.tip_not = 2 OR Eventos.tip_not = 3) AND (date()<Eventos.FechaInactivacion ;
		OR EMPTY(Eventos.FechaInactivacion)) INTO CURSOR rsCollectiveEvents
	SET RELATION TO COD_EVE INTO EVENTOS
	SCAN
		IF !EMPTY(EVENTOS.FORMULARIO)
			sTableShortName = 'E' + PADL(ALLTRIM(EVENTOS.FORMULARIO),2,'0')
			sCurrentEventCode = EVENTOS.COD_EVE
			sQryFilePtr = getInOutQueryFile(sTableShortName,nCurrentMajor_Internal,nCurrentMinor_Internal,nCurrentBuild_Internal)		
			SELECT rsExportQueries
			LOCATE FOR nRec = sQryFilePtr
			REPLACE CurrentQuery WITH 1
		ENDIF
	ENDSCAN
	SET RELATION TO
	sTableShortName = 'BRO'
	sQryFilePtr = getInOutQueryFile(sTableShortName,nCurrentMajor_Internal,nCurrentMinor_Internal,nCurrentBuild_Internal)		
	SELECT rsExportQueries
	LOCATE FOR nRec = sQryFilePtr
	REPLACE CurrentQuery WITH 1

	*Marca las filas de producción de archivos utilizadas en eventos que no obedecen el esquema DB + DC
	DO EventosConTablaDatosIndependiente.qpx
	SET RELATION TO COD_EVE INTO EVENTOS
	SCAN
		sTableShortName = 'E' + PADL(ALLTRIM(EVENTOS.FORMULARIO),2,'0')
		sCurrentEventCode = EVENTOS.COD_EVE
		sQryFilePtr = getInOutQueryFile(sTableShortName,nCurrentMajor_Internal,nCurrentMinor_Internal,nCurrentBuild_Internal)		
		SELECT rsExportQueries
		LOCATE FOR nRec = sQryFilePtr
		REPLACE CurrentQuery WITH 1
	ENDSCAN
	SET RELATION TO
	
	*Marca las filas de producción de archivos utilizadas en Laboratorios
	sTableShortName = 'LAB'
	sQryFilePtr = getInOutQueryFile(sTableShortName,nCurrentMajor_Internal,nCurrentMinor_Internal,nCurrentBuild_Internal)		
	SELECT rsExportQueries
	LOCATE FOR nRec = sQryFilePtr
	REPLACE CurrentQuery WITH 1
	
	SET DELETED OFF
ENDPROC


FUNCTION getInOutQueryFile(_Tname as string, nMajor as Byte, nMinor as Byte, nBuild as Byte)

	*Retorna el número de registro en la tabla ExportQueries que contiene la información de recepción/exportación/producción de archivos ;
	del sistema Sivigla utilizados por la versión dada por nMajor.nMinor.nBuild
	
	LOCAL sDescriptorPtr as byte
	sDescriptorPtr = 0
	
	SELECT *,RECNO() as nRec FROM ExportQueries WHERE Activo AND shortname = _Tname AND Major <= nMajor ORDER BY  Major DESC, Minor DESC, Build DESC INTO CURSOR rsCandidates1
	SELECT rsCandidates1
	bContinue = .T.
	SCAN WHILE bContinue
		SET PROCEDURE TO SivigilaSystem ADDITIVE
		IF compareVersions(ALLTRIM(TRANSFORM(rsCandidates1.Major)) + '.' + ALLTRIM(TRANSFORM(rsCandidates1.Minor)) + '.' + ALLTRIM(TRANSFORM(rsCandidates1.Build)), ;
							ALLTRIM(TRANSFORM(nMajor)) + '.' + ALLTRIM(TRANSFORM(nMinor)) + '.' + ALLTRIM(TRANSFORM(nBuild))) = -1 THEN
			bContinue = .T.
		ELSE
			bContinue = .F.
			sDescriptorPtr = rsCandidates1.nRec 
		ENDIF
	ENDSCAN
	RETURN sDescriptorPtr 
ENDPROC

PROCEDURE makeTSQLExtractStatementsTst()
*SET STEP ON 	
	DO makeTSQLExtractStatements WITH 'JsonEvento', 'dbo.DetallesEventos'
ENDPROC 


PROCEDURE makeTSQLExtractStatements(JsonColumnName as string, sTableFromName as String, sCastingTo as string)

	*Produce un archivo texto de instrucciones T-SQL de Sql Server para cada una de las tablas de datos complementarios ;
	del sivigila, de tal forma que se puedan traducir campos en formato JSON a tablas.
	
	*	JsonColumnName (in): nombre de la columna que contiene los datos complementarios en formato JSON;
		sTableFromName (in): nombre de la tabla que contiene los datos complementarios. Debe especificarse además el esquema;
							 al que pertenence la tabla; por ejemplo, tmp.D o dbo.DetallesEventos
							 
	*El archivo texto de resultados quedará con el nombre TSQLJson_ + sTableFromName y contendrá para cada evento, instrucciones;
	SQL de la forma ;
		Select json_value(JsonEvento,'$.COD_EVE') as COD_EVE,json_value(JsonEvento,'$.TRONCO') as Tronco,...JsonEvento ... ;
	que se pueden ejecutar en el servidor en donde se encuente la BD del Sistema Sivigla 4.0
	
	LOCAL TableLongName as String
	DIMENSION aTargetEvents(1)
	
	SET PROCEDURE TO Utilities ADDITIVE 
	
	nResultsFileHandler=FCREATE('TSQLJson_' + sTableFromName + '.txt')
	
	*Establece qué queries de exportación está utilizando el sistema Sivigla escritorio para cada evento
	DO getInOutQueries
	SELECT rsExportQueries 
	
	SCAN FOR rsExportQueries.CurrentQuery=1
		*Determina qué eventos se almacena en la tabla identificada por rsExportQueries.ShortName
		sEventTableNumber = ALLTRIM(RIGHT(rsExportQueries.ShortName,3))
		
		IF isNumeric(sEventTableNumber) THEN 
		
			*Construye el filtro que debe utilizarse para cada evento en la instrucción TSQL resultado
			SELECT COD_EVE FROM Eventos WHERE VAL(FORMULARIO)=&sEventTableNumber INTO ARRAY aTargetEvents
			sFilter = 'LIKE ' + "'" + "%" + '"' + ALLTRIM(aTargetEvents[1]) + '"' + "%'"
			
			*Serializa los campos del query de exportación que está utilizando el sistema Sivigila escritorio
			SQLCmd = GetQryString(rsExportQueries.ShortName, @TableLongName, rsExportQueries.Major, rsExportQueries.Minor, rsExportQueries.Build)
			sFieldList = STRTRAN(SQLCmd, 'recordSource.', '', -1, -1, 1)
			sFieldList = STRTRAN(sFieldList, 'SELECT ')
			sFieldList = STRTRAN(sFieldList, ';')
			sFieldList = SUBSTR(sFieldList,1,AT('FROM',sFieldList) -1 )
			sFieldList = UPPER(sFieldList)
			
			*Construye la instrucción TSQL resultado utilizando una subconsulta que convierte a mayúscula el campo JsonColumnName 
			sSubquerySQLCmd = "(SELECT *,UPPER(" + JsonColumnName  + ") AS U" + JsonColumnName + " FROM " + sTableFromName +;
								" WHERE " + JsonColumnName + " " + sFilter + ") AS T"
			SQLCmd = "SELECT T.*,"

			nFields = ALINES(aFieldList,sFieldList,15,',')
			FOR iField=1 TO nFields
				IF EMPTY(sCastingTo) THEN  
					SQLCmd = SQLCmd + "JSON_VALUE(U" + JsonColumnName + ",'$." + aFieldList[iField] + "') AS " + aFieldList[iField] + ','
				ELSE
					SQLCmd = SQLCmd + "CAST(JSON_VALUE(U" + JsonColumnName + ",'$." + aFieldList[iField] + "') AS " + sCastingTo + ") AS " + aFieldList[iField] + ','
				ENDIF 
			NEXT iField
			SQLCmd = SUBSTR(SQLCmd,1,LEN(SQLCmd)-1)
			SQLCmd = SQLCmd + " FROM " + sSubquerySQLCmd 
			
			*Convierte en la instrucción TSQL resultado, caracteres que no aplican
			SQLCmd = STRTRAN(SQLCmd, 'Ñ','N')
			SQLCmd = STRTRAN(SQLCmd, 'Á','A')
			SQLCmd = STRTRAN(SQLCmd, 'É','E')
			SQLCmd = STRTRAN(SQLCmd, 'Ó','I')
			SQLCmd = STRTRAN(SQLCmd, 'Ú','O')
			SQLCmd = STRTRAN(SQLCmd, 'Í','U')
			
			*Almacena la instrucción TSQL recien construida en el archivo de resultados
			sResultMsg = '-- ' + rsExportQueries.ShortName + ' ' + sFilter 
			=FPUTS(nResultsFileHandler,sResultMsg)
			=FPUTS(nResultsFileHandler,SQLCmd)
			=FPUTS(nResultsFileHandler,CHR(13)+CHR(10)+CHR(13)+CHR(10))
		ENDIF 
	ENDSCAN
	=FCLOSE(nResultsFileHandler)
ENDPROC


PROCEDURE makeInOutQueriesFile()
	
	LOCAL sInOutQry as String, sEvents as String 
	sInOutQry = ''
	sEvents = ''
	
	SET PROCEDURE TO EventsHandler ADDITIVE 
	SET PROCEDURE TO Utilities ADDITIVE 
	
	DO getInOutQueries 

	nResultsFileHandler=FCREATE('Sivigila_Queries_In_out.txt')

	SELECT rsExportQueries 
	SCAN FOR CurrentQuery=1
		sInOutQry = FILETOSTR(rsExportQueries.Qryfile)
		IF isNumeric(RIGHT(ALLTRIM(rsExportQueries.ShortName),2)) THEN 
			sEvents = 'Eventos(s): ' + GetEventsStoredInDCTable(VAL(RIGHT(ALLTRIM(rsExportQueries.ShortName),2)))
		ELSE
			sEvents = 'Eventos(s): ' + rsExportQueries.Tablename
		ENDIF 
		=FPUTS(nResultsFileHandler,sEvents)
		=FPUTS(nResultsFileHandler,CHR(13)+CHR(10)+CHR(13)+CHR(10))
		=FPUTS(nResultsFileHandler,sInOutQry)
		=FPUTS(nResultsFileHandler,CHR(13)+CHR(10)+CHR(13)+CHR(10))
	ENDSCAN
	=FCLOSE(nResultsFileHandler)
ENDPROC 

PROCEDURE makeTSQLExtractStatementsForSectores(sTableFromName as String, sTableToName as String, sDefaultValueExpr as String, sFilter as string, ;
												sCreatorId as String )

	*Produce un archivo texto de instrucciones T-SQL de Sql Server para registros de la tabla sTableFromName que cumplan la condición ;
	sFilter y permitan su inserción en la tabla sTableToName del sivigila 4.0. sDefaultValueExpr es una expresión que se usa para asignar ;
	valores por defecto en caso de que sea necesario.
	
	*El archivo texto de resultados quedará con el nombre TSQL_ + sTableFromName y contendrá para cada registro, instrucciones;
	INSERT del lenguaje TSQL que se pueden ejecutar en el servidor en donde se encuente la BD del Sistema Sivigila 4.0
	
	LOCAL SQLCmd as String
	SQLCmd = ''

	SELECT * FROM &sTableFromName WHERE &sFilter INTO CURSOR rsSource 
	IF _tally >0 THEN 

		nResultsFileHandler=FCREATE('TSQL_' + sTableFromName + '.sql')

		SELECT rsSource
		SCAN 
			SQLCmd = "INSERT INTO " + sTableToName + ;
						"(aliasLista, idListaPadre, estado, creadoPor, fechaCreacion, actualizadoPor, fechaActualizacion, codigoItem,"  + ;
						" descripcionLista, codigoItem1, codigoItem2, codigoItem3, codigoItem4, codigoItem5  )"  + ;
						" VALUES ('" + ;
						"Sectores" + "', "  + ;
						sDefaultValueExpr + ", "  + ;
						sDefaultValueExpr + ", "  + ;
						"'" + sCreatorId + "', "  + ;
						"getdate()" + ", "  + ;
						sDefaultValueExpr + ", "  + ;
						sDefaultValueExpr + ", "  + ;
						"'" + COD_SEC + "', "  + ;
						"'" + ALLTRIM(NOM_SEC) + "', "  + ;
						"'" + COD_DEP + "', "  + ;
						"'" + COD_MUN + "', "   + ;
						sDefaultValueExpr+ ", "   + ;
						sDefaultValueExpr+ ", "   + ;
						sDefaultValueExpr+ ")"  
						
			*Almacena la instrucción TSQL recien construida en el archivo de resultados
			=FPUTS(nResultsFileHandler,SQLCmd)
			=FPUTS(nResultsFileHandler,CHR(13)+CHR(10)+CHR(13)+CHR(10))
		ENDSCAN
		
		=FFLUSH(nResultsFileHandler)
		=FCLOSE(nResultsFileHandler)
		
	ENDIF
	USE IN rsSource
ENDPROC
