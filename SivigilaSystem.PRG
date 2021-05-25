#INCLUDE SIVIGILA.H


DEFINE CLASS Launcher as Session 

	sMySession = .NULL.
	sLauncherDir = '.'
	
	FUNCTION Init

		SET NEAR ON
		SET TALK OFF
		SET SAFETY OFF
		SET LOCK OFF
		SET DATE TO DMY
		SET CENTURY ON
		SET EXCLUSIVE OFF
		SET AUTOSAVE ON
		SET DELETED ON
		SET EXACT OFF
		SET SECONDS OFF
		SET MULTILOCKS OFF

		SET STATUS BAR OFF
		SET CLOCK STATUS
		SET ESCAPE OFF
		SET BELL OFF
		SET OPTIMIZE ON
		SET READBORDER OFF
		SET REFRESH TO 1, 1
		SET ENGINEBEHAVIOR 70

		This.sMySession = ALLTRIM(STR(SET("Datasession")))
		ON SHUTDOWN QUIT
		
		This.sLauncherDir = ADDBS(SYS(2023)) + '\SivigilaLauncher\'
		IF  .NOT. DIRECTORY(This.sLauncherDir)
			MD (This.sLauncherDir)
		ENDIF
		
	ENDFUNC


	FUNCTION Destroy
	ENDFUNC

ENDDEFINE 

***********************************************************************************************************************************
PROCEDURE checkSivigilaSystemTables

	LPARAMETERS nSivigilaEvents
	
	LOCAL lcOldOnError
	lcOldOnError = ON("ERROR") && Save default error handler.

	LOCAL bContinue as Boolean 
	bContinue = .T.
	
	CLOSE ALL

	* Call ON ERROR with an error procedure. 
	ON ERROR DO ErrHandler WITH ERROR(), MESSAGE(), sTableToTest, lcOldOnError

	vExclusiveState=SET("Exclusive")
	SET EXCLUSIVE OFF

	sTableToTest='articulos'
	bContinue = CheckTable(sTableToTest)
	IF bContinue THEN
		sTableToTest='ocupaciones'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='deptos'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='municipios'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='eventos'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='calendario'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='aseguradoras'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='cie10'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='pruebas'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='SECTORES'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='agentes'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='agentes_etas'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='NOTIFICADOS'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='UPGD_HAB'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='FM_GRADOS'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='FM_UNIDADES'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF
	
	IF bContinue THEN		
		sTableToTest='paises'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='eventos_bus_act'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='general'
		bContinue = CheckTable(sTableToTest, .T.)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='upgd'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='tal_hum'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF
	
	IF bContinue THEN		
		sTableToTest='BROTES'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	sTableToTest='laboratorios'
	TRY
		USE (sTableToTest) ORDER 1 EXCLUSIVE
		USE
	CATCH TO oException
	ENDTRY

	IF bContinue THEN		
		sTableToTest='paciente'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		FOR nEvent=1 TO nSivigilaEvents
			sTableToTest='EVENTOS_' + PADL(nEvent,2,'0')
			IF bContinue THEN		
				bContinue = CheckTable(sTableToTest)
			ELSE
				EXIT
			ENDIF
		NEXT
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='entra_01'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='entra_02'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	IF bContinue THEN		
		sTableToTest='entra_03'
		bContinue = CheckTable(sTableToTest)
	ELSE
		RETURN
	ENDIF

	SET EXCLUSIVE &vExclusiveState
	
	* Reset original error handler.
	ON ERROR &lcOldOnError
ENDPROC

PROCEDURE ErrHandler

	PARAMETERS nErrNumber, sErrMessage, sObjectName, defaultErrorHandler

	ON ERROR &defaultErrorHandler
	DO CASE
    	CASE nErrNumber = 26
			MESSAGEBOX( 'Se ha detectado la falta del archivo CDX para la tabla ' + UPPER(sObjectName) + CHR(13) +;
						'Se procederá a hacer una reindexación completa del Sistema.', 0 + 48 + 256, ;
						'Problema de integridad SIVIGILA' )
			DO updateSIVIGILATableIndexes IN sivigilaIndexesHandler

    	CASE nErrNumber = 114
			MESSAGEBOX( 'Se ha detectado un problema con el archivo CDX para la tabla ' + UPPER(sObjectName) + CHR(13) +;
						'Se procederá a hacer una reindexación completa del Sistema.', 0 + 48 + 256, ;
						'Problema de integridad SIVIGILA' )
			DO updateSIVIGILATableIndexes IN sivigilaIndexesHandler

    	CASE nErrNumber = 2091
			sErrorMsg=	"Se ha producido un error grave en el archivo DBF para la tabla " + UPPER(sObjectName) + CHR(13) +;
						"Código interno de error: " + ALLTRIM(STR(nErrNumber)) + CHR(13) +;
						"Mensaje de error: "  + (sErrMessage) + CHR(13) +;
						"Posible objeto del error: "  + sObjectName + CHR(13) + CHR(13) +;
						"Debe usted volver a instalar el Sistema Sivigila y Migrar " + CHR(13) +;
						"la información desde el último Backup que tenga a disposición." + CHR(13) + CHR(13) +;
						"El sistema se cerrará."
			MESSAGEBOX(sErrorMsg , 0 + 48 + 256, 'Error desconocido - SIVIGILA' )
			QUIT

		OTHERWISE
			sErrorMsg=	"Se ha producido un error inesperado en el Sistema." + CHR(13) + CHR(13) +;
						"Código interno de error: " + ALLTRIM(STR(nErrNumber)) + CHR(13) +;
						"Mensaje de error: "  + (sErrMessage) + CHR(13) +;
						"Posible objeto del error: "  + sObjectName + CHR(13) + CHR(13) +;
						"Tome una foto de esta pantalla y remítala al administrador del Sistema." + CHR(13) +;
						"El sistema se cerrará."
			MESSAGEBOX(sErrorMsg , 0 + 48 + 256, 'Error desconocido - SIVIGILA' )
			QUIT
	ENDCASE
	
ENDPROC


PROCEDURE restartSIVIGILA

	LOCAL sSIVIGILAExeFileNameAndPath as String
	        
    DECLARE INTEGER ShellExecute IN SHELL32.DLL ;
        INTEGER handle,;
        STRING @sFile,;
        STRING @lp,;
        STRING @DIR,;
        STRING @dir1,;
        INTEGER ncmd

	sSIVIGILAExeFileNameAndPath=SYS(16,0)
	nReturnedValue = ShellExecute(0, "Open", sSIVIGILAExeFileNameAndPath,"", "", 1)
    CLEAR DLLS ShellExecute
ENDPROC


FUNCTION compareVersions
	LPARAMETERS sLocalVersion as String, sForeignVersion as String

	LOCAL ARRAY aLocalVersion(1,3)
	LOCAL ARRAY aForeignVersion(1,3)
	LOCAL iNMajorLocalVersion as Integer, iNMinorLocalVersion as integer, iNBuildLocalVersion as integer
	LOCAL iNMajorForeignVersion as Integer, iNMinorForeignVersion as integer, iNBuildForeignVersion as integer

	iReturnedValueForLess = -1
	iReturnedValueForEqual = 0
	iReturnedValueForBig = 1
	iReturnedValueForOtherwise = -2
	
	ALINES(aLocalVersion, sLocalVersion , 1, '.')
	ALINES(aForeignVersion, sForeignVersion , 1, '.')	
	
	iNMajorForeignVersion = INT(VAL(aForeignVersion(1)))
	iNMinorForeignVersion = INT(VAL(aForeignVersion(2)))
	iNBuildForeignVersion = INT(VAL(aForeignVersion(3)))
	
	iNMajorLocalVersion = INT(VAL(aLocalVersion(1)))
	iNMinorLocalVersion = INT(VAL(aLocalVersion(2)))
	iNBuildLocalVersion = INT(VAL(aLocalVersion(3)))
		
	iReturnedValue = iReturnedValueForOtherwise
	
	*/ Validación para NMajor
	IF VARTYPE(iNMajorLocalVersion) = 'N' AND VARTYPE(iNMajorForeignVersion) = 'N'
		IF iNMajorForeignVersion >= iNMajorLocalVersion
			iReturnedValue = IIF(iNMajorForeignVersion > iNMajorLocalVersion, iReturnedValueForBig, iReturnedValueForEqual)
		ELSE
			iReturnedValue = iReturnedValueForLess
		ENDIF
	ELSE
		iReturnedValue = iReturnedValueForOtherwise
	ENDIF
		
	*/ Validación para NMinor
	IF iReturnedValue = iReturnedValueForEqual
		IF VARTYPE(iNMinorLocalVersion) = 'N' AND VARTYPE(iNMinorForeignVersion) = 'N'
			IF iNMinorForeignVersion >= iNMinorLocalVersion
				iReturnedValue = IIF(iNMinorForeignVersion > iNMinorLocalVersion, iReturnedValueForBig, iReturnedValueForEqual)
			ELSE
				iReturnedValue = iReturnedValueForLess
			ENDIF
		ELSE
			iReturnedValue = iReturnedValueForOtherwise
		ENDIF
	ENDIF
	
	*/ Validación para nBuild
	IF iReturnedValue = iReturnedValueForEqual
		IF VARTYPE(iNBuildLocalVersion) = 'N' AND VARTYPE(iNBuildForeignVersion) = 'N'
			IF iNBuildForeignVersion >= iNBuildLocalVersion
				iReturnedValue = IIF(iNBuildForeignVersion > iNBuildLocalVersion, iReturnedValueForBig, iReturnedValueForEqual)
			ELSE
				iReturnedValue = iReturnedValueForLess
			ENDIF
		ELSE
			iReturnedValue = iReturnedValueForOtherwise
		ENDIF
	ENDIF
	
	RETURN iReturnedValue 
ENDFUNC


FUNCTION resizeForm

	LPARAMETERS nFormWidth, nFormHeight, nNewWidth, nNewHeight, bDoNotResize as Boolean

	IF !bDoNotResize THEN
		WidthRatio = SYSMETRIC( 1 ) / (N_DEFAULT_WIDTH + N_ADJUST_VALUE)
		HeightRatio = SYSMETRIC( 2 ) / (N_DEFAULT_HEIGHT + N_ADJUST_VALUE) 
		
		IF WidthRatio < 1 OR HeightRatio < 1 THEN
			IF HeightRatio < 1 THEN
				nNewHeight = MAX(nFormHeight * HeightRatio, SAVE_FORM_HEIGHT)
			ELSE
				nNewHeight = nFormHeight 
			ENDIF
			
			IF WidthRatio < 1 THEN
				nNewWidth = nFormWidth * WidthRatio
			ELSE
				nNewWidth = nFormWidth
			ENDIF
			RETURN .T.
		ELSE
			RETURN .F.
		ENDIF
	ELSE
		RETURN .F.
	ENDIF
ENDFUNC


PROCEDURE lockSIVIGILATables
	
	*Locks all SIVIGILA Tables as defined in Tables.SIVIGILADATATables. ;
	If a table is locked, it's posible to access its records only by SIVIGILA app
	
	LOCAL oDBFHeaderCryptographer as Object 
	LOCAL sDBFHeaderCryptographerFileNameAndPath AS String
	
	sDBFHeaderCryptographerFileNameAndPath = PATH_TO_DBFCryptographer_CLASS + DBFCryptographer_CLASS_FILE_NAME
	oDBFHeaderCryptographer = NEWOBJECT("DBFCryptographer",sDBFHeaderCryptographerFileNameAndPath)
		
	SELECT DISTINCT DBF_NAME FROM SIVIGILADATATables INTO CURSOR rsSIVIGILADATATables
	SELECT rsSIVIGILADATATables
	SCAN
		oDBFHeaderCryptographer.sTargetFileNameAndPath = ALLTRIM(rsSIVIGILADATATables.DBF_NAME) + ".DBF"
		oDBFHeaderCryptographer.encryptFile
	ENDSCAN
	USE
	RELEASE oDBFHeaderCryptographer
		
	DO CloseTables WITH "SIVIGILADATATables" IN SIVIGILAUtilities 
ENDPROC


PROCEDURE unlockSIVIGILATables

	*Unlocks all SIVIGILA Tables as defined in Tables.SIVIGILADATATables. ;
	If a table is unlocked, it's posible to access its records by any app capable of reading VFP files

	LOCAL oDBFHeaderCryptographer as Object 
	LOCAL sDBFHeaderCryptographerFileNameAndPath AS String

	sDBFHeaderCryptographerFileNameAndPath = PATH_TO_DBFCryptographer_CLASS + DBFCryptographer_CLASS_FILE_NAME
	oDBFHeaderCryptographer = NEWOBJECT("DBFCryptographer",sDBFHeaderCryptographerFileNameAndPath)

	SELECT DISTINCT DBF_NAME FROM SIVIGILADATATables INTO CURSOR rsSIVIGILADATATables
	SELECT rsSIVIGILADATATables
	SCAN
		oDBFHeaderCryptographer.sTargetFileNameAndPath = ALLTRIM(rsSIVIGILADATATables.DBF_NAME) + ".DBF"
		oDBFHeaderCryptographer.decryptFile
	ENDSCAN
	USE

	RELEASE oDBFHeaderCryptographer
	DO CloseTables WITH "SIVIGILADATATables" IN SIVIGILAUtilities 
ENDPROC


PROCEDURE switchTestMode

	_SCREEN.bTestMode = !(_SCREEN.bTestMode)
	IF _SCREEN.bTestMode THEN
		_SCREEN.Caption = _SCREEN.Caption + " - EN MODO DE PRUEBAS"
		&&SET DEFAULT TO .\DatosValidados
		&&SET PATH TO ..
	ELSE
		_SCREEN.Caption = STRTRAN(_SCREEN.Caption," - EN MODO DE PRUEBAS")
	ENDIF
ENDPROC


FUNCTION CheckTable(sTableNameToTest as String, bWithoutOrder as Boolean )

	LOCAL bReturnedValue as Boolean 
	
	SELECT 0 
	TRY
		IF !bWithoutOrder THEN
			USE (sTableNameToTest) ORDER 1 EXCLUSIVE 
		ELSE
			USE (sTableNameToTest) EXCLUSIVE 
		ENDIF
		USE
		bReturnedValue = .T.
	CATCH TO oException
		bReturnedValue = .F.
	ENDTRY
	RETURN bReturnedValue
ENDFUNC


PROCEDURE reportUsedVersion(sSourceTableName as String, sEntityCode as String, sXLSTargetPath as String)

	*Construye un archivo XLS que da cuenta de la versión del sistema que está utilizando la entidad sEntityCode. Esta versión estará ;
	determinada por la última notificación que se encuentre en el sistema y que se extraerá del conjunto de notificaciones ;
	registradas en sSourceTableName. El archivos xls quedará almacenado en la carpeta sXLSTargetPath 
	
	*sSourceTableName: Nombre de la tabla que contiene las notificaciones de UPGDs. Al menos debe tener ;
	los siguientes campos {COD_PRE C(12), Version C(8), Fecha_not D}
	
	*sEntityCode: Código que identifica a la entidad territorial (1=Municipio, 2=Departamento, 3=UPGD, 4=Sede de UPGD)
	#DEFINE UNM "1"
	#DEFINE UND "2"
	#DEFINE UPGD "3"
	#DEFINE UPGD_SEDE_ "4"

	
	IF EMPTY(sXLSTargetPath) THEN
		sXLSTargetPath = '.\'
	ELSE
		sXLSTargetPath = ADDBS(sXLSTargetPath)
	ENDIF
	
	IF sEntityCode = UNM THEN
		sEntityIdentifiers = '5'
	ELSE
		IF sEntityCode = UND THEN
			sEntityIdentifiers = '2'
		ELSE
			IF sEntityCode = UPGD THEN
				sEntityIdentifiers = '10'
			ELSE
				sEntityIdentifiers = '12'
			ENDIF
		ENDIF
	ENDIF
	sSQLCmd = 'SELECT SPACE(' + sEntityIdentifiers + ') AS COD_ENTIDAD, SPACE(19) AS VersionUsa FROM ' + sSourceTableName + ' WHERE .F. ' + ;
				'INTO CURSOR rsVersionSIVIGILAPorEntidad READWRITE'
	&sSQLCmd
	
	*Recorre el recordSet de notificaciones de UPGDs
	*sSQLCmd = 'SELECT DISTINCT LEFT(cod_pre,' + sEntityIdentifiers + ') as CodEntidad, ' +;
			 Version FROM ' + sSourceTableName +;
			 ' ORDER BY CodEntidad, VERSION DESC INTO CURSOR rsNotifications'
	sSQLCmd = 'SELECT DISTINCT LEFT(cod_pre,' + sEntityIdentifiers + ') as CodEntidad, ' +;
			 ' STRTRAN(Version,"VERSIÓN: ",SUBSTR(Version,3,2)+".") AS Version_, Fecha_not ' +; 
			 ' FROM ' + sSourceTableName +;
			 ' ORDER BY CodEntidad, Fecha_not DESC, VERSION_ DESC' +; 
			 ' INTO CURSOR rsTmpNotifications READWRITE'
	&sSQLCmd
	SELECT rsTmpNotifications
	GO TOP
	REPLACE VERSION_ WITH CHRTRAN(VERSION_,' ','') ALL

	SELECT * FROM rsTmpNotifications ORDER BY CodEntidad, Fecha_not DESC, VERSION_ DESC WHERE !EMPTY(VERSION_) INTO CURSOR rsNotifications
	SELECT rsNotifications
	GO TOP
	DO WHILE !EOF()
		m.COD_ENTIDAD = rsNotifications.CodEntidad
		m.VersionUsa = ALLTRIM(rsNotifications.VERSION_)
		INSERT INTO rsVersionSIVIGILAPorEntidad FROM MEMVAR
		
		DO WHILE m.COD_ENTIDAD = rsNotifications.CodEntidad
			SKIP
		ENDDO
	ENDDO

	sSQLCmd = "INSERT INTO rsVersionSIVIGILAPorEntidad SELECT DISTINCT rsNotifications.CodEntidad as COD_ENTIDAD, 9999 AS VersionUsa FROM " +;
				" rsNotifications LEFT OUTER JOIN rsVersionSIVIGILAPorEntidad ON rsNotifications.CodEntidad" +;
				"=rsVersionSIVIGILAPorEntidad.COD_ENTIDAD WHERE ISNULL(rsVersionSIVIGILAPorEntidad.COD_ENTIDAD)"
	&sSQLCmd	

	SELECT rsVersionSIVIGILAPorEntidad
	DELETE FOR LIKE('?0000',COD_ENTIDAD)

	IF sEntityCode = UNM THEN
		sXLSFileName = 'Version_SIVIGILA_Por_Municipio '
		SELECT cod_entidad, VersionUsa, MUNICIPIOS.NOM_MUN FROM rsVersionSIVIGILAPorEntidad LEFT OUTER JOIN MUNICIPIOS ON;
			rsVersionSIVIGILAPorEntidad.cod_entidad=MUNICIPIOS.cod_mun INTO CURSOR rsVersionesParaExportar
	ELSE
		IF sEntityCode = UND THEN
			sXLSFileName = 'Version_SIVIGILA_Por_Departamento'
			SELECT cod_entidad, VersionUsa, DEPTOS.NOM_DEP FROM rsVersionSIVIGILAPorEntidad LEFT OUTER JOIN DEPTOS ON;
				rsVersionSIVIGILAPorEntidad.cod_entidad=DEPTOS.cod_dep INTO CURSOR rsVersionesParaExportar
		ELSE
			IF sEntityCode = UPGD THEN
				sXLSFileName = 'Version_SIVIGILA_Por_UPGD'
				SELECT cod_entidad, VersionUsa, UPGD_HAB.RAZSOC_HAB FROM rsVersionSIVIGILAPorEntidad LEFT OUTER JOIN UPGD_HAB ON;
					rsVersionSIVIGILAPorEntidad.cod_entidad=UPGD_HAB.cod_hab INTO CURSOR rsVersionesParaExportar
			ELSE
				sXLSFileName = 'Version_SIVIGILA_Por_Sedes_UPGD'
				sSQLCmd = 'SELECT cod_entidad, VersionUsa, UPGD.RAZ_SOC, UPGD.DIR, UPGD.ACT_SIV, UPGD.ES_UNI_NOT, UPGD.ESTADOUPGD ' +;
							'FROM rsVersionSIVIGILAPorEntidad LEFT OUTER JOIN UPGD  ' +;
							'ON rsVersionSIVIGILAPorEntidad.cod_entidad=UPGD.cod_PRE + UPGD.COD_SUB ' + ;
							'INTO CURSOR rsVersionesParaExportar'
				&sSQLCmd 
			ENDIF
		ENDIF
	ENDIF

	SELECT rsVersionesParaExportar
	sCmd = 'COPY TO "' + sXLSTargetPath +  sXLSFileName + '" FOR !DELETED() XL5 '
	&sCmd
ENDPROC


PROCEDURE reportUsedVersionLauncher(sXLSTargetPath AS String)
	
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
	
	sOldTalk = SET("Talk")
	sOldSafety = SET("Safety")
	
	SET TALK ON
	SET SAFETY OFF

	*Recolecta la información de notificaciones
	bContinue = CheckTable('UpdatesTracking', .T.)
	IF bContinue THEN
		=showProgressMessage('Recolectando datos de notificación')
		USE UpdatesTracking IN 0 EXCLUSIVE
		SELECT UpdatesTracking 
		ZAP

		INSERT INTO UpdatesTracking (cod_pre,cod_sub,version,fecha_not) ;
			SELECT cod_pre+cod_sub,cod_sub,SUBSTR(version,12) as version, fec_aju  FROM PACIENTE

		*Comentado por:	Wilosn Aguilar	Fecha:30/09/2019;
		La siguiente parte de código se desactivó por cuanto el camo Version no fluye en los planos de notificación para las tablas involucradas
		INSERT INTO UpdatesTracking (cod_pre,cod_sub,version,fecha_not) ;
			SELECT cod_pre+cod_sub,cod_sub,SUBSTR(version,12) as version, fec_aju  FROM BROTES
		INSERT INTO UpdatesTracking (cod_pre,cod_sub,version,fecha_not) ;
			SELECT cod_pre+cod_sub,cod_sub,SUBSTR(version,12) as version, fec_aju  FROM EVENTOS_43
		
		=showProgressMessage('Estableciendo el estado de actualización')
		*Calcula el estado de actualización en las UNM
		DO reportUsedVersion WITH 'UpdatesTracking','1',sXLSTargetPath 

		*Calcula el estado de actualización en las UND
		DO reportUsedVersion WITH 'UpdatesTracking','2',sXLSTargetPath 
		
		*Calcula el estado de actualización en las UPGDs
		DO reportUsedVersion WITH 'UpdatesTracking','3',sXLSTargetPath 

		*Calcula el estado de actualización en las sedes de UPGDs
		DO reportUsedVersion WITH 'UpdatesTracking','4',sXLSTargetPath 
		
		SELECT UpdatesTracking 
		USE
		=showProgressMessage('El proceso ha terminado')
	ELSE
		=showErrorMessage('No se pudo desocupar la tabla de rastreo de notificaciones. Posiblemente el sistema esté siendo usado en red. Debe asegurar que ningún usuario esté usando el sistema.', 0)
	ENDIF
		
	SET TALK &sOldTalk
	SET SAFETY &sOldSafety
ENDPROC


PROCEDURE updateSivigilaDownloaderIni(sUnitType as String)

	*Método implementado para que cada vez que se ejecute Sivigila, se actualice el archivo SivigilaDownloader.ini con la;
	*información de versión del Sivigla que se está ejecutando actualmente según las constantes ;
	*nMajor_Internal, nMinor_Iternal y nbuild_Internal que están definidas en Sivigila.h También se registra en ese archivo;
	*el identificador de tipo de unidad que está ejecutando el Sivigla según el objeto _SCREEN.oNotifierUnit


	LOCAL oSivigilaUpdaterIniMgr
	LOCAL cSivigilaVersion, sSivigilaVersionInINIFile, cSivigilaMinVersion, cSivigilaMinVersionInINIFile
	LOCAL cSivigilaVersionAfterUpdate, cSivigilaMinVersionAfterUpdate

	cSivigilaVersion = TRANSFORM(nCurrentMajor_Internal) + "."
	cSivigilaVersion =  cSivigilaVersion + TRANSFORM(nCurrentMinor_Internal)+"."
	cSivigilaVersion =  cSivigilaVersion + TRANSFORM(nCurrentBuild_Internal)
	cSivigilaMinVersion = sUPDATE_SUBVERSION

	oSivigilaUpdaterIniMgr = NEWOBJECT( 'IniMgr', 'IniMgr.prg' )

	sSivigilaVersionInINIFile = oSivigilaUpdaterIniMgr.GetValue( "Main", "Version", "SivigilaDownloader.ini" )
	cSivigilaMinVersionInINIFile = oSivigilaUpdaterIniMgr.GetValue( "Main", "minVersion", "SivigilaDownloader.ini" )

	oSivigilaUpdaterIniMgr.SetValue("Main", "Version", cSivigilaVersion, "SivigilaDownloader.ini")
	oSivigilaUpdaterIniMgr.SetValue("Main", "minVersion", cSivigilaMinVersion, "SivigilaDownloader.ini")

	*oSivigilaUpdaterIniMgr.SetValue("Main", "Hierarchy", _SCREEN.oNotifierUnit.sType, "SivigilaDownloader.ini")
	oSivigilaUpdaterIniMgr.SetValue("Main", "Hierarchy", sUnitType, "SivigilaDownloader.ini")

	bSystemUpgrated = .F.

	IF sSivigilaVersionInINIFile <> cSivigilaVersion 
		bSystemUpgrated = .T.
	ELSE
		IF VARTYPE(cSivigilaMinVersionInINIFile) = 'C' AND !EMPTY(cSivigilaMinVersionInINIFile)
			IF cSivigilaMinVersionInINIFile <> cSivigilaMinVersion 
				bSystemUpgrated = .T.
			ENDIF
		ENDIF
	ENDIF

	IF bSystemUpgrated	
		* Si la instalación actual se ha actualizado a una nueva versión de Sivigila, envia un
		* email informando que se ha hecho la actualización.
		cSivigilaVersionAfterUpdate = oSivigilaUpdaterIniMgr.GetValue( "Main", "Version", "SivigilaDownloader.ini" )
		cSivigilaMinVersionAfterUpdate = oSivigilaUpdaterIniMgr.GetValue( "Main", "minVersion", "SivigilaDownloader.ini" )
		
		cSivigilaVersion = cSivigilaVersionAfterUpdate + '(' + cSivigilaMinVersionAfterUpdate + ')'
		cSendMailCmd = "DO SendMailManual WITH 56, '" + cSivigilaVersion +;
						+ ' PC:' + GETENV("COMPUTERNAME") + ' LOGONSERVER:' + GETENV("LOGONSERVER") + ' DOM:' + GETENV("USERDOMAIN") +;
						' USER:' + GETENV("USERNAME") + "', '" + cSivigilaVersion
		cSendMailCmd = cSendMailCmd + "', '' IN " + PATH_TO_SIVIGILAMAILER  + "sendmail.prg"
		TRY
			&cSendMailCmd
		CATCH TO oException

		ENDTRY
	ENDIF

	RELEASE oSivigilaUpdaterIniMgr
ENDPROC


PROCEDURE launchModule(sProgramOrFunction as String, sParameters as string, sLibName as String, ;
						sUserID as Boolean, sOperationToCheck as String)

	LOCAL sCmdToExec as String 
	
	SET PROCEDURE TO SivigilaUserManager ADDITIVE 
	
	sProgramOrFunction = UPPER(sProgramOrFunction)

	DO CASE 
		CASE sProgramOrFunction = 'ACTUALIZARBDDESDEBDUA'
			IF hasRight(sOperationToCheck,sUserID) THEN 
				LOCAL oSIVIGILADataUpdater AS Object 
				
				LOCAL sErrorMsg as String 
				sErrorMsg = ''
				
				SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
				SET PROCEDURE TO SIVIGILAExternalDataLoader ADDITIVE
				
				SET TALK ON

				oSIVIGILADataUpdater = NEWOBJECT("SIVIGILADataUpdater","SIVIGILADataUpdater.fxp")

				*Establece un comando de depuración que elimina registros incosistentes de la tabla fuente tbBDUA
				*sPostCmds = "DELETE FOR TIP_IDE='AS' OR TIP_IDE='MS' OR COD_ASE='NULL' OR EMPTY(COD_ASE)"
				sPostCmds = "DELETE FROM tbBDUA WHERE TIP_IDE='AS' OR TIP_IDE='MS' OR COD_ASE='NULL' OR EMPTY(COD_ASE) " +;
							"OR COD_ASE IN (SELECT COD_ASE FROM ASEGURADORAS WHERE !ACTIVA)"
							
				IF loadTxtFile('','Seleccione el archivo de cambios extraído de BDUA','tbBDUA','TAB', .T., ;
								'PRI_NOM,SEG_NOM,PRI_APE,SEG_APE,TIP_SS,COD_ASE,FEC_CON,FECHA_NTO,FUENTE', ;
								sPostCmds) THEN
					oSIVIGILADataUpdater.updateBasicDataFieldFrom('PACIENTE', 'tbBDUA', 'COD_ASE,TIP_SS') 
					
					IF  EMPTY(oSIVIGILADataUpdater.sErrMsg) THEN
						sErrorMsg = "Actualización terminada con éxito"
					ELSE
						sErrorMsg = oSIVIGILADataUpdater.sErrMsg 
					ENDIF	
				ELSE
					sErrorMsg = 'No seleccionó ningún archivo válido' 
				ENDIF

				SET TALK OFF 
				
				=showErrorMessage(sErrorMsg, 0)

				RELEASE oSIVIGILADataUpdater
			ENDIF 
			
		OTHERWISE
			sCmdToExec = "DO " + sProgramOrFunction
			IF VARTYPE(sParameters) = 'C' AND !EMPTY(sParameters) THEN 
				sCmdToExec = "DO " + sProgramOrFunction + " WITH " + sParameters
			ENDIF
			IF VARTYPE(sLibName) = 'C' AND !EMPTY(sLibName) THEN 
				sCmdToExec = "DO " + sProgramOrFunction + " IN '" + sLibName + "'"
			ENDIF
			
			&sCmdToExec 
		ENDCASE 	
ENDPROC


FUNCTION isEAPB(sCompleteUnitCode as String)
	*Returns .T. if last two digits of sCompleteUnitCode are equal to '55'. That's how an EAPB is characterized in the System
	RETURN RIGHT(sCompleteUnitCode,2) == '55'
ENDFUNC


PROCEDURE initLauncher(sCmdToExec as String)

	PUBLIC goSIANIESPEnvironment as Object

	LOCAL nCurrentDataSession as Number 
	LOCAL sOldTalk as String

	*SET STEP ON 
	nCurrentDataSession = SET("Datasession")
	sOldTalk = SET("Talk")
	SET TALK OFF 

	goLauncherEnvironment = NEWOBJECT("Launcher","SivigilaSystem.prg",.null.)

	sCmd = "SET DATASESSION TO " + goLauncherEnvironment.sMySession
	&sCmd
	
	TRY 	
		&sCmdToExec
	CATCH TO oException
		sErrMsg  = "No se pudo ejecutar el complemento. Se produjo el siguiente error: " +  CHR(13) +  CHR(10) +;
					"Código interno de error: " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
					"Mensaje de error: " + (oException.Message) + CHR(13)
					
		WAIT sErrMsg WINDOW TIMEOUT 15
	ENDTRY

	SET TALK &sOldTalk

	sCmd = "SET DATASESSION TO " + ALLTRIM(STR(nCurrentDataSession))
	TRY
		&sCmd
	CATCH TO oException
	ENDTRY

	RELEASE goLauncherEnvironment 
ENDPROC 


PROCEDURE goToURL(sPageID as string)

	*Opens the URL sPageID in the default browser accordingly to it's definition in the SIVIGILAHelp.ini file

	LOCAL sHelpURLToGoto as string 

	LOCAL oForm as Object 
	LOCAL oHelpIniMgr as Object 

	oHelpIniMgr = NEWOBJECT('IniMgr', 'IniMgr.fxp', .null. , 'SIVIGILAHelp.ini', .T.)

	oForm = CREATEOBJECT('Form')  && Create a Form
	oForm.AddObject('HyperlinkButton','Hyperlink')  

	sHelpURLToGoto = oHelpIniMgr.GetValue(sPageID, 'URL')
	oForm.HyperlinkButton.NavigateTo(sHelpURLToGoto)

	RELEASE oForm
	RELEASE oHelpIniMgr
ENDPROC 


FUNCTION  getMacAddress()
	*Returns a comma separated list of PC mac addresses as returned by getmac command
	*Refers to https://www.windows-commandline.com/get-mac-address-command-line/ for details about getmac command
	
	LOCAL sMacList as String
	sMacList = ''
	
	LOCAL sProc as String
	sProc = SET("Procedure")
	
	DIMENSION aMacFile(1)
	
	SET PROCEDURE TO Utilities ADDITIVE 

	sTmpMacFileNameAnPath = ADDBS(SYS(2023)) + SYS(2015) + '.txt'

	sRunCmd = 'RUN getmac /FO "CSV" > "' + sTmpMacFileNameAnPath + '"'
	&sRunCmd
	
	nMacFileLines = ALINES(aMacFile,FILETOSTR(sTmpMacFileNameAnPath),1+2+4+8,'"')
	TRY
		DELETE FILE (sTmpMacFileNameAnPath)
	CATCH TO oException
	ENDTRY 
	FOR iMacFileLine=1 TO nMacFileLines 
		sToTest = ALLTRIM(STRTRAN(STRTRAN(aMacFile(iMacFileLine),CHR(13)),CHR(10)))
		IF isHexa(sToTest) THEN 
			sMacList = sMacList + sToTest + ','
		ENDIF 
	NEXT 
	SET PROCEDURE TO &sProc 

	RETURN SUBSTR(sMacList,1,LEN(sMacList)-1)
ENDFUNC 

FUNCTION  getIPAddress()
	*Returns PC IP addresses as returned by nslookup command

	STORE  '' TO sIPList
	STORE .T. TO bOK 
	STORE SET("Procedure") TO sProc
	
	DIMENSION aIPFile(1)
	
	SET PROCEDURE TO Utilities ADDITIVE 

	sTmpIPFileNameAnPath = ADDBS(SYS(2023)) + SYS(2015) + '.txt'

	sRunCmd = 'RUN nslookup myip.opendns.com. resolver1.opendns.com > "' + sTmpIPFileNameAnPath + '"'
	TRY 
		&sRunCmd
		sResultFile = FILETOSTR(sTmpIPFileNameAnPath)
		nStartPos = AT('Addre',sResultFile,2)
	CATCH TO oException
		bOK = .F.		
	ENDTRY 

	TRY
		DELETE FILE (sTmpIPFileNameAnPath)
	CATCH TO oException
	ENDTRY 
	
	SET PROCEDURE TO &sProc 
	IF bOK then
		RETURN ALLTRIM((SUBSTR(sResultFile,nStartPos,LEN(sResultFile))))
	ELSE
		RETURN ''
	ENDIF 
ENDFUNC 



PROCEDURE execExclusive(sProcName as String, sCommomName as String)

	LOCAL sResultMsg as string 
	sResultMsg = ''
	
	LOCAL bContinue as Boolean 
	bContinue = .T.
	
	LOCAL sdOnError as String 
	sOldOnError = ON('ERROR')
	ON ERROR DO ErrHandler WITH ERROR(), MESSAGE(), PROGRAM(), ''	&& (sOldOnError) 
	*ON ERROR DO (PATH_TO_COMMON_LIB + '\SimpleErrorHandler') WITH MESSAGE(),MESSAGE(1),PROGRAM(),LINENO(),.T.
	
	vExclusiveState=SET("Exclusive")
	vTalkState=SET("Talk")
	vSafetyState=SET("Safety")
	
	SET EXCLUSIVE ON
	SET TALK ON
	SET SAFETY OFF

	CLOSE ALL
	
	* Dado que la sesión de datos 2 mantiene las tablas de registro de errores del manejador de errores del SIVIGILA;
	y por tanto los archivos de la base de datos BDSIVIGILA están abiertos, es necesario cerrarlos y restablecer la sesión de datos ;
	tal como se encontraba antes del cierre 
			
	SET DATASESSION TO 2
	sOldSafety = SET("Safety")
	SET SAFETY OFF
	CREATE VIEW errDataSessionView
	SET SAFETY &sOldSafety
	CLOSE ALL

	SET DATASESSION TO 1
	TRY
		OPEN DATABASE BDSivigila EXCLUSIVE
		VALIDATE DATABASE RECOVER NOCONSOLE
		bContinue = .T.
	CATCH TO oException
		bContinue = .F.
		IF oException.ErrorNo=1705THEN
			sResultMsg="Se produjo el siguiente problema al intentar reindexar: " +  CHR(13) +  CHR(10) +;
						"La base de datos del sistema se encuentra abierta por otro usuario/aplicativo."
		ELSE
			sResultMsg="Se produjo el siguiente problema al intentar : " + sCommomName +  CHR(13) +  CHR(10) +;
						"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
						"Instrucción que produjo el problema : " + oException.Procedure + " - " + oException.LineContents + ;
							" - Línea " + ALLTRIM(STR(oException.LineNo)) +CHR(13) +;
						"Detalles del problema : " + oException.Details + CHR(13) +;
						"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) 
		ENDIF
		MESSAGEBOX( sResultMsg, 0 + 48 + 256, 'Error en ' + sCommomName)
				
	ENDTRY
	
	IF bContinue THEN 
		&sProcName 
		
		CLOSE ALL
		SET EXCLUSIVE &vExclusiveState
		SET TALK &vTalkState
		SET SAFETY &vSafetyState
		CLOSE DATABASES
		
		MESSAGEBOX( "Se ha terminado el proceso " + sCommomName + CHR(13) + ;
					"El Sistema se cerrará.", 0 + 48 + 256, 'SIVIGILA' )
		QUIT
	ENDIF 

	SET DATASESSION TO 2
	SET VIEW TO errDataSessionView
	SET DATASESSION TO 1
	ON ERROR &sOldOnError
ENDPROC


PROCEDURE restoreTempDB
	
	*Restaura los archivos de la BD temporal SivigilaTemp y limpia la carpeta temporal de reportes REPORTES\Tmp y los archivos rs*.DBF que corresponden;
	a recordsets temporales
	
	LOCAL bContinue as Boolean 
	LOCAL sErrorMsg  AS String, sResult  as String, sTmpDirName  as String, sSOCmd as String  
	
	SET PROCEDURE TO (PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger') ADDITIVE

	sOldSafety = SET("Safety")
	SET SAFETY OFF

	SET LIBRARY TO LOCFILE("vfpcompression.fll")
	sTmpDirName = ADDBS(SYS(2023)) + SYS(2015)
	IF UnzipQuick("SivigilaTemp.zip", sTmpDirName, .F.) THEN
		
		sSOCmd = 'XCOPY "' + sTmpDirName + '\*.*" . /s /y' + CrLf
		=STRTOFILE(sSOCmd,"RESTAURAR.bat") 
		sSOCmd = "ECHO %ERRORLEVEL% > " + sTmpDirName + "\Result.txt "
		=STRTOFILE(sSOCmd,"RESTAURAR.bat",1)  
		RUN RESTAURAR.bat
		DELETE FILE RESTAURAR.bat
		
		sResult = FILETOSTR(sTmpDirName + "\Result.txt ")
		IF '4' $ sResult THEN 
			*El comando XCOPY terminó con un error del tipo 'infracción al compartir'
			sErrorMsg = ERR_IMPOSIBLE_RESTORE_DB_MESSSAGE
		ELSE
			sErrorMsg = NOTIFICATION_DB_RESTORED_MESSSAGE 
		ENDIF
		
		WAIT  "Eliminando archivos temporales" WINDOW NOWAIT TIMEOUT 5
		sSOCmd = "RUN RD " + sTmpDirName + " /q /s"
		&sSOCmd
		WAIT CLEAR 
	ELSE
		sErrorMsg = ERR_IMPOSIBLE_RESTORE_DB_MESSSAGE
	ENDIF
	
	TRY 
		DELETE FILE rs*.DBF
		DELETE FILE Reportes\Tmp\*.*
	CATCH TO oException
	ENDTRY 
	
	SET SAFETY &sOldSafety
	=showErrorMessage(sErrorMsg,0)
ENDPROC


FUNCTION launchSivigilaCrypto_V01(sSourcePath as String, sTargetPath as String, sFileMask as String ) as boolean 
	*Decodifica los archivos que obedezcan el patrón sFileMask que se encuentren en sSourcePath y los deja decodificados en sTargetPath. ;
	Por default sFileMask = '*.txt'
	
	LOCAL oCodifier as Object 
	LOCAL sDecodifiedFilesFolder  as String, sSivigilaCryptoFileNameAndPath as String 
	LOCAL returnedValue as Boolean 
	
	IF !(VARTYPE(sFileMask)='C' AND !EMPTY(sFileMask)) THEN 
		sFileMask = '*.txt'
	ENDIF 
	
	sSivigilaCryptoFileNameAndPath = PATH_TO_SIVIGILACRYPTO + "SivigilaCrypto.prg"
	oCodifier  = NEWOBJECT( "SivigilaCrypto", sSivigilaCryptoFileNameAndPath, .NULL., sSourcePath, sFileMask, sTargetPath)
	oCodifier.BatchDecodify
	
	returnedValue = oCodifier.getSivigilaCryptoStatus()

	RELEASE oCodifier
	
	RETURN returnedValue 
ENDPROC 


FUNCTION launchSivigilaCrypto(sSourcePath as String, sTargetPath as String, sFileMask as String ) as boolean 
	*Decodifica los archivos que obedezcan el patrón sFileMask que se encuentren en sSourcePath y los deja decodificados en sTargetPath. ;
	Por default sFileMask = '*.txt'
 	
	IF !(VARTYPE(sFileMask)='C' AND !EMPTY(sFileMask)) THEN 
		sFileMask = '*.txt'
	ENDIF 

	sCmd = 'RUN SivigilaCrypto.exe "' + sSourcePath + '" "' + sTargetPath + '" "' + sFileMask + '"'
	&sCmd
	RETURN .T.
ENDPROC 

