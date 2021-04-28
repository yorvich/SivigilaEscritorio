#INCLUDE SIVIGILA.H

DEFINE CLASS DuplicatesSolver as AnomaliesSolver OF PATH_TO_DEVELOPMENT_ENVIRONMENT + 'AnomaliesSolver'

	FUNCTION setDefaultKey  as String
		RETURN This.sDefaultKey
	ENDFUNC

ENDDEFINE

PROCEDURE CheckFieldExistence

	LPARAMETERS sFiedlName as String

	CLEAR
	SET ALTERNATE TO FieldExistenceChecked 
	SET ALTERNATE ON
	
	FOR i=1 TO n_SIVIGILA_EVENTS
		sTargetTableName="EVENTOS_" + PADL(ALLTRIM(STR(i)),2,'0')
		TRY
			SELECT &sFiedlName FROM &sTargettableName INTO ARRAY atemp
			SELECT (sTargetTableName)
			USE
			?"La tabla " + sTargettableName + " pasó la prueba"
		CATCH TO oException
			
		ENDTRY
		
	NEXT i
	SET ALTERNATE OFF	
ENDPROC
	
	
PROCEDURE CheckIndexExistence

	LPARAMETERS sTagName as String

	CLEAR
	SET ALTERNATE TO IndexExistenceChecked 
	SET ALTERNATE ON
	
	FOR i=1 TO n_SIVIGILA_EVENTS
		sTargetTableName="EVENTOS_" + PADL(ALLTRIM(STR(i)),2,'0')
		USE (sTargetTableName)
		nIndexes=ATAGINFO(aCurrentIndexes)
		nIndexRow=ASCAN(aCurrentIndexes,sTagName,-1,-1,1,15) 
		IF nIndexRow!=0 THEN
			?"La tabla " + sTargettableName + " pasó la prueba"
			sReportResult=sTargetTableName + CHR(13) + CHR(10) + aCurrentIndexes(nIndexRow,3) + CHR(13) + CHR(10)
			?sReportResult
			USE
		ELSE
			sReportResult="La tabla " + sTargettableName + " NO pasó la prueba" + CHR(13) + CHR(10)
			?sReportResult
		ENDIF		
	NEXT i
	SET ALTERNATE OFF	
ENDPROC

PROCEDURE SetTestEnvironment
	
	LPARAMETERS sEventCode as String
	
	glDisableRules=.t.
	USE paciente
	DELETE ALL
	IF VARTYPE(sEventCode )='C' THEN
		sRecallCmd="RECALL ALL FOR cod_eve='" + sEventCode + "'"
	ELSE
		sRecallCmd="RECALL ALL"
	ENDIF
	&sRecallCmd
	RELEASE glDisableRules
	DO main
	
ENDPROC



PROCEDURE deleteFiles

	LPARAMETERS sTargetPath as String

	sOldONError=ON("ERROR")
	
	ON ERROR WAIT WINDOW "Error " + LTRIM(STR(ERROR())) + CHR(13) + "Message() " + MESSAGE() + CHR(13) + "Message(1) " + MESSAGE(1)

	TRY
		DELETE FILE '&sTargetPath\*.*'
	CATCH TO oException
	ENDTRY
	
	ON ERROR &sOldONError
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

PROCEDURE checkCorruptedTable
	
	LPARAMETERS sTargetTableName as String
	
	TRY
		USE (sTargetTableName) IN 0 AGAIN ALIAS TargetTable
	CATCH TO oException
		sResultMsg="Se produjo el siguiente error: " +  CHR(13) +  CHR(10) +;
					"Código interno de error: " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
					"Mensaje de error: " + (oException.Message) + CHR(13)
	
		MESSAGEBOX(sResultMsg, 0 + 48 + 256, "Error de chequeo")
	ENDTRY
	
ENDPROC



PROCEDURE reportUPGDsVersion

	LPARAMETERS nWeekToLookfor as Number, sPathToSourceFiles as String
	
	sExcludedTableEvents = " 32 "
	*En la tabla EVENTOS_32 se almacena un evento de seguimiento que no contiene el campo SEMANA, por tanto, no puede ;
	tenerse en cuenta al elaborar informeS que involucren la semana epidemiológica

	sOldCurrentDir = SYS(5) + SYS(2003)
	SET DEFAULT TO '&sPathToSourceFiles'

	*Extre las notificaciones de datos básicos
	SELECT COUNT(*) as n,cod_pre,cod_sub,version, año,VAL(semana) as nSemana FROM paciente  GROUP BY cod_pre,cod_sub,version,año,nSemana ORDER BY cod_pre,cod_sub,año,nSemana INTO CURSOR rsDB

	*Extrae las notificaciones de Brotes
	SELECT COUNT(*) as n,cod_pre,cod_sub,version, año,VAL(semana) as nSemana FROM brotes  GROUP BY cod_pre,cod_sub,version,año,nSemana ORDER BY cod_pre,cod_sub,año,nSemana INTO CURSOR rsDC

	*Extrae las notificaciones de eventos con tablas de datos independiente
	sCmd = "DO " + ADDBS(sOldCurrentDir) + "EventosConTablaDatosIndependiente.QPR"
	&sCmd
	SELECT DISTINCT "EVENTOS_" + PADL(FORMULARIO,2,'00') AS NombreTablaEventoSeguimiento FROM rsEventosConTablaDatosIndependiente WHERE ;
		!(rsEventosConTablaDatosIndependiente.FORMULARIO $ sExcludedTableEvents) INTO CURSOR rsFollowupEvents
	SELECT rsFollowupEvents
	sSQLSelectCmd = ""
	SCAN
		sSQLSelectCmd = sSQLSelectCmd + "SELECT COD_PRE,COD_SUB,Version,Año,VAL(semana) as nSemana FROM " + ;
						rsFollowupEvents.NombreTablaEventoSeguimiento +;
						" GROUP BY cod_pre,cod_sub,version,año,nSemana UNION "
	ENDSCAN
	sSQLSelectCmd = LEFT(sSQLSelectCmd ,LEN(sSQLSelectCmd) - 6)
	sSQLSelectCmd = sSQLSelectCmd + " INTO CURSOR rsFollowupsNotifications"
	&sSQLSelectCmd

	*Une todos los registros de notificación y los ordena apropiadamente
	SELECT * FROM rsDB UNION SELECT * FROM rsDC UNION SELECT * FROM rsDC rsFollowupsNotifications INTO CURSOR rsNotificacionesAux

	SELECT *, 9999 as versionUsada FROM rsNotificacionesAux WHERE AÑO='2014' AND nSemana = (nWeekToLookfor) ORDER BY cod_pre,cod_sub,año,nSemana ;
		INTO CURSOR rsNotificaciones READWRITE
*SET STEP ON
	*Recorre el recordSet de notificaciones tomando en consideración solamente la semana sWeekToLookfor
	SELECT rsNotificaciones 
	DO WHILE !EOF()
		sCurrentUPGD_Pre = rsNotificaciones.COD_PRE
		sCurrentUPGD_Sub = rsNotificaciones.COD_SUB
		IF '2014' $ rsNotificaciones.Version THEN
			nInitVersion = 2014
		ELSE
			nInitVersion = 2012
		ENDIF
		bContinue = .T.
		SKIP
		DO WHILE sCurrentUPGD_Pre = rsNotificaciones.COD_PRE AND sCurrentUPGD_Sub = rsNotificaciones.COD_SUB AND bContinue
			IF '2014' $ rsNotificaciones.Version THEN
				nCurrentVersion = 2014
			ELSE
				nCurrentVersion = 2012
			ENDIF
			SKIP
			bContinue = (nInitVersion = nCurrentVersion )
		ENDDO
		
		SKIP -1
		IF bContinue THEN
			REPLACE versionUsada WITH nInitVersion
		ELSE
			REPLACE versionUsada WITH MIN(nInitVersion, nCurrentVersion)
			DO WHILE sCurrentUPGD_Pre = rsNotificaciones.COD_PRE AND sCurrentUPGD_Sub = rsNotificaciones.COD_SUB 
				SKIP
			ENDDO
			SKIP -1
		ENDIF
		SKIP
	ENDDO
	SET DEFAULT TO '&sOldCurrentDir'
ENDPROC


PROCEDURE reportMunicipalitiesVersion

	LPARAMETERS sSourceTableName as String, sEntityCode as String
	
	*sSourceTableName: Nombre de la tabla que contiene las notificaciones de UPGDs. Al menos debe tener ;
	los siguientes campos {COD_PRE C(12), Version C(8)}
	
	*sEntityCode: Código que identifica a la entidad territorial (1=Municipio, 2=Departamento, 3=UPGD, 4=Sede de UPGD)
*SET STEP ON 	
	IF sEntityCode = '1' THEN
		sEntityIdentifiers = '5'
	ELSE
		IF sEntityCode = '2' THEN
			sEntityIdentifiers = '2'
		ELSE
			IF sEntityCode = '3' THEN
				sEntityIdentifiers = '10'
			ELSE
				sEntityIdentifiers = '12'
			ENDIF
		ENDIF
	ENDIF
	sSQLCmd = 'SELECT SPACE(' + sEntityIdentifiers + ') AS COD_ENTIDAD, SPACE(19) AS VersionUsa FROM "' + sSourceTableName + '" WHERE .F. ' + ;
				'INTO CURSOR rsVersionSIVIGILAPorEntidad READWRITE'
	&sSQLCmd
	
	*Recorre el log de autoactualizaciones
	sSQLCmd = 'SELECT LEFT(cod_upgd,' + sEntityIdentifiers + ') as CodEntidad, Version FROM "' + sSourceTableName + '" WHERE !EMPTY(cod_upgd) ' 
	IF sEntityCode = '1' THEN
		sWHEREClause = " AND !LIKE('??000',LEFT(cod_upgd,5))"
	ELSE
		IF sEntityCode = '2' THEN
			sWHEREClause = " AND LIKE('??000',LEFT(cod_upgd,5))"
		ELSE
			IF sEntityCode = '3' THEN
				sWHEREClause = " AND .T."
			ELSE
				sWHEREClause = " AND .T."
			ENDIF
		ENDIF
	ENDIF
	sSQLCmd = sSQLCmd + sWHEREClause + ' ORDER BY CodEntidad, VERSION DESC INTO CURSOR rsNotifications'
	&sSQLCmd
	SELECT rsNotifications
	GO TOP
	
	DO WHILE !EOF()
		m.COD_ENTIDAD = rsNotifications.CodEntidad
		m.VersionUsa = ALLTRIM(rsNotifications.Version)
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

	IF sEntityCode = '1' THEN
		sXLSFileName = 'VersionSIVIGILAPorMunicipio '
	ELSE
		IF sEntityCode = '2' THEN
			sXLSFileName = 'VersionSIVIGILAPorDepartamento'
		ELSE
			IF sEntityCode = '3' THEN
				sXLSFileName = 'VersionSIVIGILAPorUPGD'
			ELSE
				sXLSFileName = 'VersionSIVIGILAPorSedesUPGD'
			ENDIF
		ENDIF
	ENDIF
	COPY TO &sXLSFileName XL5 
ENDPROC


PROCEDURE reportEntityVersionTst
	*SET STEP ON 
	
	*Warning: ;
	Execute the following command from the command line before making this calling: ;
	INSERT INTO UpdatesTracking (cod_pre,cod_sub,version) SELECT cod_pre+cod_sub,cod_sub,SUBSTR(version,12) as version  FROM 'C:\Users\wilson.aguilar\Documents\ProyectoSivigila\Pruebas\DatosLineaBase\BDIns\BDINS_2016_AcumuladaSemana09\paciente'
	
	*Calcula el estado de actualización en las UNM
	DO reportMunicipalitiesVersion WITH 'C:\Users\wilson.aguilar\Documents\ProyectoSivigila\Distribucion\SIVIGILA AUTOUPDATES','1'

	*Calcula el estado de actualización en las UND
	DO reportMunicipalitiesVersion WITH 'C:\Users\wilson.aguilar\Documents\ProyectoSivigila\Distribucion\SIVIGILA AUTOUPDATES','2'
	
	*Calcula el estado de actualización en las UPGDs
	DO reportMunicipalitiesVersion WITH 'C:\Users\wilson.aguilar\Documents\ProyectoSivigila\Distribucion\SIVIGILA AUTOUPDATES','3'

	*Calcula el estado de actualización en las sedes de UPGDs
	*DO reportMunicipalitiesVersion WITH 'C:\Users\wilson.aguilar\Documents\ProyectoSivigila\Distribucion\SIVIGILA AUTOUPDATES','4'
ENDPROC



PROCEDURE checkRecsWhithoutDC

	LPARAMETERS sSIVIGILAPath as string

	LOCAL sOldCurrentDir as String, sOldProc as String, sOldSafety as String, sOldDeleted as string
	
	
	sOldSafety = SET("Safety")
	
	sOldDeleted = SET("Deleted")
	SET DELETED ON
	
	sOldProc = SET("Procedure")
	SET PROCEDURE TO (PATH_TO_COMMON_LIB) + 'QueriesHandler'
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE

	DO eventosConTablaDatosIndependiente.qpr
	SELECT * FROM EVENTOS WHERE !EMPTY(FORMULARIO) AND COD_EVE NOT IN (SELECT COD_EVE FROM rsEventosConTablaDatosIndependiente) INTO CURSOR rsEvents
	
	sOldCurrentDir = SET("Directory")
	SET DEFAULT TO '&sSIVIGILAPath'
	
	sIndividualLinkingFields = 'AÑO,SEMANA,COD_EVE,TIP_IDE,NUM_IDE,COD_PRE,COD_SUB'
	sOutbreaksLinkingFields = 'COD_EVE,AÑO,SEMANA,COD_MUN,NUM_CON,COD_PRE,COD_SUB'
	SELECT &sIndividualLinkingFields FROM PACIENTE WHERE .F. INTO CURSOR rsIndividual READWRITE
	SELECT &sOutbreaksLinkingFields FROM BROTES WHERE .F. INTO CURSOR rsOutbreak READWRITE


	SELECT rsEvents
	SCAN
		sTargetEvent = rsEvents.COD_EVE

		DO CASE
			CASE rsEvents.TIP_NOT = 1
				sSQLCmd = "INSERT INTO rsIndividual SELECT " + qualiFyFields(sIndividualLinkingFields,',','PACIENTE') + " FROM PACIENTE LEFT OUTER JOIN " + ALLTRIM(rsEvents.DC_TABLA) + " ON " +;
							MatchFields(sIndividualLinkingFields,'PACIENTE',sIndividualLinkingFields,ALLTRIM(rsEvents.DC_TABLA),',') + " WHERE " + ;
							"PACIENTE.COD_EVE='" + sTargetEvent + "' AND " + ALLTRIM(rsEvents.DC_TABLA) + ".COD_PRE IS NULL "
				&sSQLCmd
			CASE rsEvents.TIP_NOT = 3
				sSQLCmd = "INSERT INTO rsOutbreak SELECT " + qualiFyFields(sOutbreaksLinkingFields,',','BROTES') + " FROM BROTES LEFT OUTER JOIN " + ALLTRIM(rsEvents.DC_TABLA) + " ON " +;
							MatchFields(sOutbreaksLinkingFields,'BROTES',sOutbreaksLinkingFields,ALLTRIM(rsEvents.DC_TABLA),',') + " WHERE " + ;
							"BROTES.COD_EVE='" + sTargetEvent + "' AND " + ALLTRIM(rsEvents.DC_TABLA) + ".COD_PRE IS NULL "
				&sSQLCmd
		ENDCASE
	ENDSCAN

	SET SAFETY OFF
	SELECT rsIndividual 
	sCmd = 'COPY TO ' + ADDBS(sSIVIGILAPath) + 'Individual_sin_DC'
	&sCmd
	SELECT rsOutbreak 
	sCmd = 'COPY TO ' + ADDBS(sSIVIGILAPath) + 'Brotes_sin_DC'
	&sCmd
	SET SAFETY &sOldSafety

	SET DEFAULT TO '&sOldCurrentDir'
	SET DELETED &sOldDeleted
	SET PROCEDURE TO &sOldProc 
	
ENDPROC


PROCEDURE connectToMySQL()

	lcStringCnxRemoto = "DRIVER={MySQL ODBC 5.3 ANSI Driver};" + ;
	                    "SERVER=192.168.0.11;" + ;
	                    "PORT=3306;" + ;
	                    "UID=root;" + ;
	                    "PWD=RailwayStat;" + ;
	                    "DATABASE=sivigilatst;" + ;
	                    "OPTIONS=131329;"
                    
	SQLSETPROP(0,"DispLogin" , 3 )
	lnHandle = SQLSTRINGCONNECT(lcStringCnxRemoto)
	IF lnHandle > 0
		cmd = SQLEXEC(lnHandle,"SELECT * FROM PACIENTE LIMIT 500","rsDB")
		IF cmd > 0
			BROWSE
		ELSE
			AERROR(laErr)
			MESSAGEBOX("No se pudo conectar a mySQL. Error: " + CHR(13) + laErr[2])
		ENDIF
		USE IN rsDB
		SQLDISCONNECT(lnHandle)
	ELSE
		AERROR(laErr)
		MESSAGEBOX("No se pudo conectar a mySQL. Error: " + CHR(13) + laErr[2])
	ENDIF
ENDPROC


PROCEDURE makeSivigilaWebData_v01(sSourceTablePathAndName as String, sOwnerId as string, nRequiredRows as Number, nInitRec as number, ;
								sFilter as String, sDistinctFieldList as String)

	LOCAL sSQLScript as String
	sSQLScript  = ''
	
	LOCAL sInsertSQLCmd as String 
	sInsertSQLCmd = ''
	
	LOCAL sSourceTableName as String
	sSourceTableName = UPPER(JUSTFNAME(sSourceTablePathAndName))
	
	LOCAL sOldDate as String
	sOldDate =SET("Date")

	LOCAL oAnomaliesSolver as Object
	oAnomaliesSolver=NEWOBJECT("DuplicatesSolver")
	
	SET DATE YMD

	IF VARTYPE(nInitRec)!="N" THEN
		nInitRec = 1
	ENDIF
	IF VARTYPE(sFilter)!="C" THEN
		sFilter = '.T.'
	ENDIF

	sTbTmpSourceRecords = SYS(2023) + "\rsTablaFuente"
	SELECT * FROM [&sSourceTablePathAndName] WHERE &sFilter INTO TABLE &sTbTmpSourceRecords READWRITE 

	*Elimina registros duplicados de los registros fuente
	IF VARTYPE(sDistinctFieldList)='C' AND !EMPTY(sDistinctFieldList ) THEN 
		oAnomaliesSolver.sSourceTableName = "rsTablaFuente"
		oAnomaliesSolver.bIsCursor = .T.
		oAnomaliesSolver.sDefaultKey = ' ' + STRTRAN(sDistinctFieldList, ',', ', ') + ' '
		oAnomaliesSolver.deleteDuplicates()
	ENDIF
	RELEASE oAnomaliesSolver
	
	SELECT rsTablaFuente
	IF VARTYPE(nRequiredRows)!="N" THEN
		nRequiredRows = RECCOUNT()
	ENDIF
	
	GO nInitRec 
	FOR iRow=1 TO nRequiredRows 
		DO CASE
			CASE sSourceTableName = 'PACIENTE'
				sInsertSQLCmd = "INSERT INTO dbo.Pacientes (owner,TipoDocumento,NumeroDocumento,PrimerNombre,SegundoNombre,PrimerApellido,SegundoApellido," +;
						"FechaNacimiento,Sexo,PertenenciaEtnica,TelefonoFijo,TelefonoCelular,DireccionDeResidenciaContacto1,MunicipioResidencia,IdGrupoEtnico)  " +;
						" VALUES ('" +;
						sOwnerId + "','" +;
						ALLTRIM(rsTablaFuente.TIP_IDE) + "','" +;
						ALLTRIM(rsTablaFuente.NUM_IDE) + "','" +;
						ALLTRIM(rsTablaFuente.PRI_NOM) + "','" +;
						ALLTRIM(rsTablaFuente.SEG_NOM) + "','" +;
						ALLTRIM(rsTablaFuente.PRI_APE) + "','" +;
						ALLTRIM(rsTablaFuente.SEG_APE) + "','" +;
						STRTRAN(DTOC(rsTablaFuente.FECHA_NTO),'/') + "','" +;
						ALLTRIM(IIF(rsTablaFuente.SEXO='M','H',IIF(rsTablaFuente.SEXO='F','M',rsTablaFuente.SEXO))) + "','" +;
						ALLTRIM(rsTablaFuente.PER_ETN) + "','" +;
						ALLTRIM(rsTablaFuente.TELEFONO) + "','" +;
						'300' + ALLTRIM(rsTablaFuente.TELEFONO) + "','" +;
						ALLTRIM(rsTablaFuente.DIR_RES) + "','" +;
						ALLTRIM(rsTablaFuente.MUN_PRO) + "'," +;
						IIF(EMPTY(GRUPO_ETNI),'999',GRUPO_ETNI) +;
						+ ")"
						
			CASE sSourceTableName = 'GRUPOSETNICOS'
				sInsertSQLCmd = "INSERT INTO dbo.GruposEtnicos (IdGrupo, Nombre, IdOficial, Activo, FechaInactivacion) " +;
						" VALUES (" +;
						ALLTRIM(STR(VAL(rsTablaFuente.COD_GRUPO))) + ",'" +;
						ALLTRIM(rsTablaFuente.NOM_GRUPO) + "',NULL," +;
						ALLTRIM(IIF(rsTablaFuente.ACTIVO,'1','0')) + ",NULL" +;
						+ ")"
						
			CASE sSourceTableName = 'PAISES'
				sInsertSQLCmd = "INSERT INTO dbo.Paises (IdPais, NombrePais) " +;
						" VALUES (" +;
						ALLTRIM(STR(VAL(rsTablaFuente.CODNUMPAIS))) + ",'" +;
						ALLTRIM(rsTablaFuente.PAIS) + "'" +;
						+ ")"
			ENDCASE
			
			sSQLScript = sSQLScript + sInsertSQLCmd + CrLf
		SKIP
	NEXT iRow
	=STRTOFILE(sSQLScript, 'Sivigila_web_Datos_' + sSourceTableName + '.sql')
	
	SET DATE &sOldDate 
ENDPROC


FUNCTION launchExternalApp(sUPDATERExeFileNameAndPath as String)
	* Lanza una instancia de la aplicación que se encuentre en sUPDATERExeFileNameAndPath 

    DECLARE INTEGER ShellExecute IN SHELL32.DLL ;
        INTEGER handle,;
        STRING @sFile,;
        STRING @lp,;
        STRING @DIR,;
        STRING @dir1,;
        INTEGER ncmd

	nReturnedValue = ShellExecute(0, "Open", sUPDATERExeFileNameAndPath,"", "", 1)
    CLEAR DLLS ShellExecute
    
    RETURN nReturnedValue
ENDFUNC 


PROCEDURE makeSivigilaWebData_Deptos_V01(sSourceTablePathAndName as String, nRequiredRows as Number, sFilter as String, nInitRec as number)
	*Crea un script con instrucciones INSERT para SQLServer que permiten poblar una tabla Departamentos definida así:
	&&CREATE TABLE [dbo].[Departamentos] (
    &&[IdDepartamento]     INT        NOT NULL,
    &&[NombreDepartamento] NCHAR (120) NOT NULL,
    &&[IdPais]             INT        NULL,
    &&PRIMARY KEY CLUSTERED ([IdDepartamento] ASC),
    &&CONSTRAINT [FK_dbo.Departamentos_dbo.Paises_IdPais] FOREIGN KEY ([IdPais]) REFERENCES [dbo].[Paises] ([IdPais])
	&&);
	*a partir de la tabla sSourceTablePathAndName -normalmente, DEPTOS-  del sistema Sivigila Escritorio

	LOCAL sSQLScript as String
	sSQLScript  = ''
	
	LOCAL sInsertSQLCmd as String 
	sInsertSQLCmd = ''
	
	LOCAL sSourceTableName as String
	sSourceTableName = UPPER(JUSTFNAME(sSourceTablePathAndName))
	
	LOCAL sOldDate as String
	sOldDate =SET("Date")

	SET DATE YMD

	IF VARTYPE(nInitRec)!="N" THEN
		nInitRec = 1
	ENDIF
	IF VARTYPE(sFilter)!="C" THEN
		sFilter = '.T.'
	ENDIF

	sTbTmpSourceRecords = SYS(2023) + "\rsTablaFuente"
	SELECT * FROM [&sSourceTablePathAndName] WHERE &sFilter INTO TABLE &sTbTmpSourceRecords READWRITE 
	IF VARTYPE(nRequiredRows)!="N" THEN
		nRequiredRows = RECCOUNT()
	ENDIF

	SELECT rsTablaFuente
	GO nInitRec 
	FOR iRow=1 TO nRequiredRows 
		DO CASE
			CASE sSourceTableName = 'PAISES'
				sInsertSQLCmd = "INSERT INTO dbo.Paises (IdPais, NombrePais) " +;
						" VALUES (" +;
						ALLTRIM(STR(VAL(rsTablaFuente.CODNUMPAIS))) + ",'" +;
						ALLTRIM(rsTablaFuente.PAIS) + "'" +;
						+ ")"
			CASE sSourceTableName = 'DEPTOS'
				sInsertSQLCmd = "INSERT INTO dbo.Departamentos (IdDepartamento, NombreDepartamento, IdPais) " +;
						" VALUES (" +;
						ALLTRIM(STR(VAL(rsTablaFuente.COD_DEP))) + ",'" +;
						ALLTRIM(rsTablaFuente.NOM_DEP) + "'," +;
						'170' +;
						+ ")"
		ENDCASE
		
		sSQLScript = sSQLScript + sInsertSQLCmd + CrLf
		SKIP
	NEXT iRow

	*Crea departamentos desconocidos para asignarlos a los países distintos a Colombia
	SELECT '1' + CODNUMPAIS AS IdDepartamento, ALLTRIM(PAIS) + ' - DEPARTAMENTO DESCONOCIDO' AS NombreDepartamento, ;
		ALLTRIM(STR(VAL(CODNUMPAIS))) AS IdPais FROM Paises WHERE !EMPTY(CODNUMPAIS) INTO CURSOR rsDepartamentosPaisesExtranjeros
	SELECT rsDepartamentosPaisesExtranjeros
	SCAN
		sInsertSQLCmd = "INSERT INTO dbo.Departamentos (IdDepartamento, NombreDepartamento, IdPais) " +;
				" VALUES (" +;
				rsDepartamentosPaisesExtranjeros.IdDepartamento + ",'" +;
				ALLTRIM(rsDepartamentosPaisesExtranjeros.NombreDepartamento) + "'," +;
				ALLTRIM(rsDepartamentosPaisesExtranjeros.IdPais) +;
				+ ")"
		sSQLScript = sSQLScript + sInsertSQLCmd + CrLf
	ENDSCAN 
	
	=STRTOFILE(sSQLScript, 'Sivigila_web_Datos_' + sSourceTableName + '.sql')
	
	SET DATE &sOldDate 
ENDPROC


PROCEDURE makeSivigilaWebData_MUNICIPIOS_V01(sSourceTablePathAndName as String, nRequiredRows as Number, sFilter as String, nInitRec as number)
	*Crea un script con instrucciones INSERT para SQLServer que permiten poblar una tabla Municipios definida así:
	&&CREATE TABLE [dbo].[Municipios] (
    &&[IdMunicipio]     INT         NOT NULL,
    &&[NombreMunicipio] NCHAR (100) NOT NULL,
    &&[IdDepartamento]  INT         NOT NULL,
    &&[EsDistrito]      BIT         DEFAULT ((0)) NOT NULL,
    &&PRIMARY KEY CLUSTERED ([IdMunicipio] ASC),
    &&CONSTRAINT [FK_dbo.Municipios_dbo.Departamentos_IdDepartamento] FOREIGN KEY ([IdDepartamento]) REFERENCES [dbo].[Departamentos] ([IdDepartamento])
	&&);
	*a partir de la tabla sSourceTablePathAndName -normalmente, MUNICIPIOS-  del sistema Sivigila Escritorio

	LOCAL sSQLScript as String
	sSQLScript  = ''
	
	LOCAL sInsertSQLCmd as String 
	sInsertSQLCmd = ''
	
	LOCAL sSourceTableName as String
	sSourceTableName = UPPER(JUSTFNAME(sSourceTablePathAndName))
	
	LOCAL sOldDate as String
	sOldDate =SET("Date")

	SET DATE YMD

	IF VARTYPE(nInitRec)!="N" THEN
		nInitRec = 1
	ENDIF
	IF VARTYPE(sFilter)!="C" THEN
		sFilter = 'COD_DANE!="99999"'
	ENDIF

	sTbTmpSourceRecords = SYS(2023) + "\rsTablaFuente"
	SELECT *, LEFT(COD_MUN,2) AS IdDepto FROM [&sSourceTablePathAndName] WHERE &sFilter INTO TABLE &sTbTmpSourceRecords READWRITE 
	IF VARTYPE(nRequiredRows)!="N" THEN
		nRequiredRows = RECCOUNT()
	ENDIF

	SELECT rsTablaFuente
	GO nInitRec 
	FOR iRow=1 TO nRequiredRows 
		DO CASE
			CASE sSourceTableName = 'PAISES'
				sInsertSQLCmd = "INSERT INTO dbo.Paises (IdPais, NombrePais) " +;
						" VALUES (" +;
						ALLTRIM(STR(VAL(rsTablaFuente.CODNUMPAIS))) + ",'" +;
						ALLTRIM(rsTablaFuente.PAIS) + "'" +;
						+ ")"
			CASE sSourceTableName = 'MUNICIPIOS'
				sInsertSQLCmd = "INSERT INTO dbo.Municipios (IdMunicipio, NombreMunicipio, IdDepartamento) " +;
						" VALUES (" +;
						ALLTRIM(STR(VAL(rsTablaFuente.COD_DANE))) + ",'" +;
						ALLTRIM(rsTablaFuente.NOM_MUN) + "'," +;
						ALLTRIM(STR(VAL(IdDepto))) +;
						+ ")"
		ENDCASE
		
		sSQLScript = sSQLScript + sInsertSQLCmd + CrLf
		SKIP
	NEXT iRow
*SET STEP ON 
	*Crea municipios desconocidos para asignarlos a los países distintos a Colombia
	SELECT '1' + CODNUMPAIS AS IdDepto, ALLTRIM(PAIS) + ' - MUNICIPIO DESCONOCIDO' AS NombreMunicipio, ;
		'1' + CODNUMPAIS + '001' AS IdMunicipio FROM Paises WHERE !EMPTY(CODNUMPAIS) INTO CURSOR rsMunicipiosPaisesExtranjeros
	SELECT rsMunicipiosPaisesExtranjeros
	SCAN
		sInsertSQLCmd = "INSERT INTO dbo.Municipios (IdMunicipio, NombreMunicipio, IdDepartamento) " +;
				" VALUES (" +;
				rsMunicipiosPaisesExtranjeros.IdMunicipio + ",'" +;
				ALLTRIM(rsMunicipiosPaisesExtranjeros.NombreMunicipio) + "'," +;
				ALLTRIM(rsMunicipiosPaisesExtranjeros.IdDepto) +;
				+ ")"
		sSQLScript = sSQLScript + sInsertSQLCmd + CrLf
	ENDSCAN 
	
	=STRTOFILE(sSQLScript, 'Sivigila_web_Datos_' + sSourceTableName + '.sql')
	
	SET DATE &sOldDate 
ENDPROC


PROCEDURE launchProc
	LPARAMETERS sProgramOrFunction as String, sParameters as string, sLibName as String

	sProgramOrFunction = UPPER(sProgramOrFunction)

	DO CASE 
		CASE sProgramOrFunction = 'EXPORTAR_A_XLS'
			_ver=''
			sFollowupEvents=' 650 652 680 860 870 890 995 353 354 359 362 349 '
			nMajor = 18
			nMinor = 1
			nBuild = 5
			
			sCmdToExec = "DO " + sProgramOrFunction
			IF VARTYPE(sParameters) = 'C' AND !EMPTY(sParameters) THEN 
				sCmdToExec = sCmdToExec + " WITH " + sParameters
			ENDIF
			IF VARTYPE(sLibName) = 'C' AND !EMPTY(sLibName) THEN 
				sCmdToExec = sCmdToExec + " IN '" + sLibName + "'"
			ENDIF
			
			&sCmdToExec 
			
			RELEASE _ver, nMajor ,nMinor ,nBuild 
			RELEASE sFollowupEvents
			
		CASE sProgramOrFunction = 'MAKENOTIFICATION'
			_ver=''
			nMajor=18
			nMinor=1
			nBuild=5

			sCmdToExec = "DO " + sProgramOrFunction
			IF VARTYPE(sParameters) = 'C' AND !EMPTY(sParameters) THEN 
				sCmdToExec = sCmdToExec + " WITH " + sParameters
			ENDIF
			IF VARTYPE(sLibName) = 'C' AND !EMPTY(sLibName) THEN 
				sCmdToExec = sCmdToExec + " IN '" + sLibName + "'"
			ENDIF
			
			&sCmdToExec 
			
			RELEASE _ver, nMajor ,nMinor ,nBuild 

		CASE sProgramOrFunction = 'EXPORTAR'
			SET DELETED ON 
			SET SAFETY OFF 
			
			OPEN DATABASE BDSivigila
			
			_dir = SYS(2023)
			_tip_uni = '1'
			_cod_usu='SIVIGILA'
			_vigencia='2020'
			_rta_planos = '\tmp\planos_de_prueba'
			STORE 0 TO tot_rab, tot_pte, tot_lab, tot_bro, tot_upgd, tot_rh, tot_DC
			*_ver=''
			*nMajor=18
			*nMinor=1
			*nBuild=5

			sCmdToExec = "DO " + sProgramOrFunction
			IF VARTYPE(sParameters) = 'C' AND !EMPTY(sParameters) THEN 
				sCmdToExec = sCmdToExec + " WITH " + sParameters
			ENDIF
			IF VARTYPE(sLibName) = 'C' AND !EMPTY(sLibName) THEN 
				sCmdToExec = sCmdToExec + " IN '" + sLibName + "'"
			ENDIF
			
			&sCmdToExec 
			
			SET DATABASE TO BDSivigila
			CLOSE  DATABASE 
			RELEASE _dir,_ver, nMajor ,nMinor ,nBuild 

	ENDCASE

ENDPROC


PROCEDURE makeSivigilaWebData_ConfiguracionDeObjetos(sFilter as String)

	*Crea un archivo delimitado CSV para la tabla ConfiguracionDeObjetos con valores significativos para el campo monb_ctrl
	
	LOCAL sSourceTableName as String
	sSourceTableName = 'ConfiguracionDeObjetos'
	
	LOCAL sOlDel as String
	sOlDel = SET("Deleted")
	SET DELETED ON
	
	IF EMPTY(sFilter) THEN 
		sFilter = '.T.'
	ENDIF
	
	SELECT * FROM &sSourceTableName WHERE &sFilter INTO TABLE ConfiguracionDeObjetos_4_0
	
	SELECT ConfiguracionDeObjetos_4_0
	REPLACE nomb_ctrl WITH 'TxtSEXO' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='sx' 
	REPLACE nomb_ctrl WITH 'TxCON_FIN' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='c' 
	REPLACE nomb_ctrl WITH 'TxtFEC_DEF' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='fd' 
	REPLACE nomb_ctrl WITH 'TxtCER_DEF' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='cd' 
	REPLACE nomb_ctrl WITH 'TxtCBMTE' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='cbm' 
	REPLACE nomb_ctrl WITH 'TxtTIP_CAS' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='tc' 
	REPLACE nomb_ctrl WITH 'TxtUNI_MED' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='e2' 
	REPLACE nomb_ctrl WITH 'TxtPAC_HOS' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='ht' 
	REPLACE nomb_ctrl WITH 'TxtFEC_HOS' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='fh' 
	REPLACE nomb_ctrl WITH 'TxtOcupacion' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='OCU' 
	REPLACE nomb_ctrl WITH 'LblINI_SIN' FOR  contenedor='frmDatosBasicosIndividuales' and ALLTRIM(nomb_ctrl)=='sis' 

	DO ExportMemoFields WITH 'ConfiguracionDeObjetos_4_0' IN Utilities
	
	SET DELETED &sOlDel
	
ENDPROC


PROCEDURE makeSivigilaWebData(sSourceTablePathAndName as String, sOwnerId as string, nRequiredRows as Number, nInitRec as number, ;
								sFilter as String, sNonDistinctFieldList as String, nNextID as Number)

	LOCAL sSQLScript as String
	sSQLScript  = ''
	
	LOCAL sInsertSQLCmd as String 
	sInsertSQLCmd = ''
	
	LOCAL sSourceTableName as String
	sSourceTableName = UPPER(JUSTFNAME(sSourceTablePathAndName))
	
	LOCAL sOldDate as String
	sOldDate =SET("Date")

	LOCAL oAnomaliesSolver as Object
	oAnomaliesSolver=NEWOBJECT("DuplicatesSolver")
	
	SET DATE YMD

	IF VARTYPE(nInitRec)!="N" THEN
		nInitRec = 1
	ENDIF
	IF VARTYPE(sFilter)!="C" THEN
		sFilter = '.T.'
	ENDIF

	sOldDB = SET("Database")
	OPEN DATABASE SIVIGILATemp
	SET  DATABASE TO SIVIGILATemp
	sTbTmpSourceRecords = SYS(2023) + "\rsTablaFuente"
	TRY
		REMOVE TABLE rsTablaFuente DELETE
	CATCH TO oException
	ENDTRY
	SELECT * FROM [&sSourceTablePathAndName] WHERE &sFilter INTO TABLE &sTbTmpSourceRecords DATABASE SIVIGILATemp

	*Elimina registros duplicados de los registros fuente
	IF VARTYPE(sNonDistinctFieldList)='C' AND !EMPTY(sNonDistinctFieldList) THEN 
		oAnomaliesSolver.sSourceTableName = "rsTablaFuente"
		oAnomaliesSolver.bIsCursor = .T.
		oAnomaliesSolver.sExcludedFields = ' ' + STRTRAN(sNonDistinctFieldList, ',', ' ') + ' '
		oAnomaliesSolver.deleteDuplicates()
	ENDIF
	RELEASE oAnomaliesSolver
	
	SELECT rsTablaFuente
	IF VARTYPE(nRequiredRows)!="N" THEN
		nRequiredRows = RECCOUNT()
	ENDIF
	
	GO nInitRec 
	FOR iRow=1 TO nRequiredRows 
		DO CASE
			CASE sSourceTableName = 'PACIENTE'
				sInsertSQLCmd = "INSERT INTO dbo.Pacientes (owner,TipoDocumento,NumeroDocumento,PrimerNombre,SegundoNombre,PrimerApellido,SegundoApellido," +;
						"FechaNacimiento,Sexo,PertenenciaEtnica,TelefonoFijo,TelefonoCelular,DireccionDeResidenciaContacto1,MunicipioResidencia,IdGrupoEtnico)  " +;
						" VALUES ('" +;
						sOwnerId + "','" +;
						ALLTRIM(rsTablaFuente.TIP_IDE) + "','" +;
						ALLTRIM(rsTablaFuente.NUM_IDE) + "','" +;
						ALLTRIM(rsTablaFuente.PRI_NOM) + "','" +;
						ALLTRIM(rsTablaFuente.SEG_NOM) + "','" +;
						ALLTRIM(rsTablaFuente.PRI_APE) + "','" +;
						ALLTRIM(rsTablaFuente.SEG_APE) + "','" +;
						STRTRAN(DTOC(rsTablaFuente.FECHA_NTO),'/') + "','" +;
						ALLTRIM(IIF(rsTablaFuente.SEXO='M','H',IIF(rsTablaFuente.SEXO='F','M',rsTablaFuente.SEXO))) + "','" +;
						ALLTRIM(rsTablaFuente.PER_ETN) + "','" +;
						ALLTRIM(rsTablaFuente.TELEFONO) + "','" +;
						'300' + ALLTRIM(rsTablaFuente.TELEFONO) + "','" +;
						ALLTRIM(rsTablaFuente.DIR_RES) + "','" +;
						ALLTRIM(rsTablaFuente.MUN_PRO) + "'," +;
						IIF(EMPTY(GRUPO_ETNI),'999',GRUPO_ETNI) +;
						+ ")"
						
			CASE sSourceTableName = 'GRUPOSETNICOS'
				sInsertSQLCmd = "INSERT INTO dbo.GruposEtnicos (IdGrupo, Nombre, IdOficial, Activo, FechaInactivacion) " +;
						" VALUES (" +;
						ALLTRIM(STR(VAL(rsTablaFuente.COD_GRUPO))) + ",'" +;
						ALLTRIM(rsTablaFuente.NOM_GRUPO) + "',NULL," +;
						ALLTRIM(IIF(rsTablaFuente.ACTIVO,'1','0')) + ",NULL" +;
						+ ")"
						
			CASE sSourceTableName = 'PAISES'
				sInsertSQLCmd = "INSERT INTO dbo.Paises (codNumPais, Pais, CodPais3, CodPais2, Activo, Fec_inact) " +;
						" VALUES ('" +;
						ALLTRIM(rsTablaFuente.CODNUMPAIS) + "','" +;
						ALLTRIM(rsTablaFuente.PAIS) + "','" +;
						ALLTRIM(rsTablaFuente.CodPais3) + "','" +;
						ALLTRIM(rsTablaFuente.CodPais2) + "'," +;
						IIF(rsTablaFuente.ACTIVO,'1','0') + "," +;
						IIF(EMPTY(rsTablaFuente.FEC_INACT),'NULL',"'"+DTOC(rsTablaFuente.FEC_INACT)+"'")  +;
						+ ")"

			CASE sSourceTableName = 'VEREDAS'
				sInsertSQLCmd = "INSERT INTO dbo.Veredas (idVereda, NombreVereda, IdMunicipio) " +;
						" VALUES (" +;
						"" + ALLTRIM(STR(VAL(rsTablaFuente.id),25,0)) + "," +;
						"'" + ALLTRIM(rsTablaFuente.nombre) + "'," +;
						"" + ALLTRIM(STR(VAL(rsTablaFuente.cod_dep+rsTablaFuente.cod_mun),25,0)) +;
						+ ")"

			CASE sSourceTableName = 'EVENTOS'
				sInsertSQLCmd = "INSERT INTO dbo.TipoEvento (id, cod_eve, nom_eve, inmediata, tip_not, cla_per, gru_per, exa_lab,  " +;
								"con_final, val_sem, flu_not, eve_nal, fec_obliga, inicio_sin, estado_eve, activo, admite_ni,  " +;
								"latencia_dc, ajustes_na, edad_max, unidad_med, edad_min, unimed_min, lab_incrus, dc_form,  " +;
								"idTablaEvento, control, dias_ajuste, OWNER)  " +;
						" VALUES (" +;
						ALLTRIM(STR(nNextID)) + "," +;
						"'" + ALLTRIM(rsTablaFuente.cod_eve) + "'," +;
						"'" + ALLTRIM(rsTablaFuente.nom_eve) + "'," +;
						"'" + ALLTRIM(rsTablaFuente.inmediata) + "'," +;
						"'" + ALLTRIM(STR(rsTablaFuente.tip_not)) + "'," +;
						"'" + ALLTRIM(rsTablaFuente.cla_per) + "'," +;
						"'" + ALLTRIM(rsTablaFuente.gru_per) + "'," +;
						"'" + ALLTRIM(STR(rsTablaFuente.exa_lab)) + "'," +;
						"'" + ALLTRIM(STR(rsTablaFuente.con_final)) + "'," +;
						"'" + ALLTRIM(rsTablaFuente.val_sem) + "'," +;
						"" + ALLTRIM(STR(rsTablaFuente.flu_not)) + "," +;
						"" + ALLTRIM(STR(rsTablaFuente.eve_nal)) + "," +;
						"'" + STRTRAN(DTOC(rsTablaFuente.fec_obliga),'/') + "'," +;
						"" + IIF(rsTablaFuente.inicio_sin,'1','0') + "," +;
						"'" + ALLTRIM(rsTablaFuente.estado_eve) + "'," +;
						"" + IIF(rsTablaFuente.activo,'1','0') + "," +;
						"" + IIF(rsTablaFuente.admite_ni,'1','0') + "," +;
						"" + ALLTRIM(STR(rsTablaFuente.latencia_dc)) + "," +;
						"'" + ALLTRIM(rsTablaFuente.ajustes_na) + "'," +;
						"'" + ALLTRIM(rsTablaFuente.edad_max) + "'," +;
						"'" + ALLTRIM(rsTablaFuente.unidad_med) + "'," +;
						"'" + ALLTRIM(rsTablaFuente.edad_min) + "'," +;
						"'" + ALLTRIM(rsTablaFuente.unimed_min) + "'," +;
						"" + IIF(rsTablaFuente.lab_incrus,'1','0') + "," +;
						"'" + ALLTRIM(rsTablaFuente.dc_form) + "'," +;
						"" + ALLTRIM(IIF(EMPTY(rsTablaFuente.formulario),'0',rsTablaFuente.formulario)) + "," +;
						"" + IIF(rsTablaFuente.Control,'1','0') + "," +;
						"" + ALLTRIM(STR(rsTablaFuente.dias_ajuste)) + "," +;
						"'" + sOwnerId + "'" +;
						+ ")"
			CASE sSourceTableName = 'UPGDSREFERENCIA'
				sInsertSQLCmd = "INSERT INTO dbo.UpgdsReferencia (CodigoUPGD, Raz_soc) " +;
						" VALUES ('" +;
						rsTablaFuente.CodigoUPGD + "','" +;
						ALLTRIM(rsTablaFuente.Raz_soc) + "'" +;
						+ ")"

		ENDCASE

		sSQLScript = sSQLScript + sInsertSQLCmd + CrLf
		IF VARTYPE(nNextID) = 'N' THEN 
			nNextID = nNextID + 1
		ENDIF 
		TRY 
			SKIP
		CATCH TO oException
		ENDTRY 
	NEXT iRow
	=STRTOFILE(sSQLScript, 'Sivigila_web_Datos_' + sSourceTableName + '.sql')
	
	SET DATE &sOldDate 
ENDPROC


PROCEDURE makeSivigilaWebData_MUNICIPIOS(sSourceTablePathAndName as String, nRequiredRows as Number, sFilter as String, nInitRec as number)
	*Crea un script con instrucciones INSERT para SQLServer que permiten poblar una tabla Municipios definida así:
	&&CREATE TABLE [dbo].[Municipios] (
    &&[IdMunicipio]     INT         NOT NULL,
    &&[NombreMunicipio] NCHAR (100) NOT NULL,
    &&[IdDepartamento]  INT         NOT NULL,
    &&[EsDistrito]      BIT         DEFAULT ((0)) NOT NULL,
    &&PRIMARY KEY CLUSTERED ([IdMunicipio] ASC),
    &&CONSTRAINT [FK_dbo.Municipios_dbo.Departamentos_IdDepartamento] FOREIGN KEY ([IdDepartamento]) REFERENCES [dbo].[Departamentos] ([IdDepartamento])
	&&);
	*a partir de la tabla sSourceTablePathAndName -normalmente, MUNICIPIOS-  del sistema Sivigila Escritorio

	LOCAL sSQLScript as String
	sSQLScript  = ''
	
	LOCAL sInsertSQLCmd as String 
	sInsertSQLCmd = ''
	
	LOCAL sSourceTableName as String
	sSourceTableName = UPPER(JUSTFNAME(sSourceTablePathAndName))
	
	LOCAL sOldDate as String
	sOldDate =SET("Date")

	SET DATE YMD

	IF VARTYPE(nInitRec)!="N" THEN
		nInitRec = 1
	ENDIF
	IF VARTYPE(sFilter)!="C" THEN
		sFilter = 'COD_DANE!="99999"'
	ENDIF

	sTbTmpSourceRecords = SYS(2023) + "\rsTablaFuente"
	SELECT *, LEFT(COD_MUN,2) AS IdDepto FROM [&sSourceTablePathAndName] WHERE &sFilter INTO TABLE &sTbTmpSourceRecords READWRITE 
	IF VARTYPE(nRequiredRows)!="N" THEN
		nRequiredRows = RECCOUNT()
	ENDIF

	SELECT rsTablaFuente
	GO nInitRec 
	FOR iRow=1 TO nRequiredRows 
		DO CASE
			CASE sSourceTableName = 'PAISES'
				sInsertSQLCmd = "INSERT INTO dbo.Paises (IdPais, NombrePais) " +;
						" VALUES (" +;
						ALLTRIM(STR(VAL(rsTablaFuente.CODNUMPAIS))) + ",'" +;
						ALLTRIM(rsTablaFuente.PAIS) + "'" +;
						+ ")"
			CASE sSourceTableName = 'MUNICIPIOS'
				sInsertSQLCmd = "INSERT INTO dbo.Municipios (cod_mun, nom_mun, cod_dane, altitud, latitud, longitud, cod_pais, cod_dep, activo, esDistrito) " +;
						" VALUES ('" +;
						rsTablaFuente.cod_mun + "','" +;
						ALLTRIM(rsTablaFuente.nom_mun) + "','" +;
						ALLTRIM(rsTablaFuente.cod_dane) + "'," +;
						ALLTRIM(STR(rsTablaFuente.altitud)) + "," +;
						ALLTRIM(STR(rsTablaFuente.latitud,10,6)) + "," +;
						ALLTRIM(STR(rsTablaFuente.longitud,10,6)) + ",'" +;
						ALLTRIM(rsTablaFuente.cod_pais) + "','" +;
						ALLTRIM(rsTablaFuente.cod_dep) + "'," +;
						IIF(rsTablaFuente.ACTIVO,'1','0') + "," +;
						'0' +; &&esDistrito por default es cero
						+ ")"
		ENDCASE
		
		sSQLScript = sSQLScript + sInsertSQLCmd + CrLf
		SKIP
	NEXT iRow
*SET STEP ON 
	=STRTOFILE(sSQLScript, 'Sivigila_web_Datos_' + sSourceTableName + '.sql')
	
	SET DATE &sOldDate 
ENDPROC

PROCEDURE deleteMunicipios
	TRY
		USE municipios IN 0
	CATCH TO oException
	ENDTRY

	SELECT STRTRAN(nom_mun,'*') as nom_mun,RECNO() as nReg, '0' AS BORRADO FROM  municipios WHERE 'SIN ESPECIFICACIÓN ADICIONAL DE LUGAR' $ nom_mun ORDER BY nom_mun INTO CURSOR rsMunicipiosDuplicados READWRITE 
	SELECT rsMunicipiosDuplicados
	INDEX on nReg TAG nReg_idx
	sToRetain = rsMunicipiosDuplicados.Nom_mun
*SET STEP ON 	
	DO WHILE !EOF()
		SKIP 
		IF sToRetain != rsMunicipiosDuplicados.Nom_mun THEN 
			sToRetain = rsMunicipiosDuplicados.Nom_mun
		ELSE
			replace BORRADO WITH '1'
		ENDIF 
	ENDDO 
	SELECT municipios 
	SET RELATION TO RECNO() INTO rsMunicipiosDuplicados
	DELETE ALL FOR rsMunicipiosDuplicados.BORRADO = '1'
	DELETE FOR cod_mun $ '1E000,1Z000,6I000,6L000,8Y000,A0000,B1000,AR000,BP000,CF000,2T000,4B000'
ENDPROC 


PROCEDURE CheckDBProperty(sPropertyName as String)

	CLEAR
	SET ALTERNATE TO PropExistenceChecked 
	SET ALTERNATE ON
	
	OPEN DATABASE bdsivigila

	FOR i=1 TO n_SIVIGILA_EVENTS
		sTargetTableName="EVENTOS_" + PADL(ALLTRIM(STR(i)),2,'0')
		TRY
			?"Tabla: " + sTargettableName + ' - ' + DBGETPROP(sTargetTableName,'TABLE',sPropertyName)
		CATCH TO oException
			
		ENDTRY
		
	NEXT i
	SET ALTERNATE OFF	
ENDPROC


PROCEDURE jsonHandlerTst()
	*Muestra cómo utilizar la librería vfpJson según se encuentra en https://github.com/sait/vfpjson
	
	set procedure TO json additive

	oJ=json_decode('{"ajuste3":{"tip_cas":["3"],"Dependientes":["SEG_CONT"]},"ajuste6":{"tip_cas":["6"],"Dependientes":["CRI_DES"]}}')
	? oJ.get('ajuste3')
	*? oJ.get('ajuste3').get('tip_cas').get(1)
	?oJ._ajuste3._tip_cas.get(1)
	?oJ._ajuste3._dependientes.get(1)
ENDPROC 




PROCEDURE RHandlerTst()
	*Base articles
	*https://colinfay.me/writing-r-extensions/linking-guis-and-other-front-ends-to-r.html
	*https://www.r-project.org/conferences/DSC-2001/Proceedings/NeuwirthBaier.pdf
	
	*(D)COM package in https://cran.r-project.org/contrib/extra/dcom/
	
	LOCAL oR 
	
SET STEP ON 
	oR = CreateObject("StatConnectorSrv.StatConnector")
	oR.Init("R")
	?oR.Evaluate("2+2")
	RETURN 
	
	oR = NewObject("AutomationServer", "AutomationServer.fxp", "",  "StatConnectorSrv.StatConnector", .T.)
		
	IF VarType(oR.oServer)<>"O"
		?'Se presentó un error'
	ELSE
		oR.oServer.Init("R")
		oR.oServer.Close
	ENDIF

	RELEASE oR 	
ENDPROC

	
PROCEDURE createCRUDForm()

	LOCAL oForm as Object 
	
	oForm = CREATEOBJECT('frm_crud_SingleTable')  && Create a Form
	oForm.Save
	
	RELEASE oForm
	RELEASE oHelpIniMgr

ENDPROC 


PROCEDURE connectToSQLServer()

	lcStringCnxRemoto = "DRIVER={SQL Server};" + ;
	                    "SERVER=10.10.100.25\DESARROLLO;" + ;
	                    "PORT=3306;" + ;
	                    "UID=SivigilaDesktop;" + ;
	                    "PWD=RailwayStat3167;" + ;
	                    "DATABASE=sivigila;" 
SET STEP ON 

	SQLSETPROP(0,"DispLogin" , 3 )
	lnHandle = SQLSTRINGCONNECT(lcStringCnxRemoto)
	IF lnHandle > 0
		cmd = SQLEXEC(lnHandle,"SELECT top 500 * FROM Eventos","rsDB")
		IF cmd > 0
			BROWSE
		ELSE
			AERROR(laErr)
			MESSAGEBOX("No se pudo conectar a SQL Server. Error: " + CHR(13) + laErr[2])
		ENDIF
		IF cmd > 0
			USE IN rsDB
		ENDIF 
		SQLDISCONNECT(lnHandle)
	ELSE
		AERROR(laErr)
		MESSAGEBOX("No se pudo conectar a SQL Server. Error: " + CHR(13) + laErr[2])
	ENDIF
ENDPROC
