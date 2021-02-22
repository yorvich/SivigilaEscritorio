#INCLUDE SIVIGILA.H

LPARAMETERS txtDataLoaded as Byte, bBeSilent as Boolean, nResultsFileHandler as Number  

* txtDataLoaded será igual a 1, si los registros de los archivos planos fueron cargados en las tablas intermedias de consolidación;
en caso contrario será 0

* bBeSilent (opcional): indica si el procesamiento debe hacerse en modo silencioso o verboso (valor por defecto)

* nResultsFileHandler (opcional): si el procesamiento es verboso, handler para el archivo de resultados hacia donde deben dirigirse ;
las salidas que normalmente se dirigien a la interfaz de usuario 


SET PROCEDURE TO plainsLib ADDITIVE

ruta_a_planos = ubica
ARC_MAE       = RIGHT(UBICA,12)

DO cleanTheHouse
IF validateControlFile(UBICA, bBeSilent, nResultsFileHandler)
	IF loadControlFile(bBeSilent, nResultsFileHandler)
		UBICA=SUBSTR(UBICA,1,LEN(UBICA)-12)
		DO LoadAndValidatePlains WITH bBeSilent, nResultsFileHandler
		txtDataLoaded = 1
	ELSE
		txtDataLoaded = 0
		CLOSE TABLES ALL
	ENDIF
ENDIF



*---------------------------------------------------------------------------------
PROCEDURE LoadAndValidatePlains

	LPARAMETERS bBeSilent as Boolean, nResultsFileHandler as Number 
	
	SET PROCEDURE TO plainsLib	ADDITIVE
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	
	* Se crean las tablas de importacion de acuerdo con la version del sistema con la que ;
	fueron generados los archivos que se importan
	DO makeImportTables WITH _RTA_CONSOLIDA, n_version, nMajor, nMinor, nBuild

	CLOSE TABLES ALL

	*=SelectTable('EVENTOS',,.F.)
	*=SelectTable(_RTA_CONSOLIDA+'\ERRORES',,.F.)

	*Establece el orden en que deben procesarse los archivos planos según se indica:	;
		Nombre				Orden;
		D*.txt, I*.txt,		1	;
		N*.txt				2	;
		B*.txt				3	;
		U*.txt				4	;
		R*.txt				5	;
		*.*					6
	SELECT	*,IIF(LIKE('D*',ARCHIVO) OR LIKE('I*',ARCHIVO),1, ;
			IIF(LIKE('N*',ARCHIVO),2,IIF(LIKE('B*',ARCHIVO),3,IIF(LIKE('U*',ARCHIVO),4,IIF(LIKE('R*',ARCHIVO),5,6))))) as nPriority ;
			FROM _RTA_CONSOLIDA + '\PLANO_CONTROL' ORDER BY nPriority INTO CURSOR rsPLANO_CONTROL
	SELECT rsPLANO_CONTROL
	RUT_BAC=ALLTRIM(COD_PRE)+ALLTRIM(COD_SUB)+'_'+SUBSTR(ARC_MAE,7,2)

	SET DATE TO YMD
	SET MARK TO '-'
	NA_DATA='' && NOMBRE DEL ARCHIVO DE DATOS
	NA_DATB='' && NOMBRE DEL ARCHIVO DE DATOS

	SCAN
		SCATTER MEMVAR
		NREG=RECNO()
		NOM_ARC=ALLTRIM(UBICA)+ARCHIVO
		R_IMP=0
		IF !FILE(NOM_ARC)
	      	INSERT INTO ERRORES (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, VARIABLE);
	        	VALUES (m.ARCHIVO, NREG, 'Control', m.COD_PRE, m.COD_SUB, 'No se encuentra el archivo '+m.ARCHIVO)
		ELSE
			DO CASE
				CASE LEFT(ARCHIVO,1)='U'
					DO processUPGD WITH bBeSilent, nResultsFileHandler

				CASE LEFT(ARCHIVO,1)='R'
					DO processTH WITH bBeSilent, nResultsFileHandler

				CASE LEFT(ARCHIVO,1)='V'
					DO processUCI WITH bBeSilent, nResultsFileHandler

				CASE LEFT(ARCHIVO,1)='N'
					DO processIndividual WITH bBeSilent, nResultsFileHandler

			  	CASE LEFT(ARCHIVO,1)='B'
				  	DO processCollective WITH bBeSilent, nResultsFileHandler

		  		CASE LEFT(ARCHIVO,1)='D'
				  	DO processComplementary WITH bBeSilent, nResultsFileHandler
			  	
		  		CASE LEFT(ARCHIVO,1)='I'
					* Archivo contenador de datos colectivo Morbilidad por IRA (995, 43)
					* a partir de la versión 2010
					DO processIRA WITH bBeSilent, nResultsFileHandler
			  		
			ENDCASE
		ENDIF
		SELECT rsPLANO_CONTROL
	ENDSCAN
	
	*Crea una tabla con las UPGDs que tienen datos en alguno de los archivos planos N, D, B o I;
	de tal forma que esas UPGDs se activen automáticamente en el sistema. Este tratamiento es necesario;
	por cuanto existen situaciones en donde el archivo U no es procesado por processUPGD por no venir;
	relacionado en el archivo de control C
	bNFileIsPresent=.T.
	sSourcePlainFile=_RTA_CONSOLIDA + "\PLANO_PACIENTES.DBF"
	sSourcePlainTable="PLANO_PACIENTES"
	IF !USED(sSourcePlainTable) THEN
		IF FILE(sSourcePlainFile) THEN
			USE (sSourcePlainFile) IN 0
		ELSE
			bNFileIsPresent=.F.
		ENDIF
	ENDIF
	
	bDFileIsPresent=.T.
	sSourcePlainFile=_RTA_CONSOLIDA + "\PLANO_EVENTOS.DBF"
	sSourcePlainTable="PLANO_EVENTOS"
	IF !USED(sSourcePlainTable) THEN
		IF FILE(sSourcePlainFile) THEN
			USE (sSourcePlainFile) IN 0
		ELSE
			bDFileIsPresent=.F.
		ENDIF
	ENDIF

	bIFileIsPresent=.T.
	sSourcePlainFile=_RTA_CONSOLIDA + "\PLANO_E43.DBF"
	sSourcePlainTable="PLANO_E43"
	IF !USED(sSourcePlainTable) THEN
		IF FILE(sSourcePlainFile) THEN
			USE (sSourcePlainFile) IN 0
		ELSE
			bIFileIsPresent=.F.
		ENDIF
	ENDIF

	bBFileIsPresent=.T.
	sSourcePlainFile=_RTA_CONSOLIDA + "\PLANO_BROTES.DBF"
	sSourcePlainTable="PLANO_BROTES"
	IF !USED(sSourcePlainTable) THEN
		IF FILE(sSourcePlainFile) THEN
			USE (sSourcePlainFile) IN 0
		ELSE
			bBFileIsPresent=.F.
		ENDIF
	ENDIF

	sSQLClause=''
	IF bNFileIsPresent THEN
		sSQLClause = "SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_PACIENTES "
	ENDIF
	
	IF bDFileIsPresent THEN
		sSQLClause = sSQLClause + " UNION SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_EVENTOS "
	ENDIF
	
	IF bIFileIsPresent THEN
		sSQLClause = sSQLClause + " UNION SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_E43 "
	ENDIF

	IF bBFileIsPresent THEN
		sSQLClause = sSQLClause + " UNION SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_BROTES "
	ENDIF
	
	IF !EMPTY(sSQLClause) THEN
		sSQLClause = sSQLClause + "	INTO TABLE T_ReportingUPGDs"
	ELSE
		sSQLClause = "SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_CONTROL WHERE .F. INTO TABLE T_ReportingUPGDs"
	ENDIF
	&sSQLClause

	DO produceSummary

	CLOSE TABLES ALL
	WAIT "Se han cargado los archivos planos"  WINDOW TIMEOUT 1

ENDPROC

*-----------------------------------------------------------------------------------------------------
PROCEDURE produceSummary
	* Genera el conteo de los registros que se llevaron a las tablas 'PLANO_...' desde los archivos texto
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	
	makeSummary=.T.
	IF !USED('PLANO_UPGD') THEN
		IF FILE([&_RTA_CONSOLIDA\PLANO_UPGD.DBF]) THEN	
			USE [&_RTA_CONSOLIDA\PLANO_UPGD]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_UPGD
	ENDIF
	IF makeSummary THEN
		TOT_UPGD = RECCOUNT()
		UPDATE [&_RTA_CONSOLIDA\PLANO_CONTROL] SET REG_IMP = TOT_UPGD WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='U'
	ENDIF
	
	makeSummary=.T.
	IF !USED('PLANO_RHS') THEN	
		IF FILE([&_RTA_CONSOLIDA\PLANO_RHS.DBF]) THEN
			USE [&_RTA_CONSOLIDA\PLANO_RHS]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT  PLANO_RHS
	ENDIF
	IF makeSummary THEN
		TOT_TH = RECCOUNT()
		UPDATE [&_RTA_CONSOLIDA\PLANO_CONTROL] SET REG_IMP = TOT_TH WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='R'
	ENDIF

	makeSummary=.T.
	IF !USED('PLANO_PACIENTES') THEN	
		IF FILE([&_RTA_CONSOLIDA\PLANO_PACIENTES.DBF]) THEN
			USE [&_RTA_CONSOLIDA\PLANO_PACIENTES]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_PACIENTES
	ENDIF
	IF makeSummary THEN
		TOT_PTE = RECCOUNT('PLANO_PACIENTES')
		UPDATE [&_RTA_CONSOLIDA\PLANO_CONTROL] SET REG_IMP = TOT_PTE WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='N'
	ENDIF

	makeSummary=.T.
	IF !USED('PLANO_LAB') THEN	
		IF FILE ([&_RTA_CONSOLIDA\PLANO_LAB.DBF]) THEN
			USE [&_RTA_CONSOLIDA\PLANO_LAB]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_LAB
	ENDIF
	IF makeSummary THEN
		TOT_LAB = RECCOUNT('PLANO_LAB')
	ENDIF

	IF SelectTable('&_RTA_CONSOLIDA\PLANO_CONTACTOS',,.F.) THEN
		TOT_CONTACTOS = RECCOUNT('PLANO_CONTACTOS')
	ENDIF

	IF SelectTable('&_RTA_CONSOLIDA\PLANO_SEGUIMIENTOS',,.F.) THEN
		TOT_SEGUIMIENTOS = RECCOUNT('PLANO_SEGUIMIENTOS')
	ENDIF

	makeSummary=.T.
	IF !USED('PLANO_EVENTOS') THEN	
		IF FILE([&_RTA_CONSOLIDA\PLANO_EVENTOS.DBF]) THEN
			USE [&_RTA_CONSOLIDA\PLANO_EVENTOS]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_EVENTOS
	ENDIF
	IF makeSummary THEN
		TOT_DAT = RECCOUNT('PLANO_EVENTOS') - TOT_LAB
		UPDATE [&_RTA_CONSOLIDA\PLANO_CONTROL] SET REG_IMP = TOT_DAT + TOT_LAB +  TOT_CONTACTOS + TOT_SEGUIMIENTOS WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='D'
	ENDIF

	makeSummary=.T.
	IF !USED('PLANO_BROTES') THEN	
		IF FILE ([&_RTA_CONSOLIDA\PLANO_BROTES.DBF]) THEN
			USE [&_RTA_CONSOLIDA\PLANO_BROTES]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_BROTES
	ENDIF
	IF makeSummary THEN
		TOT_BRO = RECCOUNT('PLANO_BROTES')
		UPDATE [&_RTA_CONSOLIDA\PLANO_CONTROL] SET REG_IMP = TOT_BRO WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='B'
	ENDIF
	
	IF n_version = _VERSION2010
		* Existe un archivo individual para Morbiliad por IRA (995,43)
		makeSummary=.T.
		IF !USED('PLANO_E43') THEN	
			IF FILE([&_RTA_CONSOLIDA\PLANO_E43.DBF]) THEN
				USE [&_RTA_CONSOLIDA\PLANO_E43]
			ELSE
				makeSummary=.F.
			ENDIF
		ELSE
			SELECT PLANO_E43
		ENDIF
		IF makeSummary THEN
			TOT_IRA = RECCOUNT()
			UPDATE [&_RTA_CONSOLIDA\PLANO_CONTROL] SET REG_IMP = TOT_IRA WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='I'
			
			* Actualiza el contador de datos complementarios teniendo en cuenta que, aún cuando los ;
			datos del evento Morbiliad por IRA - 995 vienen en un archivo individual, la transferencia hacia;
			la base de datos maestra se realiza dentro del procesamiento general de tablas de datos complementarios;
			esto es, tablas cuyo nombre es de la forma EVENTOS_xx con xx=01, 02, ..., _MaxEvents
			TOT_DAT = TOT_DAT + TOT_IRA
		ENDIF
	ENDIF

ENDPROC

*-----------------------------------------------------------------------------------------------------
PROCEDURE processComplementary


	LPARAMETERS bBeSilent as Boolean, nResultsFileHandler as Number 

	*CARGAN LOS DATOS COMPLEMENTARIOS DE TODOS LOS EVENTOS, LABORATORIOS Y BUSQUEDA ACTIVA

	TIT('Por favor espere...'+CHR(13)+'Cargando los Datos Complementarios...')
	DO CargaEventos WITH n_version, bBeSilent, nResultsFileHandler 

ENDPROC
      


*-------------------------------------------------
PROCEDURE processCollective(bBeSilent as Boolean, nResultsFileHandler as Number )
	*Carga el archivo plano B hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_BROTES'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano N

	LOCAL sErrorMsg as String 

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ADDBS(sXLSFilesPath)+"DatosColectivos", ;
		'Not. Colectiva' IN PlainsLib

	*Corrije los posibles registros duplicados que se encuentren en la tabla PLANO_BROTES con el fin de evitar ;
	la carga de duplicados
	IF n_version = _VERSION2010 THEN
		SET PROCEDURE TO AnomaliesHandler ADDITIVE
		sSourceTableName=_RTA_CONSOLIDA+'\'+sTmpTableName
		=solveDuplicates(sSourceTableName,sXLSFilesPath)
		USE (sSourceTableName)
	ENDIF
	
	*Dado que algunos campos son exportados luego de ser sometidos a transformaciones,;
	ejecuta las	transformaciones inversas necesarias para actualizar los datos recien cargados;
	de tal forma que se restablezcan sus valores originales
	IF n_version = _VERSION2008
		REPLACE MUN_PRO WITH DEP_PRO+MUN_PRO ALL
	ENDIF

	IF n_version = _VERSION2010
		REPLACE MUN_PRO WITH cod_dpto_p+ cod_mun_pr ALL
	ENDIF
*SET STEP ON 
	*Selecciona de la tabla de brotes solamente aquellos que tengan datos complementarios.
	orphansCollectivesCursorName=''
	noOrphansCollectivesCursorName=''
	SET PROCEDURE TO PlainsLib ADDITIVE
	IF !USED('PLANO_EVENTOS') THEN
		USE _RTA_CONSOLIDA + '\PLANO_EVENTOS' IN 0
	ENDIF
	nOrphansCollectives=extractOrphans(sTmpTableName,'PLANO_EVENTOS',@orphansCollectivesCursorName,@noOrphansCollectivesCursorName,n_version,2)
	
	IF nOrphansCollectives> 9 THEN
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_OUTBREAK_PLAIN_FILE_WITHOUT_CD , 0 + 16 + 256, 'Error de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_OUTBREAK_PLAIN_FILE_WITHOUT_CD )
		ENDIF
		
		orphansExcelFileName=sXLSFilesPath + '\' + sTmpTableName + '_SinComplementarios_o_Inexistentes'
		COPY TO [&orphansExcelFileName] XL5

		* Agrega los registros sin datos complementarios a la tabla de registro de ERRORES;
		(addDupsToErrorTable sirve para este propósito aún cuando su nombre no lo refleje)
		DO addDupsToErrorTable WITH orphansCollectivesCursorName, ARC_MAE, 'B', _RTA_CONSOLIDA,;
			 'ERRORES', 'UPGD DE DONDE PROVIENEN LOS REGISTROS DE BROTES SIN DATOS COMPLEMENTARIOS: ' IN PlainsLib

		markOrpahnsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
				+ ' FROM ' + orphansCollectivesCursorName+ ' WHERE ';
				+  sTmpTableName + '.ajuste  = ' + orphansCollectivesCursorName+ '.ajuste AND ';
				+  sTmpTableName + '.año     = ' + orphansCollectivesCursorName+ '.año AND ';
				+  sTmpTableName + '.semana  = ' + orphansCollectivesCursorName+ '.semana AND ';
				+  sTmpTableName + '.cod_eve = ' + orphansCollectivesCursorName+ '.cod_eve AND ';
				+  sTmpTableName + '.num_con = ' + orphansCollectivesCursorName+ '.num_con AND ';
				+  sTmpTableName + '.cod_mun = ' + orphansCollectivesCursorName+ '.cod_mun AND ';
				+  sTmpTableName + '.cod_pre = ' + orphansCollectivesCursorName+ '.cod_pre AND ';
				+  sTmpTableName + '.cod_sub = ' + orphansCollectivesCursorName+ '.cod_sub ';
		
		&markOrpahnsRecordsClause
	ENDIF


	 * SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
		LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	collectivesDuplicatesCursorName=''	
	XtractResult = extractDupsAndNotDups(noOrphansCollectivesCursorName,@collectivesDuplicatesCursorName)

	IF XtractResult = 0		&& NO SE EXTRAJERON REGISTROS
		RETURN
	ENDIF

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_OUTBREAK_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_OUTBREAK_PLAIN_FILE_WITH_DUPLICATES)		
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH collectivesDuplicatesCursorName, ARC_MAE, 'B', ;
									_RTA_CONSOLIDA, 'ERRORES' IN PlainsLib
		
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
				+ ' FROM ' + collectivesDuplicatesCursorName + ' WHERE ';
				+  sTmpTableName + '.COD_EVE = ' + collectivesDuplicatesCursorName + '.COD_EVE AND ';
				+  sTmpTableName + '.AÑO     = ' + collectivesDuplicatesCursorName + '.AÑO     AND ';
				+  sTmpTableName + '.SEMANA  = ' + collectivesDuplicatesCursorName + '.SEMANA  AND ';
				+  sTmpTableName + '.COD_MUN = ' + collectivesDuplicatesCursorName + '.COD_MUN AND ';
				+  sTmpTableName + '.NUM_CON = ' + collectivesDuplicatesCursorName + '.NUM_CON AND ';
				+  sTmpTableName + '.COD_PRE = ' + collectivesDuplicatesCursorName + '.COD_PRE AND ';
				+  sTmpTableName + '.COD_SUB = ' + collectivesDuplicatesCursorName + '.COD_SUB'
		
		&markDupsRecordsClause
	ENDIF

ENDPROC


*-------------------------------------------------
PROCEDURE processTH(bBeSilent as Boolean, nResultsFileHandler as Number )
	*Carga el archivo plano N hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_RHS'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano 
	
	LOCAL sErrorMsg as String 

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ,'Talento H/Servicios' IN PlainsLib

	*Elimina registros duplicados en la tabla PLANO_THS
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,sTmpTableName)
	sSIVIGILACurrentDir=SYS(5)+SYS(2003)
	SET DEFAULT TO (_RTA_CONSOLIDA )
	oAnomaliesSolver.sExcludedFields = " NUM_REG SUBE "
	oAnomaliesSolver.sXlsFilePath = sXLSFilesPath
	oAnomaliesSolver.deleteDuplicates()
	RELEASE oAnomaliesSolver
	SET DEFAULT TO (sSIVIGILACurrentDir)
	
	SELECT (sTmpTableName)
	R_IMP=RECCOUNT()
	tot_th=RECCOUNT()

	DELETE ALL FOR EMPTY(cod_pre)
	 
	* SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
	LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	humanResourcesDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@humanResourcesDuplicatesCursorName)

	IF XtractResult = 0		&& NO SE EXTRAJERON REGISTROS
		RETURN
	ENDIF

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_TH_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_TH_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH humanResourcesDuplicatesCursorName, ARC_MAE, 'R', ;
			_RTA_CONSOLIDA, 'ERRORES' IN PlainsLib
		
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
			+ ' FROM ' + humanResourcesDuplicatesCursorName + ' WHERE ';
			+  sTmpTableName + '.cod_pre  = ' + humanResourcesDuplicatesCursorName + '.cod_pre  AND ';
			+  sTmpTableName + '.cod_sub = ' + humanResourcesDuplicatesCursorName + '.cod_sub AND ';
			+  sTmpTableName + '.cod_art = ' + humanResourcesDuplicatesCursorName + '.cod_art '
		
		&markDupsRecordsClause
	ENDIF

ENDPROC

*-------------------------------------------------
PROCEDURE processUCI(bBeSilent as Boolean, nResultsFileHandler as Number )
	*Carga el archivo plano V hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_UCI'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano 
	
	LOCAL sErrorMsg as String 

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ,'UCI en UPGD' IN PlainsLib

	*Elimina registros duplicados en la tabla PLANO_UCI
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,sTmpTableName)
	sSIVIGILACurrentDir=SYS(5)+SYS(2003)
	SET DEFAULT TO (_RTA_CONSOLIDA )
	oAnomaliesSolver.sExcludedFields = " NUM_REG SUBE "
	oAnomaliesSolver.sXlsFilePath = sXLSFilesPath
	oAnomaliesSolver.deleteDuplicates()
	RELEASE oAnomaliesSolver
	SET DEFAULT TO (sSIVIGILACurrentDir)
	
	SELECT (sTmpTableName)
	R_IMP=RECCOUNT()
	total_uci_cargadas=R_IMP

	DELETE ALL FOR EMPTY(cod_pre)
	 
	* SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
	LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	UCIDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@UCIDuplicatesCursorName)

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_UCI_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_UCI_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH UCIDuplicatesCursorName, ARC_MAE, 'V', _RTA_CONSOLIDA, 'ERRORES' IN PlainsLib
		
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
			+ ' FROM ' + UCIDuplicatesCursorName + ' WHERE ';
			+  sTmpTableName + '.ID_UCI  = ' + UCIDuplicatesCursorName + '.ID_UCI'
		
		&markDupsRecordsClause
	ENDIF

ENDPROC

*-----------------------------------------------------------------------------------------------------
PROCEDURE processUPGD(bBeSilent as Boolean, nResultsFileHandler as Number )
	*Carga el archivo plano U hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_UPGD'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano 
	
	LOCAL sErrorMsg as String 
		
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ,'UPGDs' IN PlainsLib
	tot_upgd=RECCOUNT()

	R_IMP=RECCOUNT()
	DELETE ALL FOR EMPTY(FEC_CAR)
	IF _TALLY>0
		sErrorMsg = 'SE HAN DESCARTADO ' + NC(_TALLY) + ' UPGD POR DATOS INCOMPLETOS'
		IF !bBeSilent THEN	
			=showErrorMessage(sErrorMsg,0)
		ELSE
			=FPUTS(nResultsFileHandler,sErrorMsg)
		ENDIF
	ENDIF

	*Suprime de la tabla PLANO_UPGD los registros que aparecen como activos y que no tengan registros relacionados ;
	en ninguno de los archivos N, D, B o I
	sSelectSQLCmd="SELECT COD_PRE + COD_SUB as codigoUPGD FROM '" +_RTA_CONSOLIDA + "\PLANO_PACIENTES'" +;
					" UNION SELECT COD_PRE + COD_SUB codigoUPGD FROM '" + _RTA_CONSOLIDA + "\PLANO_BROTES'" 
	IF FILE(_RTA_CONSOLIDA + '\PLANO_E43.DBF') THEN
		sSelectSQLCmd = sSelectSQLCmd + " UNION SELECT COD_PRE + COD_SUB codigoUPGD FROM '" + _RTA_CONSOLIDA + "\PLANO_E43'"
	ENDIF
	sSelectSQLCmd = sSelectSQLCmd + " UNION SELECT COD_PRE + COD_SUB codigoUPGD FROM '" + _RTA_CONSOLIDA + "\PLANO_EVENTOS' INTO TABLE rsActiveUPGDs"
	&sSelectSQLCmd
	DELETE FROM _RTA_CONSOLIDA + '\PLANO_UPGD' WHERE ACT_SIV=1 AND (COD_PRE + COD_SUB NOT IN (SELECT codigoUPGD FROM rsActiveUPGDs))
	IF _TALLY>0
		sErrorMsg = 'SE HAN DESCARTADO ' + NC(_TALLY) + ' UPGD POR CUANTO FIGURAN COMO ACTIVAS EN EL ARCHIVO PLANO PERO NO TIENEN REGISTROS EN NINGUNO DE LOS ARCHIVOS N, D, B o I'
		IF !bBeSilent THEN
			=showErrorMessage(sErrorMsg,0)
		ELSE
			=FPUTS(nResultsFileHandler,sErrorMsg)
		ENDIF
	ENDIF
	
	*Activa todas las UPGDs del archivo PLANO_UPGD que están reportando datos en uno de los ;
	archivos N, D, B o I
	UPDATE _RTA_CONSOLIDA + '\PLANO_UPGD' SET ACT_SIV = 1 WHERE COD_PRE + COD_SUB IN (SELECT codigoUPGD FROM rsActiveUPGDs)

	*Corrije los posibles registros duplicados que se encuentren en la tabla PLANO_UPGD con el fin de evitar ;
	la carga de duplicados
	SET PROCEDURE TO AnomaliesHandler ADDITIVE
	=solveDuplicates(sTmpTableName)
	sSourceTableName=_RTA_CONSOLIDA+'\'+sTmpTableName
	USE (sSourceTableName)
	
	* SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
		LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	UPGDsDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@UPGDsDuplicatesCursorName)

	IF XtractResult = 0		&& NO SE EXTRAJERON REGISTROS
		RETURN
	ENDIF

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_UPGD_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_UPGD_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH UPGDsDuplicatesCursorName, ARC_MAE, 'U', _RTA_CONSOLIDA,;
			 'ERRORES' IN PlainsLib

		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
				+ ' FROM ' + UPGDsDuplicatesCursorName+ ' WHERE ';
				+  sTmpTableName + '.cod_pre  = ' + UPGDsDuplicatesCursorName+ '.cod_pre AND ';
				+  sTmpTableName + '.cod_sub = ' + UPGDsDuplicatesCursorName+ '.cod_sub '
		
		&markDupsRecordsClause
	ENDIF
 
ENDPROC


*-----------------------------------------------------------------------------------------------------
PROCEDURE processIndividual(bBeSilent as Boolean, nResultsFileHandler as Number )

	*Carga el archivo plano N hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_PACIENTES'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano N
	
	LOCAL sErrorMsg as String 

	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ADDBS(sXLSFilesPath)+"DatosBasicosIndividuales", ;
							'Not. Individual' IN PlainsLib
*SET STEP ON 
	**JLGB referencia requerimiento REQ204
	*DO TransferMigrateAllFields IN TransferDatafields.prg
	DO TransformOnPlainsLoad IN TransferDataHandler WITH 2, (_RTA_CONSOLIDA)

	*Corrije los posibles registros duplicados que se encuentren en la tabla PLANO_PACIENTES con el fin de evitar ;
	la carga de duplicados
	IF n_version = _VERSION2010 THEN
		SET PROCEDURE TO AnomaliesHandler ADDITIVE
		sSourceTableName=_RTA_CONSOLIDA+'\'+sTmpTableName
		=solveDuplicates(sSourceTableName,sXLSFilesPath)
		USE (sSourceTableName)
	ENDIF
	tot_pte=RECCOUNT()


	*Dado que algunos campos son exportados luego de ser sometidos a transformaciones,;
	ejecuta las	transformaciones inversas necesarias para actualizar los datos recien cargados;
	de tal forma que se restablezcan sus valores originales
	IF n_version = _VERSION2008
		REPLACE COD_MUN WITH COD_DEP+COD_MUN, MUN_PRO WITH DEP_PRO+MUN_PRO ALL
	ENDIF

	IF n_version = _VERSION2010
		REPLACE COD_MUN WITH cod_dpto_o + cod_mun_o, ;
				MUN_PRO WITH cod_dpto_r + cod_mun_r ALL

		UPDATE &sTmpTableName SET PAIS=PAISES.pais FROM PAISES WHERE &sTmpTableName..cod_pais_o=PAISES.CodNumPais
	ENDIF
*SET STEP ON 
	*Selecciona de la tabla de pacientes solamente aquellos que tengan datos complementarios.
	orphansPatientsCursorName=''
	noOrphansPatientsCursorName=''
	SET PROCEDURE TO PlainsLib ADDITIVE
	IF !USED('PLANO_EVENTOS') THEN
		USE _RTA_CONSOLIDA + '\PLANO_EVENTOS' IN 0
	ENDIF
	nOrphansPatients=extractOrphans(sTmpTableName,'PLANO_EVENTOS',@orphansPatientsCursorName,@noOrphansPatientsCursorName,n_version, 1)
	
	IF nOrphansPatients > 9 THEN
		IF !bBeSilent THEN		
			MESSAGEBOX(WARNING_INDIVIDUAL_PLAIN_FILE_WITHOUT_CD, 0 + 16 + 256, 'Error de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_INDIVIDUAL_PLAIN_FILE_WITHOUT_CD)
		ENDIF
		
		orphansExcelFileName=sXLSFilesPath + '\' + sTmpTableName + '_SinComplementarios_o_Inexistentes'
		COPY TO [&orphansExcelFileName] XL5

		* Agrega los registros sin datos complementarios a la tabla de registro de ERRORES;
		(addDupsToErrorTable sirve para este propósito aún cuando su nombre no lo refleje)
		DO addDupsToErrorTable WITH orphansPatientsCursorName, ARC_MAE, 'N', _RTA_CONSOLIDA,;
			 'ERRORES', 'UPGD DE DONDE PROVIENEN LOS REGISTROS SIN DATOS COMPLEMENTARIOS: ' IN PlainsLib

		markOrpahnsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
				+ ' FROM ' + orphansPatientsCursorName + ' WHERE ';
				+  sTmpTableName + '.ajuste  = ' + orphansPatientsCursorName+ '.ajuste AND ';
				+  sTmpTableName + '.año     = ' + orphansPatientsCursorName+ '.año AND ';
				+  sTmpTableName + '.semana  = ' + orphansPatientsCursorName+ '.semana AND ';
				+  sTmpTableName + '.cod_eve = ' + orphansPatientsCursorName+ '.cod_eve AND ';
				+  sTmpTableName + '.num_ide = ' + orphansPatientsCursorName+ '.num_ide AND ';
				+  sTmpTableName + '.tip_ide = ' + orphansPatientsCursorName+ '.tip_ide AND ';
				+  sTmpTableName + '.cod_pre = ' + orphansPatientsCursorName+ '.cod_pre AND ';
				+  sTmpTableName + '.cod_sub = ' + orphansPatientsCursorName+ '.cod_sub ';
		
		&markOrpahnsRecordsClause
	ENDIF


	* SE CREAN UN CURSOR Y UNA TABLA, UNO CON LOS REGISTROS DUPLICADOS Y EL OTRO CON	LOS REGISTROS NO DUPLICADOS PARA SUBIR ... ;
		C_DUPS, T_NONDUPS_[Abreviatura de la tabla de la que proviene] 
	patientDuplicatesCursorName=''
	*XtractResult = extractDupsAndNotDups(sTmpTableName,@patientDuplicatesCursorName)
	XtractResult = extractDupsAndNotDups(noOrphansPatientsCursorName,@patientDuplicatesCursorName)
	
	IF XtractResult = 0		&& NO SE EXTRAJERON REGISTROS
		RETURN
	ENDIF

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_INDIVIDUAL_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_INDIVIDUAL_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
	
		DO addDupsToErrorTable WITH patientDuplicatesCursorName, ARC_MAE, 'N', _RTA_CONSOLIDA,;
			 'ERRORES' IN PlainsLib

		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
		=UpdateFieldsTable("PLANO_PACIENTES", "PLANO_PACIENTES.SUBE=1", patientDuplicatesCursorName, ;
							"AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB")
	ENDIF

ENDPROC
*-----------------------------------------------------------------------------------------------------
PROCEDURE updateSubeIn 
	PARAMETERS _PlainTable, _Nreg

	checkAndSelect(_PlainTable,.F.)
	GO _Nreg
	REPLACE SUBE WITH 1
ENDPROC

	
*-----------------------------------------------------------------------------------------------------
FUNCTION extractDupsAndNotDups

	LPARAMETERS PlainFileName, duplicatesRSName, aPlainTableKeys, sKeyIDToUse

	*Determina si existen registros duplicados en PlainFileName teniendo en cuenta la clave que para tales efectos;
	se encuentre definida en el arreglo aPlainTableKeys o de acuerdo con la indicada en aPlainTableKeys por sKeyIDToUse, ;
	si es que se recibe este parámetro opcional.
	
	*Retorna:	(1) Todos los registros son únicos	;
				(10) Todos los registros son duplicados	;
				(11) Existen registros duplicados y NO duplicados

	*Como efecto colateral, crea ;
	1. Una tabla que contiene los registros NO duplicados en  PlainFileName. ;
	El nombre de esta tabla será T_NONDUPS_ + ALLTRIM(SUBSTR(PlainFileName,RAT('_',PlainFileName)+1));
	;
	2. Un cursor que contiene el conteo de los registros duplicados en PlainFileName. ;
	El nombre de esta curosr será C_COUNT_DUPS ;
	;
	3. Una tabla que contendrá los registros duplicados en PlainFileName. ;
	El nombre de esta tabla será C_DUPS_ + ALLTRIM(SUBSTR(PlainFileName,RAT('_',PlainFileName)+1)) y ;
	quedará almacenado en duplicatesRSName;
	;
	El parámetro opcional sKeyIDToUse permite que se puedan detectar registros duplicados en PlainFileName ;
	sin utilizar la clave por defecto establecida en el arreglo aPlainTableKeys; así, por ejemplo, se pueden;
	detectar duplicados en la tabla UPGD utilizando la clave de detección de duplicados establecida ;
	para PLANO_UPGD; en este caso se debería hacer la llamada;
	=extractDupsAndNotDups('UPGD',...,'PLANO_UPGD')
	

	_ReturnedValue = 0

	IF VARTYPE(sKeyIDToUse)='C' AND !EMPTY(sKeyIDToUse) THEN 
		SELECT TAG_EXPR FROM MASTER_INDEX WHERE EXTRAS='DUPLICATES' AND TABLE_NAME='&sKeyIDToUse' INTO CURSOR rsKeysForDups
	ELSE 
		SELECT TAG_EXPR FROM MASTER_INDEX WHERE EXTRAS='DUPLICATES' AND TABLE_NAME='&PlainFileName' INTO CURSOR rsKeysForDups
		IF _TALLY = 0 THEN
			SELECT TAG_EXPR FROM MASTER_INDEX WHERE EXTRAS='DUPLICATES' AND TABLE_NAME='PLANO_EXX' INTO CURSOR rsKeysForDups
		ENDIF
	ENDIF 
	IF _TALLY > 0 THEN 
	
		KeyWithCommas = ALLTRIM(STRTRAN(rsKeysForDups.TAG_EXPR,'+',','))

		SqlClause = 'SELECT ' + KeyWithCommas + ', COUNT(*) AS counter ' + ;
					'FROM ' + '"' + PlainFileName + '"' + ' GROUP BY ' + KeyWithCommas + ;
					' HAVING COUNT(*) > 1 INTO CURS C_COUNT_DUPS NOFILTER' 
		&SqlClause

		IF _TALLY > 0
			_ReturnedValue = _ReturnedValue + 10
		ENDIF


		bNRegIsPresent = isField(JUSTSTEM(PlainFileName),'nReg')
		DO crossTables WITH PlainFileName, 'C_COUNT_DUPS', KeyWithCommas, 'LEFT OUTER', 'C_COUNT_DUPS.' + LEFT(KeyWithCommas,AT(',',KeyWithCommas)-1) + ' IS NULL', ;
							!bNRegIsPresent, SqlClause IN (PATH_TO_COMMON_LIB + 'QueriesHandler')
		SqlClause = SqlClause + ' INTO TABLE T_NONDUPS_' + ALLTRIM(SUBSTR(PlainFileName,RAT('_',PlainFileName)+1))
		&SqlClause

		IF _TALLY > 0
			_ReturnedValue = _ReturnedValue + 1
		ENDIF

		duplicatesRSName = 'C_DUPS_' + ALLTRIM(SUBSTR(PlainFileName,RAT('_',PlainFileName)+1))
		DO crossTables WITH PlainFileName, 'C_COUNT_DUPS', KeyWithCommas, 'INNER', , !bNRegIsPresent, SqlClause IN (PATH_TO_COMMON_LIB + 'QueriesHandler')
		SqlClause = SqlClause + ' INTO TABLE ' + duplicatesRSName
		&SqlClause

		SELECT (duplicatesRSName)
	ENDIF 
	USE IN 'rsKeysForDups'
			
	RETURN _ReturnedValue 
		  
ENDFUNC

*-----------------------------------------------------------------------------------------------------
PROCEDURE initPlainTableKeysArray(aPlainTableKeys)

	DIMENSION aPlainTableKeys(22,2)
	
	aPlainTableKeys(1,1) = 'PLANO_PACIENTES'
	aPlainTableKeys(1,2) = ' AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB '

	aPlainTableKeys(2,1) = 'PLANO_EXX'
	aPlainTableKeys(2,2) = ' AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB '

	aPlainTableKeys(3,1) = 'PLANO_E23'
	aPlainTableKeys(3,2) = ' COD_EVE, AÑO, SEMANA, COD_MUN, NUM_CON, COD_PRE, COD_SUB '

	aPlainTableKeys(4,1) = 'PLANO_E31'
	aPlainTableKeys(4,2) = ' AÑO, SEMANA, COD_PRE, COD_SUB, COD_EVE, COD_DEP, COD_MUN, ESPECIE_VE '

	aPlainTableKeys(5,1) = 'PLANO_E32'
	aPlainTableKeys(5,2) = ' MES, AÑO, COD_PRE, COD_SUB, COD_EVE, COD_DEP, COD_MUN '

	aPlainTableKeys(6,1) = 'PLANO_LAB'
	aPlainTableKeys(6,2) = 'AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, AGENTE, PRUEBA, COD_PRE, COD_SUB,' + ;
							'MUESTRA, RESULTADO, VALOR '

	aPlainTableKeys(7,1) = 'PLANO_BROTES'
	aPlainTableKeys(7,2) = 'COD_EVE, AÑO, SEMANA, COD_MUN, NUM_CON, COD_PRE, COD_SUB '

	aPlainTableKeys(8,1) = 'PLANO_UPGD'
	aPlainTableKeys(8,2) = 'COD_PRE, COD_SUB'

	aPlainTableKeys(9,1) = 'PLANO_RHS'
	aPlainTableKeys(9,2) = 'COD_PRE, COD_SUB, COD_ART, VALOR '

	aPlainTableKeys(10,1) = 'PLANO_EBA'
	aPlainTableKeys(10,2) = ' AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_DX '

	aPlainTableKeys(11,1) = 'PLANO_E43'
	aPlainTableKeys(11,2) = ' COD_EVE, AÑO, SEMANA, COD_MUN, NUM_CON, COD_PRE, COD_SUB '

	aPlainTableKeys(12,1) = 'T_NonOrphans_PACIENTES'
	aPlainTableKeys(12,2) = ' AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB '

	aPlainTableKeys(13,1) = 'T_NonOrphans_BROTES'
	aPlainTableKeys(13,2) = ' AÑO, SEMANA, COD_EVE, COD_MUN, NUM_CON, COD_PRE, COD_SUB '

	aPlainTableKeys(14,1) = 'PLANO_PACIENTEWEB'
	aPlainTableKeys(14,2) = ' AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, CodUPGD '

	aPlainTableKeys(15,1) = 'PLANO_E80'
	aPlainTableKeys(15,2) = ' MES, AÑO, COD_PRE, COD_SUB, COD_EVE, NUM_CON '

	aPlainTableKeys(16,1) = 'PLANO_E81'
	aPlainTableKeys(16,2) = ' MES, AÑO, COD_PRE, COD_SUB, COD_EVE, NUM_CON '

	aPlainTableKeys(17,1) = 'PLANO_UCI'
	aPlainTableKeys(17,2) = ' COD_PRE , COD_SUB , ID_UCI '

	aPlainTableKeys(18,1) = 'PLANO_E88'
	aPlainTableKeys(18,2) = ' MES, AÑO, COD_PRE, COD_SUB, COD_EVE, NUM_CON '

	aPlainTableKeys(19,1) = 'PLANO_E90'
	aPlainTableKeys(19,2) = ' MES, AÑO, COD_PRE, COD_SUB, COD_EVE, NUM_CON '

	aPlainTableKeys(20,1) = 'PLANO_E91'
	aPlainTableKeys(20,2) = ' COD_EVE, AÑO, SEMANA, COD_MUN, NUM_CON, COD_PRE, COD_SUB '

	aPlainTableKeys(21,1) = 'PLANO_CON_ID'
	aPlainTableKeys(21,2) = ' ID '
	
	aPlainTableKeys(22,1) = 'PLANO_E94'
	aPlainTableKeys(22,2) = ' COD_EVE, AÑO, SEMANA, COD_MUN, NUM_CON, COD_PRE, COD_SUB '
ENDPROC


*-----------------------------------------------------------------------------------------------------
FUNCTION loadControlFile

	LPARAMETERS bBeSilent as Boolean, nResultsFileHandler as Number 

	SET PROCEDURE TO plainsLib ADDITIVE

	_ReturnedValue = .T.
	TIT('Importando Datos desde Archivos de Texto...')
	ERRORES_UPGD=0

	_ReturnedValue = createPlainTable(_RTA_CONSOLIDA + '\', 'ERRORES')
	_ReturnedValue = createPlainTable(_RTA_CONSOLIDA + '\', 'EXISTENTES')
	_ReturnedValue = createPlainTable(_RTA_CONSOLIDA + '\', 'PLANO_CONTROL')

	USE [&_RTA_CONSOLIDA\PLANO_CONTROL] EXCLUSIVE
	ZAP

	APPEND FROM (UBICA) DELIMITED
	REPLACE archivo WITH ALLTRIM(archivo)+'.TXT' FOR !EMPTY(ARCHIVO) AND RIGHT(ALLTRIM(ARCHIVO),3)!='TXT'

	IF RECCOUNT()=0
		IF !bBeSilent THEN	
			T_I(ERR_CONTROL_FILE_WITHOUT_DATA)
		ELSE
			=FPUTS(nResultsFileHandler,ERR_CONTROL_FILE_WITHOUT_DATA)
		ENDIF
		RETURN .F.
	ENDIF

	DELETE ALL FOR LEFT(ARCHIVO,1)='T'
	GO TOP
	IF SUBSTR(ARCHIVO,2,7)!=SUBSTR(ARC_MAE,2,7)
		IF !bBeSilent THEN	
			T_I(ERR_CONTROL_FILE_WITHOUT_DATA)
		ELSE
			=FPUTS(nResultsFileHandler,ERR_CONTROL_FILE_WITHOUT_DATA)
		ENDIF
		RETURN .F.
	ENDIF

	RETURN _ReturnedValue 

ENDFUNC



*-----------------------------------------------------------------------------------------------------
FUNCTION validateControlFile(sControlFileNameAndPath as string, bBeSilent as Boolean, nResultsFileHandler as Number ) as Boolean 

	*Retorna .T. si el archivo de control ubicado en sControlFileNameAndPath correspondiente a un paquete de archivos planos de notificación;
	 satisface las reglas de validación siguientes: ;
	1. sControlFileNameAndPath  existe ;
	2. La primera letra del archivo de control es 'c' o 'C' ;
	3. La extensión del del archivo de control es TXT ;
	4. La ruta en donde se deben consolidar los archivos planos del paquete cuyo archivo de control es sControlFileNameAndPath, existe 
	
	LOCAL sControlFileNameAndExt as String
	
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE

	IF !FILE(sControlFileNameAndPath )
		IF !bBeSilent THEN
			=showErrorMessage(ERR_CONTROL_FILE_NOT_FOUND,1)
		ELSE
			=FPUTS(nResultsFileHandler,ERR_CONTROL_FILE_NOT_FOUND)
		ENDIF
		RETURN .F.
	ENDIF

	sControlFileNameAndExt = JUSTFNAME(sControlFileNameAndPath)
	
	IF UPPER(SUBSTR(sControlFileNameAndExt,1,1)) != 'C'
		IF !bBeSilent THEN
			=showProgressMessage(ERR_INVALID_CONTROL_FILE_NAME)
		ELSE
			=FPUTS(nResultsFileHandler,ERR_INVALID_CONTROL_FILE_NAME)
		ENDIF
		RETURN .F.
	ENDIF

	IF UPPER(RIGHT(sControlFileNameAndExt,3)) != 'TXT'
		IF !bBeSilent THEN
			=showProgressMessage(ERR_INVALID_CONTROL_FILE_EXT)
		ELSE
			=FPUTS(nResultsFileHandler,ERR_INVALID_CONTROL_FILE_EXT)
		ENDIF
		RETURN .F.
	ENDIF

	IF SUBSTR(RIGHT(sControlFileNameAndExt,12),7,2) = 'RA'
		IF !bBeSilent THEN	
			=showErrorMessage('AHORA SE VAN A IMPORTAR DATOS DE UNA RETROALIMENTACION', 0)
			DAT_RA=1
		ELSE
			*Comentado por:	Wilson Aguilar	Fecha:2019/04/25;
			Por decisión adopata en reunión de seguimiento de Mesa de Ayuda, se decidió modificar esta parte de código
			*=FPUTS(nResultsFileHandler,ERR_FEEDBACK_NOT_AVAILABLE)
			*RETURN .F.
			DAT_RA=1
		ENDIF
	ENDIF

	_RTA_CONSOLIDA=_mydocument+'\ARCHIVOS DE CONSOLIDACION\VERSION 20'+IIF(n_version = _Version2008,'08','10')

	IF !DIRECTORY(_RTA_CONSOLIDA)
		IF !bBeSilent THEN	
			=showErrorMessage(ERR_CONSOLIDATION_FOLDER_NOT_FOUND, 1)
		ELSE
			=FPUTS(nResultsFileHandler,ERR_CONSOLIDATION_FOLDER_NOT_FOUND)
		ENDIF
		RETURN .F.
	ELSE
		*Desocupa la carpeta en donde se van a mantener las tablas intermedias de procesamiento
		DELETE FILE [&_RTA_CONSOLIDA\*.*]
	ENDIF

	RETURN .T.

ENDFUNC

*-----------------------------------------------------------------------------------------------------
PROCEDURE processIRA(bBeSilent as Boolean, nResultsFileHandler as Number )
	* Carga el archivo plano I hacia la tabla temporal intermedia sTmpTableName 

	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_E43'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano I

	LOCAL sErrorMsg as String 

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ADDBS(sXLSFilesPath)+"DatosIrag", ;
							'Morb.por IRAG' IN PlainsLib
	tot_dat = tot_dat + _TALLY	

    * SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
		LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	iraDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@iraDuplicatesCursorName)

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN		
			MESSAGEBOX( WARNING_IRA_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_IRA_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH iraDuplicatesCursorName, ARC_MAE, 'D', _RTA_CONSOLIDA,;
			 'ERRORES' IN PlainsLib

		* Se marcan en la tala origen los registros que se subieron a errores en el paso anterior
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
						+ ' FROM ' + IRADuplicatesCursorName + ' WHERE ';
						+  sTmpTableName  + '.año     = ' + iraDuplicatesCursorName + '.año AND ';
						+  sTmpTableName  + '.semana  = ' + iraDuplicatesCursorName + '.semana AND ';
						+  sTmpTableName  + '.cod_eve = ' + iraDuplicatesCursorName + '.cod_eve AND ';
						+  sTmpTableName  + '.cod_mun = ' + iraDuplicatesCursorName + '.cod_mun AND ';
						+  sTmpTableName  + '.num_con = ' + iraDuplicatesCursorName + '.num_con '
				
		&markDupsRecordsClause
	ENDIF
ENDPROC

*-----------------------------------------------------------------------------------------------------
PROCEDURE CargaEventos(SivigilaVersion, bBeSilent as Boolean, nResultsFileHandler as Number )

	LOCAL sErrorMsg as String 

	* VERSION: SIVIGILA 2009 - PROGRAMA MODIFICADO
	* ORIGINAL: IMPORTAR.CARGAEVENTOS
	* PROCEDIMIENTO PARA CARGAR EVENTOS INDIVIDUALES EN LA TABLA DE 	;
		EVENTOS CORRESPONDOIENTE, DESDE EL ARCHIVO PLANO 'DATOS COMPLEMENTARIOS...'	;
		_RTA_CONSOLIDA -> RUTA DONDE SE ENCUENTRAN LOS ARCHICVOS DE CONSOLIDACION	;
		NOM_ARC -> CONTIENE EL NOMBRE DEL ARCHIVO DE DATOS

	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+'PLANO_EVENTOS', , ,'Datos complementarios' IN PlainsLib

	*De acuerdo con la version del Sistema SIVIGILA, determina qué tabla de definición de eventos debe;
	utilizarse para	cargar la información
	IF SivigilaVersion=_VERSION2008 THEN
		eventsDefinitonTableName='EVENTOS2008'
	ELSE
		eventsDefinitonTableName='EVENTOS'
	ENDIF

	SELECT DISTINCT COD_EVE FROM PLANO_EVENTOS WHERE CONTROL NOT LIKE 'CONTACTOS%' AND CONTROL NOT LIKE 'SEGUIMIENTO%' INTO CURSOR C_EVENTS
	SELECT C_EVENTS
	SCAN
		_EVENTO = C_EVENTS.COD_EVE

		*Establece, según el código de evento, en qué tablas maestras (indicadas por el número de formulario);
		debe almacenarse la información
		SELECT FORMULARIO, NOM_EVE FROM &eventsDefinitonTableName WHERE ;
			ALLTRIM(&eventsDefinitonTableName..COD_EVE) == ALLTRIM(_EVENTO) ;
			AND !EMPTY(&eventsDefinitonTableName..FORMULARIO) INTO ARRAY A_EVENTO

		IF _TALLY > 0
			sPlainTableName = 'PLANO_E'+PADL(ALLTRIM(A_EVENTO),2,'0')	
			WAIT WINDOW NOWAIT "EVENTO ... " + A_Evento(2)

			IF !FILE(_RTA_CONSOLIDA + '\' + sPlainTableName + '.DBF')
				sErrorMsg = 'No existe ' + sPlainTableName + ' en ' + _RTA_CONSOLIDA 
				IF !bBeSilent THEN 
					MESSAGEBOX(sErrorMsg)
				ELSE
					=FPUTS(nResultsFileHandler,sErrorMsg)
				ENDIF
				LOOP
			ENDIF
			
			IF !USED(sPlainTableName )
				SELECT 0
				USE [&_RTA_CONSOLIDA\&sPlainTableName] EXCLUSIVE
			ENDIF
			
			_condition = ' FOR COD_EVE = _EVENTO '

			* SE BUSCA LA CONDICION EN EL ARRAY PARA QUE FILTRE LABORATORIOS
			_condition = _condition + ' AND ' + initEventConditions(sPlainTableName, _RTA_CONSOLIDA)
			
			SELECT &sPlainTableName
			
			*Comentado por Wilson Aguilar 11/12/2009;
			La siguiente instrucción hace que no se carguen los registros de todos los eventos que;
			tienen como destino una misma tabla, por tanto, se suprimió
			&&ZAP
			APPEND FROM (NOM_ARC) DELIMITED &_condition
			
			*Dado que para algunos eventos algunos campos son exportados luego de ser sometidos a transformaciones,;
			ejecuta las	transformaciones inversas necesarias para actualizar los datos recien cargados;
			de tal forma que se restablezcan sus valores originales
			IF SivigilaVersion= _VERSION2010 THEN
				IF sPlainTableName = 'PLANO_E29' THEN
					REPLACE COD_MUN WITH cod_dpto_n + cod_mun_na ALL
				ENDIF
			ENDIF
			
			IF SivigilaVersion= _VERSION2008 THEN
				IF sPlainTableName = 'PLANO_E29' THEN
					REPLACE COD_MUN WITH DEP + MUN ALL
				ENDIF
			ENDIF
			tot_dat = tot_dat + _TALLY
			
			 * SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
				LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
			eventDuplicatesCursorName=''
			XtractResult = extractDupsAndNotDups(sPlainTableName,@eventDuplicatesCursorName)

			* Si XtractResult = 10 u 11, hay duplicados para registrar
			IF XtractResult > 9 
				IF !bBeSilent THEN
					MESSAGEBOX( WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART1 + _EVENTO + ;
								WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART2 , 0 + 16 + 256, 'Aviso de importación SIVIGILA')
				ELSE
					=FPUTS(nResultsFileHandler, WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART1 + _EVENTO + ;
							WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART2)	
				ENDIF
				
				* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
				DO addDupsToErrorTable WITH eventDuplicatesCursorName, ARC_MAE, 'D',;
					 _RTA_CONSOLIDA, 'ERRORES' IN PlainsLib

				EventSuffix =PADL(ALLTRIM(A_EVENTO),2,'0')
				DO CASE	
					CASE EventSuffix = '23' OR EventSuffix = '43'  OR EventSuffix = '91' OR EventSuffix = '94'
						*Enfermedades transmitidas por alimentos (Colectiva), Hepatitis A, Fiebre Tifoide;
						Morbilidad por IRA (colectiva)
						markDupsRecordsClause='UPDATE ' + sPlainTableName + ' SET ' + sPlainTableName + '.sube = 1 ' ;
								+ ' FROM ' + eventDuplicatesCursorName + ' WHERE ';
								+  sPlainTableName  + '.cod_eve= ' + eventDuplicatesCursorName + '.cod_eve AND ';
								+  sPlainTableName  + '.año= ' + eventDuplicatesCursorName + '.año AND ';
								+  sPlainTableName  + '.semana = ' + eventDuplicatesCursorName + '.semana AND ';
								+  sPlainTableName  + '.cod_mun= ' + eventDuplicatesCursorName + '.cod_mun AND ';						
								+  sPlainTableName  + '.num_con= ' + eventDuplicatesCursorName + '.num_con AND ';
								+  sPlainTableName  + '.cod_pre= ' + eventDuplicatesCursorName + '.cod_pre AND ';
								+  sPlainTableName  + '.cod_sub= ' + eventDuplicatesCursorName + '.cod_sub '
							
					CASE EventSuffix = '31'
						&& Vigilancia entomológica de la malaria
						markDupsRecordsClause='UPDATE ' + sPlainTableName + ' SET ' + sPlainTableName + '.sube = 1 ' ;
								+ ' FROM ' + eventDuplicatesCursorName + ' WHERE ';
								+  sPlainTableName  + '.año= ' + eventDuplicatesCursorName + '.año AND ';
								+  sPlainTableName  + '.semana = ' + eventDuplicatesCursorName + '.semana AND ';
								+  sPlainTableName  + '.cod_pre= ' + eventDuplicatesCursorName + '.cod_pre AND ';
								+  sPlainTableName  + '.cod_sub= ' + eventDuplicatesCursorName + '.cod_sub AND ';
								+  sPlainTableName  + '.cod_eve= ' + eventDuplicatesCursorName + '.cod_eve AND ';
								+  sPlainTableName  + '.cod_dep= ' + eventDuplicatesCursorName + '.cod_dep AND ';
								+  sPlainTableName  + '.cod_mun= ' + eventDuplicatesCursorName + '.cod_mun AND ';						
								+  sPlainTableName  + '.especie_ve= ' + eventDuplicatesCursorName + '.especie_ve AND ';
								+  sPlainTableName  + '.ajuste= ' + eventDuplicatesCursorName + '.ajuste '											
					CASE EventSuffix = '32'
						&& Informe mensual de actividades para la prevención y control de la rabia

						markDupsRecordsClause='UPDATE ' + sPlainTableName + ' SET ' + sPlainTableName + '.sube = 1 ' ;
								+ ' FROM ' + eventDuplicatesCursorName + ' WHERE ';
								+  sPlainTableName  + '.mes = ' + eventDuplicatesCursorName + '.mes AND ';
								+  sPlainTableName  + '.año= ' + eventDuplicatesCursorName + '.año AND ';
								+  sPlainTableName  + '.cod_pre= ' + eventDuplicatesCursorName + '.cod_pre AND ';
								+  sPlainTableName  + '.cod_sub= ' + eventDuplicatesCursorName + '.cod_sub AND ';
								+  sPlainTableName  + '.cod_eve= ' + eventDuplicatesCursorName + '.cod_eve AND ';
								+  sPlainTableName  + '.cod_dep= ' + eventDuplicatesCursorName + '.cod_dep AND ';
								+  sPlainTableName  + '.cod_mun= ' + eventDuplicatesCursorName + '.cod_mun '						
					CASE EventSuffix = '80' OR EventSuffix = '81' OR EventSuffix = '88'
						&& IAD - Infecciones asociadas a dispositivos
						&& CAB - Infecciones asociadas consumo de antibioticos

						markDupsRecordsClause='UPDATE ' + sPlainTableName + ' SET ' + sPlainTableName + '.sube = 1 ' ;
								+ ' FROM ' + eventDuplicatesCursorName + ' WHERE ';
								+  sPlainTableName  + '.mes = ' + eventDuplicatesCursorName + '.mes AND ';
								+  sPlainTableName  + '.año= ' + eventDuplicatesCursorName + '.año AND ';
								+  sPlainTableName  + '.cod_pre= ' + eventDuplicatesCursorName + '.cod_pre AND ';
								+  sPlainTableName  + '.cod_sub= ' + eventDuplicatesCursorName + '.cod_sub AND ';
								+  sPlainTableName  + '.cod_eve= ' + eventDuplicatesCursorName + '.cod_eve AND ';
								+  sPlainTableName  + '.num_con= ' + eventDuplicatesCursorName + '.num_con '						
					OTHERWISE
						markDupsRecordsClause='UPDATE ' + sPlainTableName + ' SET ' + sPlainTableName + '.sube = 1 ' ;
								+ ' FROM ' + eventDuplicatesCursorName + ' WHERE ';
								+  sPlainTableName  + '.año = ' + eventDuplicatesCursorName + '.año AND ';
								+  sPlainTableName  + '.semana = ' + eventDuplicatesCursorName + '.semana AND ';
								+  sPlainTableName  + '.cod_eve = ' + eventDuplicatesCursorName + '.cod_eve AND ';
								+  sPlainTableName  + '.tip_ide = ' + eventDuplicatesCursorName + '.tip_ide AND ';
								+  sPlainTableName  + '.num_ide = ' + eventDuplicatesCursorName + '.num_ide '
						
				ENDCASE
				&markDupsRecordsClause
			ENDIF
		ENDIF
		SELECT C_EVENTS
	ENDSCAN

	* Una vez finalizado el proceso de encontrar los duplicados y el correspondiente marcado, se	;
	continúa con los demás registros que vienen en este archivo: 
	
	*Se cargan los resultados de laboratorio contenidos en el archivo plano D
	sTmpTableName = 'PLANO_LAB'

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, 'LIKE("LABORATORIO*",CONTROL)', ,'Laboratorios' IN PlainsLib
	REPLACE NUM_REG WITH RECNO() ALL
	
	*Elimina registros duplicados en la tabla PLANO_LAB
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,sTmpTableName)
	sSIVIGILACurrentDir=SYS(5)+SYS(2003)
	SET DEFAULT TO (_RTA_CONSOLIDA )
	oAnomaliesSolver.sExcludedFields = " NUM_REG SUBE "
	oAnomaliesSolver.sXlsFilePath = sXLSFilesPath
	oAnomaliesSolver.deleteDuplicates()
	RELEASE oAnomaliesSolver
	SET DEFAULT TO (sSIVIGILACurrentDir)
	
	SELECT (sTmpTableName)
	tot_lab=RECCOUNT()

	* SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
		LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS_* 
	labsDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@labsDuplicatesCursorName)

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH labsDuplicatesCursorName, ARC_MAE, 'D', _RTA_CONSOLIDA,;
			 'ERRORES' IN PlainsLib
		
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
			+ ' FROM ' + labsDuplicatesCursorName + ' WHERE ';
			+  sTmpTableName  + '.año = ' + labsDuplicatesCursorName + '.año AND ';
			+  sTmpTableName  + '.semana = ' + labsDuplicatesCursorName + '.semana AND ';
			+  sTmpTableName  + '.cod_eve = ' + labsDuplicatesCursorName + '.cod_eve AND ';
			+  sTmpTableName  + '.tip_ide = ' + labsDuplicatesCursorName + '.tip_ide AND ';
			+  sTmpTableName  + '.num_ide = ' + labsDuplicatesCursorName + '.num_ide AND ';
			+  sTmpTableName  + '.AGENTE  = ' + labsDuplicatesCursorName + '.AGENTE  AND ';
			+  sTmpTableName  + '.PRUEBA  = ' + labsDuplicatesCursorName + '.PRUEBA  '
					
		&markDupsRecordsClause
	ENDIF

	*Se cargan los registros de contactos contenidos en el archivo plano D
	sTmpTableName = 'PLANO_CONTACTOS'

	IF FILE(_RTA_CONSOLIDA+'\' + sTmpTableName + '.DBF') THEN
		DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, 'LIKE("CONTACTOS",CONTROL)', ,'Contactos' IN PlainsLib
		REPLACE NUM_REG WITH RECNO() ALL
		TOT_CONTACTOS = RECCOUNT(sTmpTableName)
	ELSE
		TOT_CONTACTOS = 0
	ENDIF 

	*Se cargan los registros de seguimiento a contactos contenidos en el archivo plano D
	sTmpTableName = 'PLANO_SEGUIMIENTOS'

	IF FILE(_RTA_CONSOLIDA+'\'+sTmpTableName + '.DBF') THEN
		DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, 'LIKE("SEGUIMIENTO*",CONTROL)', ,'SeguimientoContactos' IN PlainsLib
		REPLACE NUM_REG WITH RECNO() ALL
		TOT_SEGUIMIENTOS = RECCOUNT(sTmpTableName)
	ELSE
		TOT_SEGUIMIENTOS = 0
	ENDIF 
ENDPROC

*-----------------------------------------------------------------------------------------------------------------------
PROCEDURE cleanTheHouse
	DELETE FILE 'T_Non*.DBF'
	DELETE FILE 'C_DUPS_*.DBF'
	DELETE FILE 'T_ReportingUPGDs.DBF'
ENDPROC