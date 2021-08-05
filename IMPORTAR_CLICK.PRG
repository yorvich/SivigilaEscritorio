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

DO cleanTheHouse IN PlainsLib
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
	      	INSERT INTO "&_RTA_CONSOLIDA\ERRORES" (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, VARIABLE);
	        	VALUES (m.ARCHIVO, NREG, 'Control', m.COD_PRE, m.COD_SUB, 'No se encuentra el archivo '+m.ARCHIVO)
		ELSE
			DO CASE
				CASE LEFT(ARCHIVO,1)='U'
					DO processUPGD WITH bBeSilent, nResultsFileHandler IN PlainsLib

				CASE LEFT(ARCHIVO,1)='R'
					DO processTH WITH bBeSilent, nResultsFileHandler IN PlainsLib

				CASE LEFT(ARCHIVO,1)='V'
					DO processUCI WITH bBeSilent, nResultsFileHandler IN PlainsLib

				CASE LEFT(ARCHIVO,1)='N'
					DO processIndividual WITH bBeSilent, nResultsFileHandler IN PlainsLib

			  	CASE LEFT(ARCHIVO,1)='B'
				  	DO processCollective WITH bBeSilent, nResultsFileHandler IN PlainsLib

		  		CASE LEFT(ARCHIVO,1)='D'
				  	DO processComplementary WITH n_version, bBeSilent, nResultsFileHandler IN PlainsLib 
			  	
		  		CASE LEFT(ARCHIVO,1)='I'
					* Archivo contenador de datos colectivo Morbilidad por IRA (995, 43)
					* a partir de la versión 2010
					DO processIRA WITH bBeSilent, nResultsFileHandler IN PlainsLib
			  		
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
