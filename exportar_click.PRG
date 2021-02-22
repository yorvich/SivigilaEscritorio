#INCLUDE SIVIGILA.h


LPARAMETERS sExportationPath as String, bBeSilent as Boolean, nResultsFileHandler as Number, bIncludeUPGDs as Boolean

* bBeSilent: si es .F. los mensajes del procesamiento se dirigen a la interfaz de usuario (valor por defecto);
			 en caso contrario, se dirigen a un archivo plano de resultados	
			 
*sResultsFileName: nombre y ruta del archivo texto que contendrá los resultados de procesamiento en caso de que bBeSilent sea .T.

*bIncludeUPGDs: si es .T., genera el archivo plano de notificación U; en caso contrario (default), no lo genera. 

LOCAL oDataExporter as Object 
*An object to export intermediate results to XLS

LOCAL sOldProcedure as String

LOCAL sDirListing as String
sDirListing = ''

DIMENSION aPlains(1)


sOldProcedure = SET("Procedure")

sSetProc = 'SET PROCEDURE TO ' + PATH_TO_COMMON_LIB + '\TablesHandler ADDITIVE'
&sSetProc
sSetProc = 'SET PROCEDURE TO ' + PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger ADDITIVE'
&sSetProc
SET PROCEDURE TO PlainsLib ADDITIVE

oDataExporter=NEWOBJECT("DataExporter","DataExporter.Prg",.null.)

sInitTime=TIME()

DO cleanTheHouse IN IMPORTAR_CLICK

*SET STEP ON 
*Comentado por: Wilson Aguilar Fecha: 30/11/2009;
Fija el Path de la carpeta en donde se almacenarán los archivos que resultarán como producto ;
del proceso de exportación
RUT_SEM=sExportationPath + '\PLANOS_SEMANA_' + TRANSFORM(m.SEM_REP,'@L 99')
IF !EMPTY(n_inmediata) THEN
	RUT_SEM = RUT_SEM + '_INMEDIATA_' + STRTRAN(TTOC(DATETIME(),3),":","_")
ELSE
	IF !bBeSilent THEN	
		nExistentPlainDir = ADIR(aPlains, RUT_SEM + '*' + ALLTRIM(STR(YEAR(DATE()))) + '*',"D") 
		IF nExistentPlainDir >=1 THEN
			FOR iPlainDir = 1 TO MIN(6,nExistentPlainDir)
				sDirListing = sDirListing + aPlains(iPlainDir ,1) + chr(13)
			NEXT iPlainDir 
			sDirListing = sDirListing + '...'
			IF getYesOrNo('Se ha encontrado una o más notificaciones generadas para la semana objetivo:' + chr(13) + chr(13) + ;
				sDirListing + chr(13) + chr(13) + 'Desea Continuar de todos modos',"SIVIGILA") = _NO THEN
				RETURN
			ENDIF
		ENDIF
	ENDIF
	RUT_SEM = RUT_SEM + '_' + STRTRAN(TTOC(DATETIME(),3),":","_")
ENDIF
RUT_SEM = ADDBS(RUT_SEM)
IF !DIRECTORY(RUT_SEM)
   MKDIR (RUT_SEM)
ENDIF
oDataExporter.sExportationPath = RUT_SEM

*Comentado por: Wilson Aguilar Fecha: 30/11/2009;
Fija los nombres de los archivos que resultarán como producto del proceso de exportación

*Se ha de tener en cuenta las siguientes siglas para los nombres de los archivos a remitir ;
por parte de las Unidades Primarias Generadoras del Dato (UPGD) y las Unidades Notificadoras (UN);

*Sigla;
U		Archivo plano contenedor de datos de caracterización de UPGD.;
R		Archivo plano contenedor de datos sobre el talento humano disponible en las UPGD;
N		Archivo plano contenedor de datos básicos de notificación individual periódica semanal;
B		Archivo plano contenedor de datos de notificación colectiva periódica semanal obligatoria;
D		Archivo plano contenedor de datos complementarios de notificación periódica semanal obligatoria;
C		Archivo plano de control;
T		Archivo plano  contenedor de datos agrupados de los eventos de notificación individual periódica semanal obligatoria.	1;
I		Archivo plano contenedor de datos del evento colectivo morbilidad por IRA (Vigilancia Centinela ESI - IRAG);
V		Archivo plano contenedor de datos de caracterización de UCI en UPGD;
SIVIGILA	Archivo plano contenedor de datos de cuándo y con qué versión fueron generados los archivos anteriore;


*Establece el prefijo identificador de la Unidad que notifica para efectos de nombrar los archivos;
planos resultantes del procesamiento
DO CASE
	CASE _TIP_UNI = UPGD_UNIT
		*El nombre del archivo plano debe ser SIGLA + CODIGO UPGD +  SEMANA EPIDEMIOLÓGICA.txt;
		 por tanto inmediatamente después de la Sigla deben venir los cinco dígitos asignados consecutivamente;
		 al prestador de servicios de salud
		TA=SUBSTR(ALLTRIM(_UN),6,5) + PADL(m.SEM_REP,2,'0')
	CASE _TIP_UNI = UNM_UNIT
		*El nombre del archivo plano debe ser SIGLA + CODIGO MUNICIPIO +  SEMANA EPIDEMIOLÓGICA.txt;
		 por tanto inmediatamente después de la Sigla deben venir los cinco dígitos asignados por el DANE para ;
		 identificar los municipios
		TA=SUBSTR(ALLTRIM(_UN),1,5) + PADL(m.SEM_REP,2,'0')
	CASE _TIP_UNI = UND_UNIT
		*El nombre del archivo plano debe ser SIGLA + CODIGO DEPARTAMENTO +  SEMANA EPIDEMIOLÓGICA.txt;
		 por tanto inmediatamente después de la Sigla deben venir los dos dígitos asignados por el DANE para;
		 identificar los departamentos (Se debe agregar 3 ceros al código del departamento)
		TA=SUBSTR(ALLTRIM(_UN),1,2) + '000' + PADL(m.SEM_REP,2,'0')
ENDCASE
CAR=RUT_SEM+'U'+TA
TAL=RUT_SEM+'R'+TA
PTE=RUT_SEM+'N'+TA
DAT=RUT_SEM+'D'+TA
BRO=RUT_SEM+'B'+TA
TOT=RUT_SEM+'T'+TA
CON=RUT_SEM+'C'+TA
INS=RUT_SEM+'SIVIGILA.TXT'
STT=RUT_SEM+'S'+TA		&& (7) NOMBRE DEL ARCHIVO DE ESTRUCTURAS
PTW=RUT_SEM+'W'+TA		&& (?) NOMBRE DEL ARCHIVO PACIENTE WEB
IRA=RUT_SEM+'I'+TA
UCIFileName=RUT_SEM+'V'+TA 


&&sUPGDsFileNameAndPath = ''
&&sHumanResourcesFileNameAndPath = ''
&&sPatientsFileNameAndPath = ''
&&sEventsDataFileNameAndPath = ''
&&sCollectiveEventsFileNameAndPath = ''
&&sTotalsFileNameAndPath = ''
&&sControlFileNameAndPath = ''
&&sSIVIGILAFileNameAndPath = ''
&&sWEBPatientsFileNameAndPath=''
&&sIRAFileNameAndPath = ''
&&sNotifierFileNamePrefix=''
&&DO setOutputTxtFileNames WITH (_TIP_UNI), (_UN), m.SEM_REP, RUT_SEM, sNotifierFileNamePrefix,;
&&			sUPGDsFileNameAndPath, sHumanResourcesFileNameAndPath, sPatientsFileNameAndPath, ;
&&			sEventsDataFileNameAndPath, sCollectiveEventsFileNameAndPath, sTotalsFileNameAndPath ,;
&&			sControlFileNameAndPath, sSIVIGILAFileNameAndPath, sWEBPatientsFileNameAndPath ,;
&&			sIRAFileNameAndPath IN PlainsLib
&&TA=sNotifierFileNamePrefix

*Comentado por: Wilson Aguilar Fecha: 30/11/2009;
Inicializa los contadores de registros exportados por tipo de archivo
REG_P=0
REG_D=0
REG_B=0
REG_C=0
REG_T=0
REG_U=0
REG_W=0		&&(?) CONTADOR DEL ARCHIVO PACIENTE WEB
REG_I=0		&&(?) CONTADOR DE MORBILIDAD POR IRA
REG_V=0

*Comentado por: Wilson Aguilar	Fecha: 04/02/2015;
La siguiente parte de código se cambió por una que exporta la información sin tener en cuenta que las UPGDs involucradas estén activas;
en este sentido, la modificación hecha obliga a que se exporten todos los datos pendientes por notificar sin importar si las UPGDs ;
correspondientes a ellos están o no activas en el sistema. Con ésto se supera el error con código ... 
&&bCheckEnabledUPGD=.T.
&&IF VARTYPE(m.SEM_REP)="C" THEN
&&	bCheckEnabledUPGD=!(m.SEM_REP='RA')
&&ENDIF
*Por default, en la exportación de información, se tienen en cuenta solamente las UPGDs que están activas en el Sistema;
sin embargo, si la exportación de datos se refiere a una retroalimentación de información, entonces se exporta la información;
de todas las UPGDs sin tener en cuenta si están activas o no.
bCheckEnabledUPGD = .F.


*Comentado por: Wilson Aguilar	Fecha: 04/02/2015;
En concordancia con comentario de la parte de código antecedente, la siguiente parte de código hubo de cambiarse;
con el objeto de mantener la activación automática siempre y cuando no se tratará de una retroalimentación de información.
* Activa automáticamente UPGDs que figuren como inactivas pero que tengan datos por Notificar siempre y cuando;
la exportación de datos NO se refiera a una retroalimentación de información
&&IF bCheckEnabledUPGD THEN
&&	DO activateNonActiveUPGDsWithNotification IN plainsLib
&&ENDIF
bIsFeedback = .F.
IF VARTYPE(m.SEM_REP)="C" THEN
	bIsFeedback = (m.SEM_REP='RA')
ENDIF
IF !bIsFeedback THEN
	DO activateNonActiveUPGDsWithNotification IN plainsLib
ENDIF

IF !EMPTY(n_inmediata) THEN
	n_inmediata = " AND RecordSource.INMEDIATA='SI' AND RecordSource.EST_NOTIF=" + NOT_NOTIFIED
ENDIF

*Crea la tabla PLANO en donde se almacenarán en forma temporal los datos a exportar
DO CreateExportationPlainTable IN PlainsLib


* Genera el archivo N de datos básicos de notificación individual periódica semanal
=showProgressMessage('Generado Archivo Plano de Notificación Individual')

* Si es necesario, agrega un índice a la tabla PACIENTE con miras a optimizar el proceso
*DO addIndexToTable WITH 'PACIENTE','NOTIFICA','NOTIFICA' IN sivigilaindexeshandler
*SET STEP ON 
exportationIsOK = ExportFromQRY('PTE', FIL_NOT + FIL_EVE + FIL_VIG + n_inmediata, , RUT_SEM, bCheckEnabledUPGD,,.T.,,,!EMPTY(n_inmediata), PTE) 
IF !exportationIsOK THEN
	IF !bBeSilent THEN
		MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
				'Existen registros repetidos o duplicados en la fuente de notificación individual.' + CHR(13) + ;
				'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
				'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
	ENDIF
ENDIF

oDataExporter.sSourceTableName = "C_PTE"
oDataExporter.sExportedFileName = "DatosBasicosIndividuales"
oDataExporter.exportToXLS()
REG_P=_TALLY


* Genera el archivo D de datos complementarios de notificación periódica semanal obligatoria;
incluyendo la información de resultados de laboratorio
SELECT PLANO
ZAP

USE BDSivigila!vTablasDeDC IN 0
SELECT vTablasDeDC
SCAN
	eventToExport = vTablasDeDC.ID_DC_TABLA
	IF eventToExport != 43
		* El evento 43 (Morbilidad por IRA) se genera en un archivo independiente
		=showProgressMessage('Generando Archivo Plano del Grupo de Eventos ' + PADL(eventToExport ,2,'0'))
		
		exportationIsOK = ExportFromQRY('E'+PADL(eventToExport ,2,'0') , FIL_NOT + FIL_EVE + FIL_VIG + n_inmediata, , ;
										RUT_SEM, bCheckEnabledUPGD,,.T.)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
						'Existen registros repetidos o duplicados en la fuente de datos complementarios.' + CHR(13) + ;
						'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
						'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ENDIF
		ENDIF
	ENDIF
ENDSCAN

IF !bIsFeedback THEN
	*Se debe producir archivos de notificación rutinaria para contactos y seguimientos
	
	=showProgressMessage('Generando Archivo Plano de Laboratorios')

	exportationIsOK = ExportFromQRY('LAB' , FIL_NOT + FIL_EVE + FIL_VIG + n_inmediata, , RUT_SEM, bCheckEnabledUPGD,,.T.)
	IF !exportationIsOK THEN
		IF !bBeSilent THEN
			MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
					'Existen registros repetidos o duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
					'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
					'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
		ENDIF
	ENDIF


	=showProgressMessage('Generando Archivo Plano de Contactos')

	exportationIsOK = ExportFromQRY('CNTA' , FIL_NOT + FIL_EVE + FIL_VIG + n_inmediata, , RUT_SEM, bCheckEnabledUPGD,,.T.,.T.)
	IF !exportationIsOK THEN
		IF !bBeSilent THEN
			MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
					'Existen registros repetidos o duplicados en la fuente de notificación contactos.' + CHR(13) + ;
					'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
					'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
		ENDIF
	ENDIF

	=showProgressMessage('Generando Archivo Plano de Seguimiento a contactos')

	exportationIsOK = ExportFromQRY('SGCN' , FIL_NOT + FIL_EVE + FIL_VIG + n_inmediata, , RUT_SEM, bCheckEnabledUPGD,,.T.,.T.,,,DAT)
	IF !exportationIsOK THEN
		IF !bBeSilent THEN
			MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
					'Existen registros repetidos o duplicados en la fuente de notificación seguimiento a contactos.' + CHR(13) + ;
					'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
					'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
		ENDIF
	ENDIF
ELSE
	=showProgressMessage('Generando Archivo Plano de Laboratorios')
	exportationIsOK = ExportFromQRY('LAB' , FIL_NOT + FIL_EVE + FIL_VIG + n_inmediata, , RUT_SEM, bCheckEnabledUPGD,,.T.,,,,DAT)
	IF !exportationIsOK THEN
		IF !bBeSilent THEN
			MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
					'Existen registros repetidos o duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
					'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
					'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
		ENDIF
	ENDIF
ENDIF 
REG_D=_TALLY

* Genera el archivo B de datos de notificación colectiva periódica semanal obligatoria
SELECT PLANO
ZAP

=showProgressMessage('Generando Archivo Plano de Notificación Colectiva')

exportationIsOK = ExportFromQRY('BRO' , FIL_NOT + FIL_EVE + FIL_VIG + n_inmediata, , RUT_SEM, bCheckEnabledUPGD,,.T.,,,,BRO)
IF !exportationIsOK THEN
	IF !bBeSilent THEN
		MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
				'Existen registros repetidos o duplicados en la fuente de notificación colectiva.' + CHR(13) + ;
				'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
				'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
	ENDIF
ENDIF
oDataExporter.sSourceTableName = "C_BRO"
oDataExporter.sExportedFileName = "DatosColectivos"
oDataExporter.exportToXLS()
REG_B=_TALLY


* Genera el archivo U contenedor de datos de caracterización de UPGD
SELECT PLANO
ZAP
*SET STEP ON 
IF !bIsFeedback OR bIncludeUPGDs THEN
	=showProgressMessage('Generando Archivo Plano de las UPGD')
	* Comentado por: wilson Aguilar	Fecha: 23/04/2011 ;
	La siguiente parte de código fue modificada por solicitud del Grupo SIVIGILA del INS del 05/04/2011 en donde se indicó que en los procesos de;
	notificación se extrajera la información de todas las UPGDs; por tanto, en la llamada a ExportFromQRY se suprimió el filtro FIL_NOT
	*exportationIsOK = ExportFromQRY('UPGD' , FIL_NOT, @A_PlainTableKeys, RUT_SEM, .F.)
	exportationIsOK = ExportFromQRY('UPGD' , '', , RUT_SEM, .F.,,.T.,,,,CAR)

	IF !exportationIsOK THEN
		IF !bBeSilent THEN
			MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
						'Existen registros repetidos o duplicados en la fuente de UPGDs.' + CHR(13) + ;
						'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
						'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
		ENDIF 
	ENDIF
	REG_U=_TALLY
ENDIF

* Genera el archivo V contenedor de datos de UCI por UPGD
SELECT PLANO
ZAP

IF !bIsFeedback  OR bIncludeUPGDs THEN
	=showProgressMessage('Generando Archivo Plano de UCI por UPGD')
	exportationIsOK = ExportFromQRY('UCI' , FIL_NOT, , RUT_SEM, .F.,,.T.,,,,UCIFileName)

	IF !exportationIsOK THEN
		IF !bBeSilent THEN
			MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
					'Existen registros repetidos o duplicados en la fuente de UCI por UPGDs.' + CHR(13) + ;
					'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
					'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
		ENDIF
	ENDIF
	REG_V=_TALLY
ENDIF

* Si se trata de una notificación rutinaria, genera el archivo R contenedor de datos sobre el talento humano disponible en las UPGD
IF !bIsFeedback THEN
	IF EMPTY(n_inmediata) THEN
		SELECT PLANO
		ZAP

		=showProgressMessage('Generando Archivo Plano de Talento Humano / Servicios')

		exportationIsOK = ExportFromQRY('TAL' , 'recordSource.NOTIFICA=0 AND recordSource.ESTADO=0', , RUT_SEM, ;
										bCheckEnabledUPGD,,.T.,,,,TAL)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
						'Existen registros repetidos o duplicados en la fuente de Talento Humano / Servicios.' + CHR(13) + ;
						'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
						'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ENDIF
		ENDIF
		REG_T = _TALLY
	ELSE
		REG_T = 0
	ENDIF
ENDIF

* Genera el archivo W de notificación datos básicos para la Web 
* Comentado por: wilson aguilar	Fecha: 20/10/2011 ;
El Grupo SIVIGILA del INS decidió que con la veersión 2012, el Sistema ya no debe generar el archivo W. 
&&SELECT (AREA_PL)
&&ZAP

&&TIT('Generando Archivo Plano de notificación datos básicos para la Web')
&&ExportFromQRY('PTW', FIL_NOT + FIL_EVE + FIL_VIG + n_inmediata,@A_PlainTableKeys,RUT_SEM, bCheckEnabledUPGD, ";", .T.)		&&(?)
&&SELECT (AREA_PL)
&&sCopyCmd="COPY TO " + '"' + PTW + '"' + " DELIMITED WITH "
&&sCopyCmd=sCopyCmd+ '"' + '"' + ' WITH CHARACTER' + '"' + '"'
&&&sCopyCmd
&&REG_W=_TALLY


* Genera el archivo I contenedor de datos del evento colectivo morbilidad por IRA (Vigilancia Centinela ESI - IRAG)
SELECT PLANO
ZAP

=showProgressMessage('Generando Archivo Plano Morbilidad por IRA')
* Si es necesario, agrega un índice a la tabla EVENTOS_43 con miras a optimizar el proceso ;
DO addIndexToTable WITH 'EVENTOS_43','NOTIFICA','NOTIFICA' IN sivigilaindexeshandler

exportationIsOK  = ExportFromQRY('E43' , FIL_NOT + FIL_EVE + FIL_VIG + n_inmediata, , RUT_SEM, bCheckEnabledUPGD,,.T.,,,,IRA)
IF !exportationIsOK THEN
	IF !bBeSilent THEN
		MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
				'Existen registros repetidos o duplicados en la fuente de Morbilidad por IRA.' + CHR(13) + ;
				'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
				'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + RUT_SEM , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
	ENDIF
ENDIF
oDataExporter.sSourceTableName = "C_E43"
oDataExporter.sExportedFileName = "DatosIrag"
oDataExporter.exportToXLS()
REG_I=_TALLY
SELECT PLANO
ZAP
USE

* Genera el archivo T contenedor de datos agrupados de los eventos de notificación individual y de brotes periódica semanal obligatoria
=showProgressMessage('Generando Archivo T...')
SELECT COD_EVE, DATE() AS FEC_NOT, SEMANA, AÑO , COD_PRE, COD_SUB, SUM(VAL(GRU_1)) AS GRU_1, SUM(VAL(GRU_2)) AS GRU_2,;
	SUM(VAL(GRUPO_3)) AS GRU_3,	SUM(VAL(GRUPO_4)) AS GRU_4,	SUM(VAL(GRUPO_5)) AS GRU_5,	SUM(VAL(GRUPO_6)) AS GRU_6, ;
	SUM(VAL(GRUPO_7)) AS GRU_7,	SUM(VAL(GRUPO_8)) AS GRU_8,	SUM(VAL(GRUPO_9)) AS GRU_9,	SUM(VAL(GRUPO_10)) AS GRU_10, ;
	SUM(VAL(GRUPO_11)) AS GRU_11, SUM(VAL(GRUPO_12)) AS GRU_12,	SUM(VAL(GRUPO_13)) AS GRU_13, SUM(VAL(GRUPO_14)) AS GRU_14, ;
	SUM(VAL(GRUPO_15)) AS GRU_15, SUM(VAL(GRUPO_16)) AS GRU_16,	SUM(VAL(GRUPO_17)) AS GRU_17, SUM(VAL(GRUPO_18)) AS GRU_18, ;
	SUM(VAL(CAS_PRO)) AS CAS_PRO,  SUM(VAL(CAS_CONL)) AS CAS_CONL, SUM(VAL(CAS_CONC)) AS CAS_CONC,;
	SUM(VAL(CAS_CONN)) AS CAS_CONN, SUM(VAL(HOMBRES)) AS HOMBRES, SUM(VAL(MUJERES)) AS MUJERES,;
	SUM(VAL(VIVOS)) AS VIVOS, SUM(VAL(MUERTOS)) AS MUERTOS, MUN_PRO;
	FROM BROTES AS recordSource;
	WHERE &FIL_NOT  &FIL_EVE &FIL_VIG &n_inmediata;
	GROUP BY AÑO, COD_PRE, COD_SUB, SEMANA, MUN_PRO, COD_EVE;
	INTO CURSOR rsConteoBrotes READWRITE 

SELECT COD_EVE, DATE() AS FEC_NOT, SEMANA, AÑO , COD_PRE, COD_SUB, ;
	SUM(IIF(UNI_MED!='1',1,0)) AS GRU_1,;
	SUM(IIF(UNI_MED='1',IIF(BETWEE(VAL(EDAD),1,4),1,0),0)) AS GRU_2,;
	SUM(IIF(UNI_MED='1',IIF(BETWEE(VAL(EDAD),5,14),1,0),0)) AS GRU_3,;
	SUM(IIF(UNI_MED='1',IIF(BETWEE(VAL(EDAD),15,44),1,0),0)) AS GRU_4,;
	SUM(IIF(UNI_MED='1',IIF(BETWEE(VAL(EDAD),45,64),1,0),0)) AS GRU_5,;
	SUM(IIF(UNI_MED='1',IIF(VAL(EDAD)>=65,1,0),0)) AS GRU_6,;
	SUM(IIF(tip_cas='1',1,0)) AS cas_pro,;
	SUM(IIF(tip_cas='2',1,0)) AS cas_conl,;
	SUM(IIF(tip_cas='3',1,0)) AS cas_conc,;
	SUM(IIF(tip_cas='4',1,0)) AS cas_conn,;
	SUM(IIF(CON_FIN='1',1,0)) AS VIVOS,;
	SUM(IIF(CON_FIN='2',1,0)) AS MUERTOS, COD_MUN as mun_pro;
	FROM paciente as recordSource;
	WHERE &FIL_NOT&FIL_EVE &FIL_VIG &n_inmediata AND COD_EVE!='000 ';
	GROUP BY AÑO, COD_PRE, COD_SUB, SEMANA, COD_MUN, COD_EVE;
	INTO CURSOR rsConteoIndividuales

SELECT rsConteoBrotes 
APPEND FROM DBF('rsConteoIndividuales')

CREATE TABLE PLANO FREE (PARTE1 C(254))
SELECT rsConteoBrotes 
GO TOP
SCAN
   PLA=COD_EVE+','+TF(FEC_NOT)+','+SEMANA+','+AÑO+','+COD_PRE+','+COD_SUB+','+NC(GRU_1)+','+NC(GRU_2)+','+;
      NC(GRU_3)+','+NC(GRU_4)+','+NC(GRU_5)+','+NC(GRU_6)+','+NC(CAS_PRO)+','+NC(CAS_CONL)+','+NC(CAS_CONC)+','+NC(CAS_CONN)+','+;
      NC(HOMBRES)+','+NC(MUJERES)+','+NC(VIVOS)+','+NC(MUERTOS)+','+ LEFT(MUN_PRO,2)+','+RIGHT(MUN_PRO,3)

   INSERT INTO PLANO (PARTE1) VALUES (PLA)
ENDSCAN
SELECT PLANO
COPY TO (TOT) SDF
USE IN rsConteoBrotes
USE IN rsConteoIndividuales
REG_C=_TALLY


*Genera el archivo XLS de notificación negativa de eventos 
IF SelectTable("rsNegativeNotificationReport",,.F.) THEN
	=showProgressMessage('Generando Archivo XLS de notificación negativa de eventos')
	oDataExporter.sSourceTableName = "rsNegativeNotificationReport"
	oDataExporter.sExportedFileName = "Eventos_Notificacion_Negativa_Semana_" + TRANSFORM(m.SEM_REP,'@L 99') + "_" + STRTRAN(TTOC(DATETIME(),3),":","_")
	oDataExporter.exportToXLS()
ENDIF

*Genera el archivo SIVIGILA contenedor de datos de cuándo y con qué versión fueron generados ;
los archivos planos de notificación o retroalimentación
DO makeSIVIGILAVersionFile WITH (INS), (_ver) IN PlainsLib

L=0
SET DEVICE TO FILE (CON)
IF REG_U<>0
   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'U'+TA+','+TRANS(REG_U,'@l 9999999')
   L=L+1
ENDIF
IF REG_T<>0
   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'R'+TA+','+TRANS(REG_T,'@l 9999999')
   L=L+1
ENDIF
IF REG_P<>0
   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'N'+TA+','+TRANS(REG_P,'@l 9999999')
   L=L+1
ENDIF
IF REG_B<>0
   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'B'+TA+','+TRANS(REG_B,'@l 9999999')
   L=L+1
ENDIF
IF REG_C<>0
   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'T'+TA+','+TRANS(REG_C,'@l 9999999')
   L=L+1
ENDIF
IF REG_D<>0
   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'D'+TA+','+TRANS(REG_D,'@l 9999999')
   L=L+1
ENDIF

* Comentado por: wilson aguilar	Fecha: 20/10/2011 ;
El Grupo SIVIGILA del INS decidió que con la veersión 2012, el Sistema ya no debe generar el archivo W. 
&&IF REG_W<>0
&&   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'W'+TA+','+TRANS(REG_W,'@l 9999999')
&&   L=L+1
&&ENDIF

IF REG_I<>0
   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'I'+TA+','+TRANS(REG_I,'@l 9999999')
   L=L+1
ENDIF
IF REG_V<>0
   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'V'+TA+','+TRANS(REG_V,'@l 9999999')
   L=L+1
ENDIF

SET DEVICE TO SCREEN 
*SET STEP ON 
*Codifica los archivos recien producidos
sSivigilaCryptoFileNameAndPath = PATH_TO_SIVIGILACRYPTO + "SivigilaCrypto.prg"
oCodifier  = NEWOBJECT( "SivigilaCrypto", sSivigilaCryptoFileNameAndPath, .NULL., RUT_SEM, "*.Txt", RUT_SEM)
oCodifier.BatchCodify
RELEASE oCodifier

*Produce un archivo empaquetado en formato ZIP para envío a la Unidad Jerarquíca Superior
bClaimNotificationIsFinished=.T.
TRY
	*Establece el nombre del archivo ZIP correspondiente a los planos generados
	sPlainsFilesPath = JUSTPATH(RUT_SEM)
	sTargetZIPFileName = JUSTFNAME(sPlainsFilesPath) + '.ZIP'
	sTargetXLSZIPFileName = JUSTFNAME(sPlainsFilesPath) + '_XLS' + '.ZIP'
	
	sResultMsg=''
	lFileZip=.F. &&Para procesos de enviar correo verificar si el archivo zip fue creado
	DO ZIP WITH RUT_SEM, '*.TXT', sExportationPath+'\', sTargetZIPFileName, sResultMsg
	*DO ArchiveFiles WITH RUT_SEM, '*.TXT', sExportationPath+'\', sTargetZIPFileName, sResultMsg IN backup
*SET STEP ON 	
	DO ZIP WITH RUT_SEM, '*.XLS', sExportationPath+'\', sTargetXLSZIPFileName , sResultMsg, .T.
	*DO ArchiveFiles  WITH RUT_SEM, '*.XLS', sExportationPath+'\', sTargetXLSZIPFileName , sResultMsg, .T. IN backup
	IF EMPTY(sResultMsg) THEN
		*sTargetZIPFileNameAndPath = sPlainsFilesPath + '.ZIP'
		*COPY FILE (sTargetZIPFileName) TO (sTargetZIPFileNameAndPath)
		lFileZip=.T.
	ELSE
		bClaimNotificationIsFinished = .F.
		sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
					sResultMsg + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
					"Debe crear usted mismo el archivo ZIP para envío a la Unidad Jerarquíca Superior."
	ENDIF
	*DELETE FILE (sTargetZIPFileName)
CATCH TO oException
	bClaimNotificationIsFinished = .F.
	sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
				"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
				"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
				"Debe crear usted mismo el archivo ZIP para envío a la Unidad Jerarquíca Superior."
ENDTRY

*SET STEP ON 
*Actualiza el campo NOTIFICA de las tablas maestras con el número de la semana que se notificó
IF opc_exp = ORDINARY_NOTIFICATION THEN
	*Se trata de una notificación de información usual, esto es, no es de caracter inmediato ni es;
	una retroalimentación
	
	*Establece la condición de actualización de los registros de las tablas maestras
	updateCondition = 'NOTIFICA=0'
	
	*Comentado por wilson Aguilar 03/12/2009;
	La siguiente parte del código debe incorporarse si el INS decide que las notifcaciones de caracter;
	inmediato no sean re-notificadas, es decir, si decide que una vez que se ha hecho una ;
	notificación inmediata, en un tiempo posterior no pueda volver a realizarse la notificación de ;
	los registros que fueron objeto de notificación inmediata. En ese caso, también tendrá que;
	cambiarse la condición del IF anterior por IF opc_pexp<>2 THEN ...
	*IF !EMPTY(n_inmediata) then
	*	updateCondition=updateCondition + ' AND ' + n_inmediata
	*ENDIF
	
	=showProgressMessage('Cerrando registros...')

	IF VARTYPE(glDisableRules)='L' THEN
		lOldDisableRules=glDisableRules
	ELSE
		lOldDisableRules=.F.
	ENDIF
	glDisableRules=.T.
	
	sDataTablaToUpdate = 'PACIENTE'
	DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
	REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR NOTIFICA=0 AND EST_INGR!=2

	USE BDSivigila!vTablasDeDC IN 0
	SELECT vTablasDeDC
	SCAN
		sDataTablaToUpdate = ALLTRIM(vTablasDeDC.DC_TABLA)
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		IF vTablasDeDC.ID_DC_TABLA != 47 THEN
			REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition
		ELSE
			*La tabla EVENTOS_47 almacena registros de un evento cuyos datos pueden ser cargados desde un sistema externo
			REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR NOTIFICA=0 AND EST_INGR!=2
		ENDIF
	ENDSCAN

	sDataTablaToUpdate = 'BROTES'
	DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
	REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

	sDataTablaToUpdate = 'LABORATORIOS'
	DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
	REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

	sDataTablaToUpdate = 'CONTACTOS'
	DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
	REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

	sDataTablaToUpdate = 'SEGUIMIENTOCONTACTOS'
	DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
	REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

	sDataTablaToUpdate = 'UPGD'
	DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
	REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

	sDataTablaToUpdate = 'TAL_HUM'
	DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
	REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

	sDataTablaToUpdate = 'UPGD_UCIS'
	DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
	REPLACE NOTIFICA WITH M.SEM_REP, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

	sDataTablaToUpdate = 'GENERAL'
	DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
	IF VAL(SEMANA)<M.SEM_REP
	   REPLACE SEMANA WITH nc(M.SEM_REP)
	   FLUSH
	   _SEM_RP=VAL(SEMANA)
	ENDIF
	glDisableRules=lOldDisableRules
ELSE
	IF opc_exp = INMEDIATE_NOTIFICATION THEN
		*Se trata de una notificación de información de caracter inmediato 
		
		*Establece la condición de actualización de los registros de las tablas maestras
		updateCondition = ' INMEDIATA="SI" AND EST_NOTIF=' + NOT_NOTIFIED
		
		=showProgressMessage('Cerrando registros...')

		IF VARTYPE(glDisableRules)='L' THEN
			lOldDisableRules=glDisableRules
		ELSE
			lOldDisableRules=.F.
		ENDIF
		glDisableRules=.T.
		
		sDataTablaToUpdate = 'PACIENTE'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR NOTIFICA=0 AND &updateCondition 

		USE BDSivigila!vTablasDeDC IN 0
		SELECT vTablasDeDC
		SCAN
			sDataTablaToUpdate =  ALLTRIM(vTablasDeDC.DC_TABLA)
			DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
			REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition
		ENDSCAN

		sDataTablaToUpdate = 'BROTES'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition

		sDataTablaToUpdate = 'LABORATORIOS'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition

		sDataTablaToUpdate = 'CONTACTOS'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition

		sDataTablaToUpdate = 'SEGUIMIENTOCONTACTOS'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition

		glDisableRules=lOldDisableRules
	ENDIF
ENDIF
CLOSE TABLES ALL
=showProgressMessage('Producción de archivos planos finalizada')



*-------------------------------------------------------------------------
* Actualizacion Autor: José Luis Gonzalez, Fecha: 23/10/2013;
	Propósito: si se trata de una "Notificacion Inmediata" enviar por correo electronico; 
	detalles de la notificacion y los archivos adjuntos, como base el archivos el zip ;
	o los archivos individuales si no se logro general el zip
IF !EMPTY(n_inmediata) THEN
	IF lFileZip THEN
		sAdjuntosSendMail = sExportationPath + '\' + sTargetZIPFileName
	ELSE
		sAdjuntosSendMail = bro + ".TXT;" + car + ".TXT;" + tal + ".TXT;" + dat + ".TXT;" + tot + ".TXT;" + con + ".TXT;" + stt + ".TXT;" + ptw + ".TXT;" + ins 
	ENDIF
*SET STEP ON 
	*Codigo para comunicacion inmediata : 12
	IF !_SCREEN.bTestMode THEN
		sSendMailCmd = "DO SendMailManual WITH 12, '', '' , sAdjuntosSendMail IN " + PATH_TO_SIVIGILAMailer + "sendmail.prg"
		&sSendMailCmd
	ENDIF
ENDIF
*-------------------------------------------------------------------------


x=SUBSTR(RUT_SEM,RAT('\',RUT_SEM,2)+1)
FINAL=sExportationPath + "\" +  x		&& PARTE DEL MENSAJE ...

xx='.TXT Con ' &&+ CHR(9)

=showProgressMessage('Proceso Terminado')
sPlainsExportedMsg = 'SE HAN GENERADO LOS SIGUIENTES ARCHIVOS DE TEXTO!'+CHR(13)+CHR(13)+;
					   IIF(!bIsFeedback,RIGHT(CAR,8)+xx+TRANS(REG_U,'@l 99999999')+' - Registros de UPGD'+CHR(13),'')+;
					   IIF(!bIsFeedback,RIGHT(TAL,8)+xx+TRANS(REG_T,'@l 99999999')+' - Registros de Talento H / Servicios'+CHR(13),'')+;
					   RIGHT(PTE,8)+xx+TRANS(REG_P,'@l 99999999')+' - Registros de Not. Individual'+CHR(13)+;
					   RIGHT(DAT,8)+xx+TRANS(REG_D,'@l 99999999')+' - Registros de Datos Complementarios'+CHR(13)+;
					   RIGHT(BRO,8)+xx+TRANS(REG_B,'@l 99999999')+' - Registros de Not. Colectiva'+CHR(13)+;
					   RIGHT(TOT,8)+xx+TRANS(REG_C,'@l 99999999')+' - Registros de Not. Colectiva Reporte'+CHR(13)+;
					   RIGHT(IRA,8)+xx+TRANS(REG_I,'@l 99999999')+' - Registros de Morb. por IRA'+CHR(13)+;
					   IIF(!bIsFeedback,RIGHT(UCIFileName,8)+xx+TRANS(REG_V,'@l 99999999')+' - Registros de UCI por UPGD','')+CHR(13)+;
					   RIGHT(CON,8)+xx+TRANS(L,'@l 99999999')+' - Registros de Control'+CHR(13)+CHR(13)+;
					   'UBÍQUELOS EN '+FINAL+ IIF(VAL(SUBSTR(OS(),8,5))>=6,'',CHR(13)+CHR(13)+;
					   'POR FAVOR ENVIE EL ARCHIVO COMPRIMIDO: '+CHR(13)+FINAL+'.ZIP'+CHR(13)+;
					   'AL INSTITUTO NACIONAL DE SALUD MAIL: sivigila@ins.gov.co')
						* Comentado por: wilson aguilar	Fecha: 20/10/2011 ;
						El Grupo SIVIGILA del INS decidió que con la veersión 2012, el Sistema ya no debe generar el archivo W;
						por tanto la siguiente línea de código que hace parte del mensaje para el usuario, se suprimió
					   &&RIGHT(PTW,8)+xx+TRANS(REG_W,'@l 99999999')+' - Registros de Not. WEB'+CHR(13)+;

IF !bBeSilent THEN
	=showErrorMessage(sPlainsExportedMsg,0)	
ELSE
	=FPUTS(nResultsFileHandler,sPlainsExportedMsg)
ENDIF

EXPO=1

IF !bClaimNotificationIsFinished THEN
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
	IF !bBeSilent THEN
		=showErrorMessage(sResultMsg, 0)
	ELSE
		=FPUTS(nResultsFileHandler,sResultMsg)
	ENDIF
ENDIF

sEndTime=TIME()
IF bBeSilent THEN
	=FPUTS(nResultsFileHandler,'Hora de inicio: ' + sInitTime)
	=FPUTS(nResultsFileHandler,'Hora de finalización: ' + sEndTime)			
ENDIF

SET DEFAULT TO (_DIR_ACT)

DO cleanTheHouse IN IMPORTAR_CLICK

SET PROCEDURE TO &sOldProcedure 	

=hideProgressMessage()
