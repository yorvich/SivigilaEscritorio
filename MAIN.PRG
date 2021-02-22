#INCLUDE SIVIGILA.H

CLEAR ALL

PUBLIC nMajor,nMinor,nBuild
*Números que identifican la versión específica del Software SIVIGILA

PUBLIC _MaxEvents
*Número de tablas de datos complementarios de eventos de salud pública incorporados en la versión 2010 del Sistema

PUBLIC UPGD_UNIT as String, UNM_UNIT as String, UND_UNIT as String
*Tipos de unidades notificadoras en el sistema ;
Unidad Primaria Generadora del Dato (UPGD) ;
Unidad Notificadora Municipal (UNM) ;
Unidad Notificadora Departamental (UND) 

LOCAL sOldSetExclusive as String 
sOldSetExclusive = SET("Exclusive")

* Impide la ejecución si se trata de una versión limitada en el tiempo
*IF DATE()>{^2018-02-23} THEN
*	DO showErrorMessage WITH 'Se ha excedido el tiempo límite para usar esta aplicación.' ,0 IN SIVIGILAMessenger
*	CANCEL
*ENDIF

*Establece el ambiente de trabajo para el sistema
DO setSivigilaEnvironment

*Agrega la propiedad "Unidad notificadora" al aplicativo
*sOldSetExclusive = SET("Exclusive")
_SCREEN.AddProperty('sNotifierUnit',fieldValue("UNI_NOT","GENERAL"))
*SET EXCLUSIVE &sOldSetExclusive

*Agrega al aplicativo un objeto "Unidad notificadora" que contiene la caracterización de la unidad.
SET PROCEDURE TO NotifierUnit ADDITIVE
_SCREEN.AddObject('oNotifierUnit',"NotifierUnit",.T.,.T.)

_SCREEN.AddProperty('bTestMode',.F.)

*Impide que se ejecuten dos o más instancias del SIVIGILA excepto cuando la Unidad notifcadora es el INS
SET EXCLUSIVE OFF
SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
TRY
	vSUFile = FILETOSTR('su') 
CATCH TO oeXCEPTION
ENDTRY
IF !(fieldValue("TIP_UNI","GENERAL")=UND_ID AND fieldValue("UNI_NOT","GENERAL")=INS_ID AND IIF(VARTYPE(vSUFile)='C',vSUFile = CHR(13)+chr(10)+CHR(7),.F.)) THEN
	IF IsAppRunning("SIVIGILA001") THEN
		QUIT
	ENDIF
ENDIF

SET SYSMENU TO DEFAULT 
on key label f12 quit

_ver = APP_NAME +  " - "

* Establece el tipo de versión correspondiente al aplicativo
bVersionType=nDemoVersion 
bVersionType=nCompleteVersion 

DO resetSivigilaVersion

PUBLIC gsSivigilaRecordVersion AS String
gsSivigilaRecordVersion = ALLTRIM(_ver) + ALLTRIM(STR(nMajor)) + '.' + ALLTRIM(STR(nMinor)) + '.' + ALLTRIM(STR(nBuild)) +;
							'$' + ALLTRIM(STR(nCurrentBuild_Title)) + '$' + RIGHT(sUPDATE_SUBVERSION,3) 
							&& La siguiente parte solo es usada cuando se liberan subversiones: '$' + ALLTRIM(STR(nCurrentBuild_Title)) + '$' + RIGHT(sUPDATE_SUBVERSION,3) 

*Lanza el módulo de descarga de actualizaciones
sPathToSivigilaUpdaterFileNameAndPath = PATH_TO_SIVIGILAUPDATER + "SivigilaUpdater.prg"
goSivigilaUpdater = NEWOBJECT("SivigilaUpdater",sPathToSivigilaUpdaterFileNameAndPath )
IF NOT goSivigilaUpdater.launchSivigilaDownloader()
	sErrorMessage = "El Sistema SIVIGILA no ha podido lanzar un módulo fundamental. El Sistema SIVIGILA se ha estropeado." + CR_LF  + CR_LF + ;
					"Por favor, comuníquese con la Mesa de Ayuda en el INS para solucionar el problema"
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
	=showErrorMessage(sErrorMessage, 1)
	RETURN
ENDIF

_MaxEvents = n_SIVIGILA_EVENTS


*Establece los tipos de unidades notificadoras en el sistema
UPGD_UNIT = '1'
UNM_UNIT = '2'
UND_UNIT = '3'


*Identificadores de errores manejados por la aplicación
PUBLIC DATA_VALIDATION_ERROR as Integer, RECORD_VALIDATION_ERROR as Integer
DATA_VALIDATION_ERROR = 1582
RECORD_VALIDATION_ERROR = 1583


PUBLIC gbIsInmediateNotification as Boolean
* Permite identificar si en la captura de un caso, el usuario ha establecido que el caso va a ser grabado;
en el contexto de una notificción inmediata y, en consecuencia, solo deben hacerse obligatorios los campos definidos en ;
Tables.ConfiguracionNotificacionInmediata, independientemente de las reglas de validación establecidas por defecto para las variables ;
capturadas

PUBLIC gsInmediateToOrdinary as String
* Permite establecer si en la captura de un caso, se ha pasado del estado "Captura por notificación inmediata" al estado ;
"Captura rutinaria". Inicialmente todos los casos se capturan en el estado "Captura rutinaria". Los valores de esta variable deben ;
fijarse según la siguiente tabla de escenarios o cambios de estado:

*Captura por notificación inmediata		Captura rutinaria
*			0									0
*			0									1
*			1									0
*			1									1
*En la actual implementación, tan solo se usan los escenarios 1 y 3
gsInmediateToOrdinary  = '00'


PUBLIC gsLoadedToOrdinary as String
gsLoadedToOrdinary = '00'
* Permite establecer si en la captura de un caso, se ha pasado del estado "Original cargado desde un sistema externo" al estado ;
"Captura rutinaria". Inicialmente todos los casos se capturan en el estado "Captura rutinaria". Los valores de esta variable deben ;
fijarse según los escenarios o cambios de estado establecidos en EstadosCargaDesdeSistemasExternos.isf:


_SCREEN.WINDOWSTATE=2
_SCREEN.caption=_ver
_SCREEN.closable=.f.
_SCREEN.icon='net05.ico'

sScreenHeight = FONTMETRIC(1, _SCREEN.FONTNAME, _SCREEN.FONTSIZE) * SROWS()
sScreenWidth = FONTMETRIC(6, _SCREEN.FONTNAME, _SCREEN.FONTSIZE) * SCOLS()
DO CASE
	CASE sScreenWidth = 1360 OR sScreenWidth = 1366 OR sScreenWidth = 1364
		_SCREEN.picture = 'C:\USERS\WILSON.AGUILAR\DOCUMENTS\PROYECTOSIVIGILA\DESARROLLO\IMAGENESSIVIGILA2018\BackGround_1366x768.jpg'
	CASE sScreenWidth = 1600 OR sScreenWidth = 1680
		_SCREEN.picture = 'C:\USERS\WILSON.AGUILAR\DOCUMENTS\PROYECTOSIVIGILA\DESARROLLO\IMAGENESSIVIGILA2018\BackGround_1680x1050.jpg'
	CASE sScreenWidth = 1440
		_SCREEN.picture = 'C:\USERS\WILSON.AGUILAR\DOCUMENTS\PROYECTOSIVIGILA\DESARROLLO\IMAGENESSIVIGILA2018\BackGround_1440x900.jpg'	
	CASE sScreenWidth = 1280
		_SCREEN.picture = 'C:\USERS\WILSON.AGUILAR\DOCUMENTS\PROYECTOSIVIGILA\DESARROLLO\IMAGENESSIVIGILA2018\BackGround_1280x800.jpg'	
	OTHERWISE
		_SCREEN.picture = 'C:\USERS\WILSON.AGUILAR\DOCUMENTS\PROYECTOSIVIGILA\DESARROLLO\IMAGENESSIVIGILA2018\BackGround_1980x1322.jpg'	
ENDCASE

*SET CLASSLIB TO SIVIGILAImgWallPaper ADDITIVE 
*_SCREEN.AddObject('oWallPaper', "SIVIGILAImgWallpaper")

_SCREEN.refresh

set sysmenu save
set sysmenu to

SET RESOURCE TO .\FOXUSER.DBF

*Desbloquea las tablas del sistema para que puedan ser accesadas
*DO unlockSIVIGILATables IN SIVIGILASystem

*Verifica la integridad de las tablas del sistema. En caso de encontrar problemas, checkSivigilaSystemTables tratará de ;
corregirlos y, en cualquier caso, cerrará el sistema para ser reiniciado por parte del usuario.
*Comentado por:	Wilson Aguilar	Fecha:02/02/2020;
Se decidió eliminar esta pare de código por cuanto, básicamente, no está haciendo nada
*WAIT WINDOW "Chequeando la Base de datos" nowait
*DO checkSivigilaSystemTables IN SivigilaSystem WITH _MaxEvents 
*WAIT clear

dSivigila2010LaunchDate = {^2018-02-05}

*CTOD('08/01/2018')
*Fecha de lanzamiento del Sistema SIVIGILA 2018 1.0.0.

*CTOD('26/11/2016')
*Fecha de lanzamiento del Sistema SIVIGILA 2017.

*CTOD('29/11/2015')
*Fecha de lanzamiento del Sistema SIVIGILA 2016.

*CTOD('01/12/2014')
*Fecha de lanzamiento del Sistema SIVIGILA 2015.

*CTOD('02/01/2014')
*Fecha de lanzamiento del Sistema SIVIGILA 2014.

*CTOD('31/12/2011')
*Fecha de lanzamiento del Sistema SIVIGILA 2012.

*CTOD('01/05/2010')
*Fecha de lanzamiento del Sistema SIVIGILA 2010.
* Utilizada para controlar la activación de UPGDs

*La fecha de lanzamiento del sistema 2010 fue 01/05/2010. La versiones posteriores son tratadas internamente como;
una mejora adicional al mismo sistema 2010.

True=.T.
False=.F.

_version='Instituto Nacional de Salud - Todos los derechos reservados!'
_SCREEN.caption=_ver

_dir_act=sys(5)+sys(2003)+'\'
_dir_ini=_dir_act

_cod_acc=''
_rta=0
area_tabla=0
var_gua=''

store '' to _contraseña

*Establece el manejador de errores del SIVIGILA
RELEASE goErrorLog
PUBLIC goErrorLog
PUBLIC gsErrorMsg
goErrorlog = NEWOBJECT( 'xErrLog', 'errorlog.prg', NULL )
gsErrorMsg = ""
ON ERROR DO TrackErr WITH SET("DATASESSION"), PROGRAM(), LINENO(), _mydocument+'\ARCHIVOS DE ERROR'

**********************************************************
_dir = SYS(2023) + '\' + APP_NAME
if !directory(_dir)
   mkdir (_dir)
endif
_dir=_dir+'\usu1'

gsSIVIGILADataDir = '.\'

_mydocument=HOME(8)
_mydocument=SUBSTR(_mydocument,1,LEN(_mydocument)-1)

S=_mydocument
CS=SUBSTR(S,LEN(S),1)
X=1
DO WHILE CS!='\'
   S=SUBSTR(_mydocument,1,LEN(_mydocument)-X)
   CS=SUBSTR(S,LEN(S),1)
   X=X+1
ENDDO
_mydocument = SUBSTR(_mydocument,1,LEN(_mydocument)-X) + '\' + APP_NAME
if !directory(_mydocument)
   mkdir (_mydocument)
ENDIF

_RTA_PLANOS=_mydocument+'\ARCHIVOS PLANOS'
if !directory(_RTA_PLANOS)
   mkdir (_RTA_PLANOS)
ENDIF

_RTA_XLS=_mydocument+'\ARCHIVOS XLS'
if !directory(_RTA_XLS)
   mkdir (_RTA_XLS)
ENDIF

*Crea las carpetas en donde se almacenan tablas intemedias utilizadas al cargar archivos planos
_RTA_CONSOLIDA=''
sCmd = "DO MKDirs WITH '" + _mydocument + "\ARCHIVOS DE CONSOLIDACION' IN " + PATH_TO_COMMON_LIB + "FilesHandler"
&sCmd
sCmd = "DO MKDirs WITH '" + _mydocument + "\ARCHIVOS DE CONSOLIDACION\VERSION 2008' IN " + PATH_TO_COMMON_LIB + "FilesHandler"
&sCmd
sCmd = "DO MKDirs WITH '" + _mydocument + "\ARCHIVOS DE CONSOLIDACION\VERSION 2010' IN " + PATH_TO_COMMON_LIB + "FilesHandler"
&sCmd

**********************************************************


*JLGB : 04/04/2020
*Elimina Archivos remanentes de instalaciones previas si es necesario
sFileDel = "FiltrarArchivos.bat"
IF FILE(sFileDel) THEN
	
	LOCAL sBatFilenameToRun as String, sBatFileContent as String, sRunCmd as String
	
	sBatFilenameToRun =  "FiltrarArchivos01.bat"
	
	*Para evitar problemas de código de página con el archivo .bat, lo convierte a CodePage 850-International MS-DOS
	sBatFileContent  = CPCONVERT(CPCURRENT(),850,FILETOSTR(sFileDel))
	=STRTOFILE(sBatFileContent, sBatFilenameToRun)
	sRunCmd = 'RUN  ' +  sBatFilenameToRun 
	&sRunCmd
	
	*Borra el archivo .bat recién construido
	DELETE FILE "&sBatFilenameToRun"
	DELETE FILE "&sFileDel"
ENDIF


**********************

directiva='REPORTES/POSTER_INS.JPG'

_raz_soc=''
_cod_pre=''

tecla_pres='LASTKEY()=5 OR LASTKEY()=19 OR LASTKEY()=127 OR LASTKEY()=31 OR LASTKEY()=27'
*	5: up arrow;
	19: left arrow;
	127: backspace;
	31: ?;
	27: esc
	
tec_gri='LASTKEY()=5 OR LASTKEY()=24'

store 0 to for_reg,tmp_f_a_sal,fa
store '' to tmp_f_a

fic_ya_not='La ficha ya fue notificada.'+CHR(13)+'Si desea realizar cambios a esta información,'+CHR(13)+'Por favor Realice un Ajuste a la Notificación Individual.'
_12 = '1- Si, 2- No'
_123='1-Si, 2-No, 3-Desconocida'
_12N='1-Si, 2-No, 3-No Aplica'
_129='1-Si, 2-No, 9-Desconocida'
_h_m='Formato HH:MM,  use el separador ":" para horas y minutos: Ej 16:15 o 06:09'
_v12="tit('Valores: 1 y 2')"

TTT_DOC='DOCUMENTO DE IDENTIDAD'+CHR(13)+;
		'Si no se tiene el doc. del paciente se debe ingresar:'+CHR(13)+;
		'Si es menor de edad, El doc. de la madre y un número consecutivo -posiblemente el del Nacimiento-'+CHR(13)+;
		'Si es mayor de edad, un número consecutivo'
		
_txt='Texto...'
_num='Numérico...'
_cerrar='Click para Cerrar este formulario, o Presione la Tecla ESC'
_dma='Formato DD/MM/AAAA'
_dat_pte='Primero se deben almacenar los datos del paciente'

specialChars = "+.\/:*?',{}<>_&;^()=#$%[]" + '"' 
_car_esp = "This.value=chrtran(This.value,specialChars,'')"
_car_esp2=_car_esp
RemoveSpecialChars = "This.value=chrtran(This.value,specialChars,'')"


sChangePatientStateAdjustments = "' 0 3 4 5 6 D '"
*Ajustes que implican cambios en la clasificación de un caso según Diccionario de Datos del Sistema

sChangeAnyValueAdjustment='7'
*Ajuste que implica cambio en cualquier valor de cualquier variable de un caso según Diccionario de Datos del Sistema


_admo='Administrador del Sistema'
_ucs='No se le ha asignado acceso para operar este módulo'+chr(13)+'Por favor solicite autorización al '+_admo
_xls='Desea guardar el resultado en un archivo XLS...'
_esc=''
_val="tit('Validando datos permitidos, Por favor espere...')"
_nomb_usua=''
_nom_acc=''
_nivel=''
_rt_empresa=''
FORMU='' &&V ESC FOR

STORE '' TO m.cod_pre,m.cod_sub, m.raz_soc, _tip_ide, _num_ide, _cod_eve, _fec_not, m.num_ide, m.tip_ide, m.fec_not, m.cod_eve,;
   TITULO,m.lugar,m.sexo,cod_lg, _AÑ_ANT, _SE_ANT, _CP_ANT, _CS_ANT, _TI_ANT, _NI_ANT, _CE_ANT, m.ini_sin, DAT1,DAT2, m.vigencia, ;
   m.cod_dep, m.cod_mun, m.mun_pro, m.ocupacion, m.cod_ase, m.ajuste, _val_sem, CTRL_PAIS

STORE 0 TO estado,m.flu_not,m.semana, _semana,_año, m.per_rep, m.sem_rep,nreg,pte_nue,;
    tot_DC, tot_pte, tot_dat, tot_bro, tot_rab, tot_vihg, tot_vihrn, tot_upgd, tot_th, total_uci_cargadas, tot_rh, tot_lab, con_pte,;
    tot_contactos, tot_seguimientos, con_dat, con_bro, con_upgd, con_th, con_uci, con_lab, m.tal_hum,;
    ti_cas,d_a_aj, tip_not, caj_aju,scconf,bd_ind_col, n_version,m.notifica,VER_MSG_PA

_semana_noti=''

tipo_obliga='' &&variable validacion

tie_ley=.T.


m.año=nc(YEAR(DATE()))
 
*Matricula la versión actual del Sistema SIVIGILA
DO updateSivigilaDownloaderIni IN SIVIGILASystem WITH _SCREEN.oNotifierUnit.sType

SELECT GENERAL
IF EMPTY(UNIDAD)
   REPLACE UNIDAD WITH LEFT(SYS(5),1)
   REPLACE VIGENCIA WITH NC(YEAR(DATE()))
ENDIF
_cod_pre=uni_not
_cod_sub=RIGHT(uni_not,2)
_raz_soc=raz_soc

_tip_uni=tip_uni
*Tipo de la unidad notificadora según aparece registrado en el campo GENERAL.TIP_UNI

_lsp=lsp
_fm=fm
_uni_not=uni_not
_prestador=''
_SEM_RP=VAL(SEMANA)
_vigencia=vigencia

_UN=''
*Código de la unidad notificadora según aparece registrado en el campo GENERAL.UNI_NOT

base_consulta=0

_ccrx=0  && control cod automatico PARA LA CASE BOTONES
_ccrv=''  && variable control

inicio=0
m.contra=space(10)
DO FORM acceso
IF inicio=0
   clear all
   SET SYSMENU TO DEFAULT 
   return
ENDIF 
CLOSE ALL

_dir=SYS(2023)+'\' + APP_NAME
if !directory(_dir)
   mkdir (_dir)
endif
_dir=_dir+'\usu1'

ret=0
nd=1
do while directory(_dir)
   _dir = SYS(2023) + '\' + APP_NAME + '\usu' + nc(nd)
   nd=nd+1
enddo
nd=nd-1
mkdir (_dir)

*modify window screen title _ver+ALLTRIM(_RAZ_SOC) color rgb(,,,128,128,128) icon file net05.ico noclose

DO SVG.mpr

_autoriza=_raz_soc

PUBLIC gen_col
gen_col=1

bus_pac=''

_FEC_SYS='La fecha ingresada es mayor a la fecha del Sistema'


&&_LLAVE="SEEK M.AÑO + M.SEMANA + M.COD_EVE + M.TIP_IDE + M.NUM_IDE"
_dat_pri="REPLACE AJUSTE WITH AJU_TMP, SEMANA WITH _SEMANA, AÑO WITH _AÑO, COD_PRE WITH _COD_PRE, COD_SUB WITH _COD_SUB, TIP_IDE WITH _TIP_IDE, NUM_IDE WITH _NUM_IDE, COD_EVE WITH _COD_EVE"


USE GENERAL IN 0
SELECT GENERAL
vg='EMPTY(GENERAL.UNI_NOT) OR EMPTY(GENERAL.RAZ_SOC)'

DO while EMPTY(GENERAL.UNI_NOT) OR EMPTY(GENERAL.RAZ_SOC)
   TIT('Configure el Sistema!')
   DO FORM GENERAL
   IF &vg
      t_sn('Desea Salir sin registrar los datos de'+CHR(13)+;
      'La UPGD que Notifica')
      IF _rta=6
         quit
      endif
   ELSE
      exit
   ENDIF
ENDDO

*Fija el título de la ventana de la aplicación
*_SCREEN.caption=_ver+'Revisión ' + NC(nCurrentMajor) + '.' + NC(nCurrentMinor) + '.' + NC(nCurrentBuild) + sCurrentLetter
_SCREEN.caption=_ver+'Revisión ' + NC(nCurrentMajor_Title) + '.' + NC(nCurrentMinor_Title) + '.' + NC(nCurrentBuild_Title) + sCurrentLetter + ' ' +;
				'Act.' + sCURRENT_PATCH_ID 
IF bVersionType = nDemoVersion  THEN
	_SCREEN.caption = _SCREEN.caption + " - Versión de demostración "
ENDIF
_SCREEN.caption = _SCREEN.caption  + ' - ' + _uni_not + ALLTRIM(raz_soc)


* La siguiente parte de código se ha excluido temporalmente por determinación del comité de cambios. En reunión efectuada;
el 06/07/2011, el comité determinó que la funcionalidad del recálculo se implantaría con la versión 2012 del sistema SIVIGILA
* Restablece las tablas al último backup de recalculo de semana epidemiologica efectuado. ;
	Si el último proceso efectuado no finalizó exitosamente.;
		   1) No hubo errores en el último recálculo. No se restauraron backups.;
		   0) Hubo errores en el último recálculo. Se restauraron backups.;
		  -1) Hubo errores en el último recálculo. No Se restauraron backups.;
	
&&sOldProcedure = SET('PROCEDURE')
&&SET PROCEDURE TO sivigilaUtilities
&&IF checkComputeEpidemiologicalWeek() <= 0
&&	CLOSE ALL
&&	QUIT
&&ENDIF

*Comentado por: Wilson Aguilar	Fecha: 21/06/2010;
Efectuado el análisis de funcionamiento de este PROCEDURE, en reunión efectuada el 18/06/2010, el INS autorizó su eliminación;
por cuanto su ejecución dá lugar a la creación de registros duplicados en las tablas maestras del sistema al cambiar el valor de;
la semana y año epidemiológicos de ocurrencia en los eventos. El análisis efectuado concluyó, además, que su ejecución en;
un Sistema SIVIGILA 2010 en donde no se haya hecho una migración previa a partir de un sistema 2008, siempre conducirá, a medida;
que se vaya utilizando el sistema, a la creación de registros duplicados. Ver Error con ID=316
&&DO AJUSTES_2008

SET PROCEDURE TO SIVIGILAMessenger.fxp ADDITIVE 
IF getYesOrNo("Desea ver los casos que están pendientes de ajuste", MSG_NOTIFICATION_TITLE ) = YES THEN 
	DO DetectAdjusmentsDelays WITH _RTA_XLS IN AnomaliesHandler
ENDIF 

STORE 0 TO missingNotifications362 
IF getYesOrNo("Desea ver los meses faltantes de notificación del evento 362", MSG_NOTIFICATION_TITLE ) = YES THEN 
	DO detectDelays_362 IN AnomaliesHandler WITH missingNotifications362, (_RTA_XLS)
ENDIF 

*JLGB 30/07/2018 : Ejecutar comandos al inicio de programa
DO TransformOnStartingOrFinish WITH .T. IN transferdatahandler.prg

DO FORM frmControlPanel
CLOSE ALL

sFollowupEvents=' 650 652 680 860 870 890 995 353 354 359 362 349 '
*Los eventos correspondientes a estos códigos son eventos que no obedecen al esquema de datos básicos + datos complementarios y ;
requieren tratamiento especial en algunos procesos de exportación.

sAdjustmentDBClosureValidationEvents = ''
* Los eventos en esta tienen fecha límite para reportar ajustes ;
	(hasta 31 Marzo para ajustes sobre eventos del año anterior)


* Establece los eventos que puede capturar la aplicación en función del tipo de versión correspondiente al aplicativo
IF bVersionType=nDemoVersion  THEN
	UPDATE ENTRA_03 SET ACCESO='N' WHERE SUBSTR(CLAVE,1,3)=='EVE' AND SUBSTR(CLAVE,4,3) IN ;
		(SELECT COD_EVE FROM EVENTOS WHERE ESTADO_EVE!='Demo')
	USE
	
	SELECT EVENTOS
	USE
ENDIF

WAIT WINDOW "Abriendo la Base de Datos" nowait
do bases
OPEN DATABASE BDSivigila
WAIT clear

_autoriza='INSTITUTO NACIONAL DE SALUD'


read event
RETURN

PROCEDURE cer_ses()
	
	STORE '' TO sErrorMessage
	
	LOCAL sBackupPath as String , sBackupTable as String 

	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	
	*Lanza transformaciones/procesos que deban ejecutarse al salir del aplicativo
	DO TransformOnStartingOrFinish WITH .F. IN transferdatahandler.prg
	    
	CLOSE DATABASES ALL 

	DO makeBackupDir WITH sBackupPath, APP_NAME, sErrorMessage IN Backup
	IF !EMPTY(sErrorMessage) THEN 
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		=showErrorMessage(sErrorMessage, 2)
	ELSE 
		sBackupTable = sBackupPath + '\backup'
		IF SelectTable(sBackupTable ,,.F.) THEN 
			IF EMPTY(backup.FECHA) OR (DATE()-backup.FECHA >= _SCREEN.oNotifierUnit.nBackupPeriod) THEN
			   DO makeSivigilaBackup IN Backup
			ENDIF
		ENDIF 
		CLOSE ALL  
	ENDIF
		 
	*Bloquea las tablas del sistema de tal forma que no puedan ser accesadas mediante aplicaciones de terceras partes
	*DO lockSIVIGILATables  IN SIVIGILASystem

	_SCREEN.RemoveObject('oNotifierUnit')	 
	set defa to (_dir_ini)
	set sysmenu to defa
	x=_dir+'\*.*'
	delete FILE(x)
	X=_dir+'\'
	TRY
		RMDIR (X)
	CATCH TO oException
	ENDTRY 
	 
	SET EXCLUSIVE ON 
	ON error   
	CAPSLOCK(.F.)

ENDPROC 


PROCEDURE resetMaxEvents
	_MaxEvents = n_SIVIGILA_EVENTS
ENDPROC


PROCEDURE setSivigilaVersion
	
	LPARAMETERS sSivigilaVersionNumbers as string

	*Establece los valores de las variables globales nMajor, nMinor y nBuild a aquellos que se encuentren en sSivigilaVersionNumbers. ;
	Se supone que sSivigilaVersionNumbers es una cadena de números de la forma a1.a2.a3
	
	nMajor=VAL(GETWORDNUM(sSivigilaVersionNumbers,1,'.') )
	nMinor=VAL(GETWORDNUM(sSivigilaVersionNumbers,2,'.') )
	nBuild=VAL(GETWORDNUM(sSivigilaVersionNumbers,3,'.') )
ENDPROC


PROCEDURE resetSivigilaVersion

	*Restablece los valores de las variables globales nMajor, nMinor y nBuild a sus valores internos por defecto según se definen;
	en SIVIGILA.h
		
	nMajor=nCurrentMajor_Internal
	nMinor=nCurrentMinor_Internal
	nBuild=nCurrentBuild_Internal
	gsSivigilaRecordVersion = _ver + ALLTRIM(STR(nMajor)) + '.' + ALLTRIM(STR(nMinor)) + '.' + ALLTRIM(STR(nBuild))

ENDPROC


FUNCTION SIVIGILADefaultXLSPath

	LOCAL sSIVIGILADefaultXLSPath as String

	TRY
		sSIVIGILADefaultXLSPath = _RTA_XLS
	CATCH TO oException
		sSIVIGILADefaultXLSPath = SYS(2003)
	ENDTRY
	RETURN sSIVIGILADefaultXLSPath
ENDFUNC



