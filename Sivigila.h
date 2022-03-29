#INCLUDE DevEnvironment.h

#INCLUDE GlobalConst.h
#INCLUDE SIVIGILAMessages.h
#INCLUDE SIVIGILADataUpdater.h
#INCLUDE SiviglaTxtDocumentoIdentidad.h
#INCLUDE CLS_BASIC_DATA.h
#INCLUDE CLS_UPGD.h
*----------------------------------------------------------------------------------------------------------------------------

*/ Sentencia incluida en: ;
	main.prg

#DEFINE APP_NAME "SIVIGILA - 2018"
*Nombre popular del aplicativo

#DEFINE INS_ID "110000000000"
*C�digo identificador del INS como Unidad notificadora

#DEFINE UPGD_ID "1"
#DEFINE nUPGD_ID 1

#DEFINE UNM_ID "2"
#DEFINE nUNM_ID 2

#DEFINE UND_ID "3"
#DEFINE nUND_ID 3

#DEFINE UNN_ID "1" &&Unidad notificadora nacional
*C�digos identificadores de los distintos tipos de Unidad Notificadora que se manejan en el Sistema


*----------------------------------------------------------------------------------------------------------------------------
*/ Sentencia incluida en: ;
	Forms.frmUPGDsCleaner

#DEFINE SUBUPGD_CODE_LENGHT 12
#DEFINE UPGD_CODE_LENGHT 10
#DEFINE UNM_CODE_LENGHT 5
#DEFINE UND_CODE_LENGHT 2
*Longitudes de los c�digos identificadores de los distintos tipos de Unidad Notificadora que se manejan en el Sistema


*----------------------------------------------------------------------------------------------------------------------------
*/ Sentencia incluida en: ;
 EventsHandler.prg

#DEFINE sConfigurationTableName "ConfiguracionDeObjetos"


*------------------------------------------- VERSIONAMIENTO DEL SISTEMA -----------------------------------------------------
*/Se considera que el SIVIGILA ha sido publicado en dos versiones principales: 2008 y 2010; sin embargo, de cara al p�blico se;
han producido otras versiones principales como la 2012 y posiblemente se seguir�n publicando otras versiones en el futuro ;
posiblemente conocidas con los nombres 2013, 2014 o algo semejante. ;

*Para todos los efectos, internamente se considera que solamente hay dos grandes versiones del SIVIGILA: 2008 y 2010. ;
De este modo, toda versi�n que de cara al p�blico se llame 2012, 2013 o algo semejante, internamente ser� tan solo una versi�n ;
2010 mejorada. Ello conlleva a manejar el versionamiento del Sistema con un conjunto de n�meros diferentes (Major.Minor.Build): ;
;
	- N�meros de cara al p�blico: dados por nCurrentMajor, nCurrentMinor y nCurrentBuild ;
	- N�meros internos: dados por nCurrentMajor_Internal, nCurrentMinor_Internal y nCurrentBuild_Internal ;
;
NOTA: en la actual implementaci�n sCurrentLetter y sCurrentLetter_Internal NO se usan

* Los n�meros identificadores de versi�n de cara al p�blico permiten, por ejemplo, producir archivos planos de notificaci�n ;
generados por un sistema SIVIGILA 2012 Versi�n 2.0.0 ;
Por otra parte, los n�meros internos identificadores de versi�n permiten que cuando esos archivos se carguen al Sistema, ;
internamente ellos sean leidos como generados por un Sistema 2010 versi�n 12.2.0

* Para que el esquema descrito funcione debe garantizarse en las definiciones #DEFINE siguientes que: ;
	1. nCurrentMajor_Internal es un n�mero >= al que se encuentre en el �ltimo registro de Tables.ExportQueries.Fields.Major
	2. nCurrentMinor_Internal = nCurrentMajor
	3. nCurrentBuild_Internal = nCurrentMinor

* A la fecha 01/07/2015 la pr�xima versi�n del Sistema ser�, de cara al p�blico, la 2016 y saldr� publicada oficialmente ;
como SIVIGILA 2016 Versi�n 1.0.0, e internamente se manejar� como SIVIGILA 2010 versi�n 16.1.0 ;
Las versiones de desarrollo antes de llegar a la versi�n a publicar, se manejar�n, de cara al p�blico, como SIVIGILA 2016 ;
versiones 0.1.0, 0.1.1, 0.1.2, ... ,0.2.0, 0.2.1, 0.2.2,... Internamente estas versiones de desarrollo se manejar�n como ;
SIVIGILA 2010 versiones 16.0.1, 16.0.2, 16.0.3, ... ,16.1.0, 16.1.1, 16.1.2

#DEFINE nCurrentMajor 4
#DEFINE nCurrentMinor 1
#DEFINE nCurrentBuild 0
#DEFINE sCurrentLetter ''

#DEFINE nCurrentMajor_Internal 18
#DEFINE nCurrentMinor_Internal 4
#DEFINE nCurrentBuild_Internal 1
#DEFINE sCurrentLetter_Internal ''

#DEFINE nCurrentMajor_Title 4
#DEFINE nCurrentMinor_Title 1
#DEFINE nCurrentBuild_Title 0


*/ Sentencias incluida en MAIN.PRG
*/ Sentencias incluida en MAKEAPP.PRG
#DEFINE nDemoVersion 2
#DEFINE nCompleteVersion 1
#DEFINE n_SIVIGILA_EVENTS 95
*/ Sentencias incluida en MAIN.PRG
*/ Sentencias incluida en MAKEAPP.PRG


#DEFINE sCURRENT_PATCH_OR_SUBVERSION ''
*Read _ as 1

#DEFINE sUPDATE_SUBVERSION '0.0.0'
* Constante que contiene el valor de la sub-version que se libera.
* se registra como minVersion en SivigilaDownloader.ini y utilizada como valor de comparacion por SivigilaDownloader

#DEFINE sCURRENT_PATCH_ID 'KB00082'
*�ltimo conocido KB00082

#DEFINE _VERSION2010 0
#DEFINE _VERSION2008 1
#DEFINE firstUncommonFieldOrdinalPos 8
*/ Sentencias incluidas en PLAINSLIB.PRG
*/ Sentencias incluidas en TRANSFERDATAHANDLER.PRG


#DEFINE sExportingMsg "Exportando a excel..."
*/ Sentencia incluida en SIVIGILAUTILITIES.DeleteSIVIGILAComplementaryData
*/ Sentencia incluida en SIVIGILAUTILITIES.UpdateSIVIGILAComplementaryData

#DEFINE FIELDS_ADMITTED_AS_EMPTY_VALUES  "BROTES.NUM_CON"
* nombres de campo en un evento colectivo que admiten valores nulos para la clave
*/ Sentencia incluida en SIVIGILAUTILITIES.BuildSqlForGivenCase

*----------------------------------------------------------------------------------------------------------------------------
*/ Sentencia incluida en: ;
	TransferDataHandler.prg

#DEFINE nSivigila2010Events 44
*N�mero de tablas eventos_?? con las que se lanz� originalmente el Sistema SIVIGILA 2010

#DEFINE nSIVIGILA2008EVENTS 32
*N�mero de tablas de datos complementarios de eventos(eventos_??) con las que se lanz� originalmente el Sistema SIVIGILA 2008

#DEFINE N_EVENTS_AFTER_SIVIGILA2010_LAUNCHING 32
*N�mero de tablas adicionales de datos complementarios de eventos que se han incorporado al SIVIGILA 2010 ;
luego del lanzamiento oficial del 06/05/2010


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	printDBPlusDCToFile.prg

#DEFINE MAX_SIVIGILA_DECIMAL_WIDTH 7
#DEFINE MAX_SIVIGILA_DECIMAL_PLACES 3


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	anomaliesSolver.prg ;
	SIVIGILAReporter.prg

#DEFINE NO_NOTIFICATION_EVENT_CODE "000"

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	SIVIGILAReporter.prg

#DEFINE MAX_EPIDEMIOLOGICAL_WEEKS 53
*N�mero m�ximo de semanas epidemiol�gicas en un a�o epidemiol�gico de acuerdo con la tabla CALENDARIO del Sistema


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.Acerca ;
	Forms.Geo_ref

#DEFINE RUN_CMD_FAILED 1405
*C�digo de error que se produce cuando falla la ejecuci�n de un comando RUN...


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.General

#DEFINE ACCESS_LOGICAL_DRIVE_FAILED 202
*C�digo de error que se produce cuando falla el acceso a un drive l�gico del computador


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	SIVIGILAIniMgr.prg ;
	SIVIGILADataUpdater.prg ;
	Forms.Acerca ;
	Forms.GeoRef ;
	Forms.Pte

#DEFINE SIVIGILA_INI_FILE_NAME "SivigilaRelatedApps"
*Nombre del archivo INI del Sistema SIVIGILA

#DEFINE SIVIRIPS_SECTION_NAME "SIANIESP"
*Nombre de la secci�n del archivo INI del Sistema en donde se guardan los items y valores de la aplicaci�n que d� acceso a RIPS

#DEFINE SIVIRIPS_GETFILE_TITLE_BAR_CAPTION "Seleccione el archivo ejecutable que d� acceso a RIPS"

#DEFINE EPIMAP_SECTION_NAME "EPIMAP"
*Nombre de la secci�n del archivo INI del Sistema en donde se guardan los items y valores de la aplicaci�n que d� acceso a EPIMAP

#DEFINE EPIMAP_GETFILE_TITLE_BAR_CAPTION "Seleccione el archivo ejecutable de EPIMAP"

#DEFINE WEBSITES_SECTION_NAME "UPGDsFile"

#DEFINE CHANGE_PATIENT_STATE_ADJUSTMENTS " 0 3 4 5 6 D "
*Ajustes que implican cambios en la clasificaci�n de un caso seg�n Diccionario de Datos del Sistema

#DEFINE CONFIRMATION_OR_DISCARD_ADJUSTMENTS " 3 4 5 6 D R "
*Ajustes que confirman o descartan un caso seg�n Diccionario de Datos del Sistema

#DEFINE CONFIRMATION_ADJUSTMENTS " 3 4 5 "
*Ajustes que confirman un caso seg�n Diccionario de Datos del Sistema

#DEFINE CHANGE_ANY_VALUE_ADJUSTMENT "7"
*Ajuste que implica cambio en cualquier valor de cualquier variable de un caso seg�n Diccionario de Datos del Sistema

#DEFINE CLINICAL_DISCARD_ADJUSTMENT "6"
*Ajuste por descarte cl�nico

#DEFINE CAPTURE_DISCARD_ADJUSTMENT "D"
*Ajuste por error de digitaci�n

#DEFINE LAB_CONFIRMATION_ADJUSTMENT "3"
*Ajuste correspondiente a confirmado por laboratorio

#DEFINE LAB_CONFIRMATION_ADJUSTMENT_2 "2"
*Ajuste correspondiente a confirmado por laboratorio -el valor aplica solo para ciertos eventos seleccionados-.

#DEFINE PROBABLE_ADJUSTMENT "2"
*Ajuste correspondiente a caso probable

#DEFINE ADJUSTMENT_DATE_FIELDNAME 'FEC_AJU'
*Nombre del campo correspondiente a la fecha de ajuste/grabaci�n de un registro del sistema

#DEFINE NA_AGE '0'
*Edad correspondiente a mortinatos o evento In Utero

#DEFINE IN_HOUSE_PRISION '000000000170'
*C�digo del establecimiento penitenciario CASA POR C�RCEL

#DEFINE IS_DEAD '2'
*C�digo que designa un paciente muerto

#DEFINE IS_ALIVE '1'
*C�digo que designa un paciente vivo

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	SIVIGILACommandButtons.vct

#DEFINE ALL_EVENTS_ADJUSTMENTS " 7 D "
*Ajustes que siempre se pueden aplicar a cualquier evento del Sistema


#DEFINE N_DEFAULT_WIDTH  1024
#DEFINE N_DEFAULT_HEIGHT 800
#DEFINE N_ADJUST_VALUE -25
#DEFINE SAVE_FORM_HEIGHT 495


#DEFINE	EVENTOS_24_NON_ADJUSTABLE_CONTROLS  "C1, FN ,A� ,SMN, SE, CI "
* Lista de controles del formulario Eventos_24 que no se habilitan para ajustes

#DEFINE	ADJUSTABLE_CONTROLS_650  "Txtinf_lab, TxtFechaF_TOM_MUE"
* Lista de controles del formulario Eventos_24 que, para el evento 650, se habilitan para cuando ajuste=Confirmado por laboratorio(2)

#DEFINE	EVENTOS_32_NON_ADJUSTABLE_CONTROLS  "FN, C1, CM, TxtA�o, TxtMes"
* Lista de controles del formulario Eventos_32 que no se habilitan para ajustes

#DEFINE	EVENTOS_28_NON_ADJUSTABLE_CONTROLS  "C1, FN, A�, SMN, SE, TD, ND"
* Lista de controles del formulario Eventos_28 que no se habilitan para ajustes

#DEFINE	EVENTOS_29_NON_ADJUSTABLE_CONTROLS  "C1, FN, A�, SMN, SE, TD, ND"
* Lista de controles del formulario Eventos_29 que no se habilitan para ajustes

#DEFINE	EVENTOS_31_NON_ADJUSTABLE_CONTROLS  "C1, FN, A�, SMN, SE, CM, TxtEspecie_ve"
* Lista de controles del formulario Eventos_31 que no se habilitan para ajustes

#DEFINE	EVENTOS_43_NON_ADJUSTABLE_CONTROLS  "UP, C2, C4, SMN, C3, CO, C5"
* Lista de controles del formulario Eventos_43 que no se habilitan para ajustes

#DEFINE	BROTES_NON_ADJUSTABLE_CONTROLS  "TxtCOD_EVE, TxtNombreEvento, C2, C4, C3, C5, CO, UP"
* Lista de controles del formulario BROTES que no se habilitan para ajustes

#DEFINE	UPGD_UCIS_NON_ADJUSTABLE_CONTROLS "TxtTIPO_UCI"
* Lista de controles del formulario frmUcis_x_upgd que no se habilitan para ajustes

#DEFINE	LABS_NON_ADJUSTABLE_CONTROLS "txtFec_exa, txtFec_rec, SivigilaTxtboxMuestra, SivigilaTxtboxSearcherPrueba, SivigilaTxtboxSearcherAgente, txtFec_exp, TxtValor"
* Lista de controles del formulario Laboraatorios que no se habilitan para ajustes

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.Eventos_46

#DEFINE	FOREIGN_UNKNOWN_MUNICIPALITY "99999"

#DEFINE	FOREIGN_UNKNOWN_STATE "01"

#DEFINE	COLOMBIA_COUNTRY_CODE "170"


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Modules.EventsHandler

#DEFINE	S_EXCLUDED_DB_CLOSURE_EVENTS " 456 457 550 228 115 155 850 813 342 551 "
* Lista de eventos que no tienen fecha l�mite para el cierre anual de notificaci�n. Usualmente todo evento tiene como fecha l�mite ;
m�xima para notificaci�n de casos el 31 Marzo del a�o en curso. Luego de esa fecha, no se pueden notificar casos del a�o anterior.

* Unidades de medida de la edad
#DEFINE	S_YEARS_UM "1"
#DEFINE	S_MONTHS_UM "2"
#DEFINE	S_DAYS_UM "3"
#DEFINE	S_HOURS_UM "4"
#DEFINE	S_MINUTES_UM "5"


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.Eventos_02

#DEFINE UNKNOWN_SUBSTANCE "9999"


#DEFINE SUBSTANCE_MIXTURE_CODES "0427 0457 0480 0491 0811 0812 1106"

#DEFINE QUIMICAL_SUBSTANCES_MIXTURE_CODE "1106"

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.frmLoadValidator

#DEFINE BASIC_DATA_TABLENAME "PACIENTE"


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.Brotes

#DEFINE UNKNOWN_SECTOR "9999999"

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.frmDatosBasicosIndividuales

#DEFINE UNKNOWN_LANE_SUFFIX "999"

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.Pte

#DEFINE DENGUE_CODES " 210 220 580 "

#DEFINE MAX_ALTITUDE_FOR_DENGUE 2200

#DEFINE MUNICIPALITY_CODES_WITHOUT_DENGUE ",11001,15001,"


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Exportar.prg
	Forms.Reporte

#DEFINE LABS_CONFIRMATION_RULE_VERSION 2015
*N�mero de versi�n a partir de la cual se implementa una regla de validaci�n que exige que los casos confirmados por laboratorio;
tengan un registro relacionado en Tables.Laboratorios

#DEFINE ORDINARY_CASE_WITOUT_CD 1
*Identificador de un caso de notificaci�n rutinaria que no tiene datos complementarios

#DEFINE ORDINARY_CASE_WITOUT_LABS 5
*Identificador de un caso de notificaci�n rutinaria que no tiene datos de laboratorio confirmatorios

#DEFINE INCOMPLETE_FOREIGN_CASE 6
*Identificador de un caso proveniente de un sistema externo que no ha sido completado en sus datos b�sicos o complementarios

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.Exportar

#DEFINE sNHSN_NNIS_EVENT_CODE "352"
* C�digo del evento al que aplica un informe especial basado en el �ndice de riesgo NHSN

#DEFINE sALERTS_EVENT_CODE "351"
* C�digo del evento al que aplica un informe especial que muestra alertas con base en resultados de laboratorio

#DEFINE sNHSN_NNIS_REPORT_FILENAME "rpt352_NHSN_NNIS.xlsx"
* Nombre del archivo (reporte) especial basado en el �ndice de riesgo NHSN

#DEFINE sALERTS_REPORT_FILENAME "rpt351_Semaforizacion.xlsx"
* Nombre del archivo (reporte) especial que muestra alertas con base en resultados de laboratorio

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.Laboratorios

#DEFINE OTHER_MICROORGANISM_CODE "998"
*C�digo de "otro microorganismo" en la Tables.Microorganismos


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Modules.PlainsLib

#DEFINE CD_TABLES_WITHOUT_NUM_IDE ",23,31,32,43,80,81,88,"
*Lista de tablas de datos complementarios que no tienen los campos TIP_IDE y NUM_IDE


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Modules.Exportar ;
	Modules.Exportar_Click

#DEFINE NOT_NOTIFIED '2'
*Identificador de un registro que no ha sido notificado en forma rutinaria

#DEFINE ORDINARY_NOTIFIED '0'
#DEFINE nORDINARY_NOTIFIED 0
*Identificador de un registro que ya ha sido notificado en forma rutinaria

#DEFINE INMEDIATE_NOTIFIED '1'
*Identificador de un registro que ha sido notificado en forma inmediata

#DEFINE ORDINARY_NOTIFICATION 1
*Identificador de una notificaci�n rutinaria

#DEFINE FEEDBACK_NOTIFICATION 2
*Identificador de una notificaci�n tipo retroalimentaci�n

#DEFINE INMEDIATE_NOTIFICATION 3
*Identificador de una notificaci�n inmediata

#DEFINE RE_NOTIFICATION 4
*Identificador de una operaci�n en donde se vuelven a notificar datos ya notificados

#DEFINE DBF_FEEDBACK_NOTIFICATION 5
*Identificador de una notificaci�n tipo retroalimentaci�n hacia archivos DBF

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.General

#DEFINE DAYS_BETWEEN_BACKUPS 1
*N�mero de d�as por default en los cuales el sistema debe disparar la producci�n de un backup de sus BD

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Modules.TransferDataHandler

*#DEFINE TRANSFORMATION_DONE '3'
#DEFINE TRANSFORMATION_DONE '5'
*N�mero que, aplicado a un registro, indica que ya ha sido objeto de una transformaci�n al momento de migrar datos o al ;
momento de cargarlos desde una archivo plano. El c�digo 3, entr� en desuso a partir de la versi�n 2018 1.8.7

#DEFINE TRANSFORMATION_IN_PROGESS '2'
*N�mero que, aplicado a un registro, indica que est� siendo objeto de una transformaci�n al momento de migrar datos o al ;
momento de cargarlos desde una archivo plano


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Modules.

#DEFINE DB_CLOSURE_DAY '30'
#DEFINE DB_CLOSURE_MONTH '03'


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Forms.Eventos_47

#DEFINE COD110_FIELD_INVESTIGATION_CLOSURE_DATE '29/03/2018'
*Fecha a partir de la cual entra en desuso la investigaci�n de campo para el evento Cod. 110


*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Modules.AdjustmentsProcessor

#DEFINE SIVIGILA_MARK 'SISSIV'
*Marca utilizada para los registros que en el sistema han sido modificados por el propio sistema Sivigila sin intervenci�n ;
de un usuario

#DEFINE UNDEFINED_SOCIAL_SECURITY 'I'
*Valor utilizado para los registros de datos b�sicos cuyos valores de "Tipo de regimen de salud" han sido asignados por una EAPB ;
como "Indeterminado/Pendiente"

*----------------------------------------------------------------------------------------------------------------------------
*/ Usado en: ;
	Modules.SivigilaUtilities

*#DEFINE DATA_VALIDATION_ERROR 1582
*#DEFINE RECORD_VALIDATION_ERROR 1583
#DEFINE PK_VALIDATION_ERROR 1884
