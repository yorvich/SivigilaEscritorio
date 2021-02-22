PARAMETERS isNewRec as Boolean, oEvent as Object, sExportPath as String
*Almacena un registro nuevo de datos complementarios o actualiza uno existente en función de isNewRec 

*isNewRec (in): condición lógica que al evaluarse en .T., indica que se debe agregar un registro nuevo a la tabla de datos complementarios;
				 y si es .F. que se debe actualizar un regitro existente
*oEvent (in): objeto de la clase Event que es utilizado para establecer si luego de Guardar un registro nuevo, se debe enviar o no un mail;
				a la cuenta destinada para tales efectos según se encuentre definida en la tabla SendMailConfig.MsgTipo(=48). Se enviará;
				un email si el evento en oEvent está configurado como de notificación super-inmediata.

#include SIVIGILA.h

LOCAL sCDTableName as String
*Nombre de la tabla de datos complementarios en donde se agregará o actualizará el registro

LOCAL bSendMail as Boolean
*Indica si debe enviarse un email de notificación super-inmediata del caso a una cuenta de correo establecida para tales efectos
bSendMail = .T.

nOldSelect = SELECT()
IF !USED('EVENTOS') THEN 
	SELECT 7
	USE eventos ORDER 1
ELSE
	SELECT EVENTOS
	SET ORDER TO 1
ENDIF 
IF !SEEK(m.cod_eve)
   t_i('No existe el evento: '+m.cod_eve)
   return
ENDIF
m.flu_not=flu_not
SELECT (nOldSelect)

IF &isNewRec
	*Libera la variable que da cuenta del estado de notificación del registro para que el valor del campo correspondiente sea asignado por Default
	RELEASE m.EST_NOTIF
	
	INSERT INTO (ALIAS()) FROM MEMVAR 
	IF goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR
		RETURN 0
	ENDIF

	*Garantiza que el registro sea declarado como no notificado y que la fecha de grabación o ajuste sea la actual   
	REPLACE NOTIFICA WITH 0
	REPLACE FEC_AJU WITH DATE()

	*Si el evento es de notificación super-inmediata envía correo electrónico de notificación al INS
	IF VARTYPE(oEvent)='O' THEN
		IF oEvent.bSUPER_FAST 
			IF !EMPTY(oEvent.sCONDICION_SUPER_FAST) THEN
				bSendMail = (EVALUATE(oEvent.sCONDICION_SUPER_FAST))
			ENDIF			
			IF bSendMail THEN
				sExportationPath = ADDBS(sExportPath)
				
				*Genera un archivo texto con un informe de notificación legible en cualquier dispositivo
				sExportedFileName = 'Notificacion_inmediata' + "_" + oEvent.sCOD_EVE + STRTRAN(TTOC(DATETIME(),3),":","_")
				DO printDBPlusDCToFile WITH sExportationPath  + sExportedFileName 
				
				*Envía el email adjuntando tanto el reporte como el archivo excel
				IF !_SCREEN.bTestMode THEN
					sAttachements = sExportationPath + sExportedFileName + '.TXT'
					*26/06/2018 : JLGB EXPORTAR NUEVO ARCHIVO DE XLS
					sAttachements = sAttachements + ';' + sExportationPath + sExportedFileName + '.XLS'
					
					*Construye el subject del email basándose en los datos del caso registrado que printDBPlusDCToFile ha construido(rsPaciente y rsDC)
					sSubject = ' COD_' + oEvent.sCOD_EVE + ' ' + 'COD_ASE_' + IIF(EMPTY(rsPaciente.COD_ASE),'NR',rsPaciente.COD_ASE) + ;
								' ' + 'COD_DPTO_NOTIFICADOR_' + LEFT(rsPaciente.COD_PRE,2) +;
								' ' + 'COD_DPTO_RESIDENCIA_' + LEFT(rsPaciente.MUN_PRO,2) + ' ' + 'COD_DPTO_PROCEDENCIA_' + LEFT(rsPaciente.COD_MUN,2) +;
								' ' + 'COD_MUN_PROCEDENCIA_' + rsPaciente.COD_MUN 
					sBody = "Sender:" + CrLf +_SCREEN.oNotifierUnit.sIPAdresses +;
							+ ' PC:' + GETENV("COMPUTERNAME") + ' LOGONSERVER:' + GETENV("LOGONSERVER") + ' DOM:' + GETENV("USERDOMAIN") +;
								' USER:' + GETENV("USERNAME")
					sSendMailCmd = "DO SendMailManual WITH 48, sSubject , sBody, sAttachements IN " + PATH_TO_SIVIGILAMailer + "sendmail.prg"
					&sSendMailCmd
				ENDIF
			ENDIF
		ENDIF
	ENDIF
	SELECT (nOldSelect)
	tit('SE HA ALMACENADO UNA NUEVA FICHA')
ELSE
	*Busca el registro más reciente de datos complementarios para actualizarlo, si es que existe
	SET PROCEDURE TO EventsHandler ADDITIVE
	sCDTableName = GetWarehouseTableName(m.COD_EVE)
	SET PROCEDURE TO CasesHandler ADDITIVE
	nRecIdToUpdate = lastCaseRecord(sCDTableName,"AÑO + SEMANA + COD_EVE + TIP_IDE + NUM_IDE + COD_PRE + COD_SUB ='" +;
					+ m.año + m.semana + m.cod_eve + m.tip_ide + + m.num_ide + m.cod_pre + m.cod_sub + "'")
	SELECT (sCDTableName)
	IF nRecIdToUpdate > 0 THEN
		GATHER MEMVAR 
		IF goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR
			RETURN 0
		ENDIF
		REPLACE NOTIFICA WITH 0
		REPLACE FEC_AJU WITH DATE()
		tit('LA FICHA SE HA ACTUALIZADO')
	ENDIF
ENDIF


REPLACE AJUSTE WITH AJU_TMP, SEMANA WITH _SEMANA, AÑO WITH _AÑO, COD_PRE WITH _COD_PRE, COD_SUB WITH _COD_SUB, ;
		TIP_IDE WITH _TIP_IDE, NUM_IDE WITH _NUM_IDE, COD_EVE WITH _COD_EVE
IF goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR
	RETURN 0
ENDIF

FLUSH
