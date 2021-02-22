#INCLUDE SIVIGILA.H

PARAMETERS opc_exp

LOCAL oReporter as Object

SET PROCEDURE TO (PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger') ADDITIVE

*Comentado por: Wilson Aguilar Fecha: 30/11/2009
n_inmediata=''

*Corrije los posibles registros duplicados que se encuentren en la tabla UPGD con el fin de evitar ;
la generación de duplicados por cruce con dicha tabla
=showProgressMessage('Verificando consistencia de UPGDs')
SET PROCEDURE TO AnomaliesHandler ADDITIVE
=solveDuplicates('UPGD')
WAIT CLEAR
*SET STEP ON 
*Corrije los posibles registros duplicados que se encuentren en la tabla TAL_HUM con el fin de evitar ;
la generación de duplicados
*=solveDuplicates('TAL_HUM')
DO case
	case opc_exp = ORDINARY_NOTIFICATION
		*Procesa la notificación de información
		DO flujo WITH .F.
		DO checkDataToExport
		
		=showProgressMessage("Generando reporte de notificación negativa de eventos")
		oReporter=NEWOBJECT("SIVIGILAReporter","SIVIGILAReporter.PRG")
		oReporter.makeNegativeNotificationReport(ALLTRIM(STR(YEAR(DATE()))))
		RELEASE oReporter 		
		=hideProgressMessage()
		
		DO FORM frmSearchRec WITH 'rsNegativeNotificationReport','.T.',,.T.,'Notificación negativa para eventos'
		DO FORM reporte WITH opc_exp 
		
	case opc_exp = FEEDBACK_NOTIFICATION
   		*Procesa la retroalimentación de información
		DO FORM frmExportToFile WITH FEEDBACK_NOTIFICATION
		
	case opc_exp = INMEDIATE_NOTIFICATION
		*Procesa la notificación inmediata de información   
		n_inmediata=" and INMEDIATA='SI'"
		DO flujo WITH .T.
		DO checkDataToExport WITH .T.
		DO FORM reporte WITH opc_exp 
		
	CASE opc_exp = RE_NOTIFICATION
		*Vuelve a notificar información que ya se ha notificado con anterioridad
		DO FORM frmExportToFile WITH RE_NOTIFICATION

	CASE opc_exp = DBF_FEEDBACK_NOTIFICATION
   		*Procesa la retroalimentación de información hacia archihvos DBF
		DO FORM frmExportToFile WITH DBF_FEEDBACK_NOTIFICATION

ENDCASE


PROCEDURE flujo(bInmediateNotification AS Boolean)

	*bInmediateNotification:	Indica si la notificación que se va a llevar es inmediata(.T.) o rutinaria(.F.)
	
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE 
	
	LOCAL eventTableName as String
	
	=showProgressMessage('Preparando el Flujo en la Notificación de Eventos,' + CHR(13) + 'Por favor espere...')

	DO reg_flu WITH 'paciente', bInmediateNotification 
	DO reg_flu WITH 'brotes', bInmediateNotification
	DO reg_flu WITH 'laboratorios', bInmediateNotification

	USE BDSivigila!vTablasDeDC IN 0
	SELECT vTablasDeDC
	SCAN
		DO reg_flu WITH ALLTRIM(vTablasDeDC.DC_TABLA), bInmediateNotification
	ENDSCAN
	
	*Declara como registros de notificación inmediata todos aquellos que pertenezcan a tablas que, por naturaleza, deben ser;
	notificados en forma inmediata
	glDisableRules = .T.	
	UPDATE CONTACTOS SET CONTACTOS.INMEDIATA = 'SI', CONTACTOS.FLU_NOT=3  WHERE NOTIFICA=0
	UPDATE SEGUIMIENTOCONTACTOS SET SEGUIMIENTOCONTACTOS.INMEDIATA = 'SI',  SEGUIMIENTOCONTACTOS.FLU_NOT=3 WHERE NOTIFICA=0
	glDisableRules = .F.
	
	DO bases
	=showProgressMessage ('Flujo de eventos, Ok.')
ENDPROC


PROCEDURE reg_flu(sourceTableName, bInmediateNotification AS Boolean)

	*Actualiza los campos FLU_NOT (flujo de notificación) e INMEDIATA de los registros no notificados de la tabla sourceTableName;
	con el valor correspondiente de la tabla Eventos
	
	IF VARTYPE(glDisableRules)='L' THEN
		lOldDisableRules=glDisableRules
	ELSE
		lOldDisableRules=.F.
	ENDIF

	glDisableRules=.T.	
	sUpdateSQLCmd = "UPDATE " + sourceTableName + " SET " + sourceTableName + ".FLU_NOT = EVENTOS.FLU_NOT, " + sourceTableName + ;
					".INMEDIATA = EVENTOS.INMEDIATA FROM " + sourceTableName + " INNER JOIN EVENTOS ON " +;
					sourceTableName + ".COD_EVE = EVENTOS.COD_EVE WHERE " + sourceTableName + ".NOTIFICA=0"
	&sUpdateSQLCmd				
	glDisableRules=lOldDisableRules
ENDPROC


PROCEDURE checkDataToExport(bInmediateNotification AS Boolean)

	*bInmediateNotification:	Indica si la notificación que se va a efectuar es inmediata(.T.) o rutinaria(.F.)


	LOCAL oCurrentEvent as Object 

	SET PROCEDURE TO (PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger') ADDITIVE

	fic_sin_per=0


	*Cometario por: wilson Aguilar 	Fecha: 29/06/2010
	*Crea una tabla temporal destinada a almacenar campos clave (+Notifica, +Flu_not, +Inmediata, +EST_NOTIF) de las tablas ;
	de Eventos_XX
	x=_dir+'\xdat_com'
	CREATE TABLE (x) FREE (semana c(2), año c(4), cod_pre c(10), cod_sub c(2), cod_eve c(4), tip_ide c(2), num_ide  c(17), ;
		NOTIFICA N(1), flu_not n(1), inmediata c(2), EST_NOTIF N(1))

	*Cometario por: wilson Aguilar 	Fecha: 29/06/2010
	*Agrega a la tabla temporal xdat_com la información que se deba notificar de las tablas de datos complementarios
	sNotificationFilter = " NOTIFICA=0 "
	IF bInmediateNotification THEN
		sNotificationFilter = sNotificationFilter + " AND INMEDIATA='SI' AND EST_NOTIF=" + NOT_NOTIFIED
	ENDIF

	USE BDSivigila!vTablasDeDC IN 0
	SELECT vTablasDeDC
	SCAN
		=showProgressMessage (vTablasDeDC.DC_TABLA)
		SELECT xdat_com
		APPEND FROM (vTablasDeDC.DC_TABLA) FOR &sNotificationFilter 
	ENDSCAN

	*Cometario por: wilson Aguilar 	Fecha: 29/06/2010
	*Elimina la información que no deba fluir más allá del TipoDeUnidad (_tip_uni)
	SELECT xdat_com
	DELETE FOR xdat_com.flu_not < VAL(_tip_uni)
	COUNT TO tot_DC FOR !DELETED()
	
	=hideProgressMessage()

	*Agrega a la tabla temporal xdat_com la información de las tablas de contactos y seguimientos;
	La instrucción SQL construida para estos efectos, garantiza que se puedan notificar en forma inmediata y si;
	es necesario, solamente los seguimientos a contactos aún cuando también garantiza que se notifiquen contactos y seguimientos
	INSERT INTO xdat_com (cod_eve, tip_ide, num_ide, cod_pre, notifica, flu_not, inmediata, est_notif) ;
		SELECT COD_EVEPOS, TIP_IDE, NUM_IDE, 'CONT/SEGUI', S.NOTIFICA, S.FLU_NOT, S.INMEDIATA, S.EST_NOTIF FROM CONTACTOS C INNER JOIN ;
			(SELECT * FROM SEGUIMIENTOCONTACTOS WHERE &sNotificationFilter) S ON C.ID = S.ID_PERSONA 
	IF bInmediateNotification THEN
		*Garantiza que se pueda hacer la exportación de contactos y/o seguimientos aún cuando no existan notificaciones de eventos
		tot_DC = tot_DC + _TALLY
	ENDIF 
	
	DO permisos_eventos WITH SELECT()
	fic_sin_per=fic_sin_per+_tally


	GO top
	SELECT 1
	USE UPGD ORDER 1
	UPSE=_dir+'\xupgd'
	COPY STRUCTURE TO (UPSE)

	SELECT 300
	USE (UPSE) EXCLUSIVE
	INDEX ON cod_pre+cod_sub TAG PRE UNIQUE

	*Cometario por: wilson Aguilar 	Fecha: 29/06/2010
	*Crea y almacena en la tabla xpte, los datos que se deben notificar de la tabla PACIENTE
	xpt=_dir+'\xpte'
	&&SELECT *, 000 as lab, 0 as aplica_b, 0 as carab, from paciente WHERE &sNotificationFilter INTO TABLE (xpt)
	SELECT PACIENTE.*, 000 as lab, 0 as aplica_b, 0 as carab, E.LAB_INCRUS from paciente LEFT OUTER JOIN ;
		(SELECT EVENTOS.COD_EVE,EVENTOS.LAB_INCRUS FROM EVENTOS) AS E ON ;
		PACIENTE.COD_EVE = E.COD_EVE WHERE &sNotificationFilter INTO TABLE (xpt)
	tot_pte=_tally

	*Cometario por: wilson Aguilar 	Fecha: 29/06/2010
	*Elimina la información que no deba fluir más allá del TipoDeUnidad (_tip_uni)
	DELETE FOR xpte.flu_not<VAL(_tip_uni)
	area_p=SELECT()

	DO permisos_eventos WITH area_p
	fic_sin_per=fic_sin_per+_tally

	*Cometario por: wilson Aguilar 	Fecha: 29/06/2010
	*Por cada registro a notificar de XPTE, determina si debe exigirse los datos complementarios (actualiza xpte.Fields.aplica_b) ;
	y si hay información asociada de laboratorios (actualiza xpte.Fields.lab). Todo registro que tenga aplica_b = ORDINARY_CASE_WITOUT_CD se considera;
	que tiene datos complementarios faltantes o incompletos; todo registro que tenga aplica_b = ORDINARY_CASE_WITOUT_LABS se considera que le falta;
	un registro de laboratorios confirmatorio asociado; en caso contrario, idependiente del valor asumido por aplica_b, se;
	considera que el registro no tiene problemas
	oCurrentEvent = NEWOBJECT("Event","Event.Prg",.NULL.,XPTE.COD_EVE)
	SELECT XPTE
	GO TOP
	DO WHILE !EOF()
		sLinkedDataToLookFor = XPTE.AÑO + XPTE.SEMANA + XPTE.COD_EVE + XPTE.TIP_IDE + XPTE.NUM_IDE + XPTE.COD_PRE + XPTE.COD_SUB
		CON_AJ=IIF(AJUSTE='7',' AND NOTIFICA=0','')
		ajst=ajuste
		tipocaso = tip_cas
		fecha_notifica = Fec_Not
	   
		oCurrentEvent.Refresh(XPTE.COD_EVE)
		SELECT XPTE
	   
		*Cometario por: wilson Aguilar 	Fecha: 08/03/2010;
		La siguiente parte de código fue modificada debido al error con ID=177 cuyo origen radica en que el sistema versión 2010;
		exige la presencia de datos complementarios para todos los eventos que tienen formulario en la tabla de Eventos.;
		La modificación introducida permite sortear el hecho de que algunos eventos en la versión Sivigila 2008 ;
		no tenían cara B y, a partir de la versoón 2010 sí la tienen. A estos eventos, solo se les exigirá los datos complementarios;
		a partir de la fecha FEC_OBLIGA 
		&& IF !EMPTY(formulario) AND VAL(ajst)=0 AND cod_eve!='600 ' 

		*Cometario por: wilson Aguilar 	Fecha: 30/12/2012;
		La siguiente parte de código fue modificada debido a que a partir de la versión 2012 a todos los eventos que tienen ficha de captura para;
		datos complementarios se les exige tener esa información. Dicho en otras palabras, ya no se tiene en cuenta una fecha a partir de;
		cual deba exigirse esa información.  
		&&IF !EMPTY(formulario) AND VAL(ajst)=0 AND DATE()>=FEC_OBLIGA THEN
		
		*Cometario por: wilson Aguilar 	Fecha: 18/11/2012;
		Para la versión 2014 del Sistema se restableció la parte de código que tiene en cuenta la fecha a partir de la cual debe exigirse ;
		datos complementarios para un evento según su definición en la tabla Eventos. Ello, por cuanto el evento 330 no venía capturando datos ;
		complementarios y, a prtir de esa versión, empezó a capturarlos
		
		IF oCurrentEvent.bHasComplementaryData AND ((VAL(AJST)=0 AND AJST!='D'))  AND DATE()>=oCurrentEvent.dFEC_OBLIGA AND oCurrentEvent.nLATENCIA_DC=0 THEN
			*Es obligatorio que el registro de datos básicos tenga datos complementarios
			
			SELECT XPTE
			IF !bInmediateNotification AND ingr_x_ni THEN
				*Se trata de una notificación rutinaria y el registro ha sido explícitamente ingresado por el usuario como de ;
				notificación inmediata y aún no ha completado todas las variables pertinentes de datos básicos y/o datos complementarios;
				En consecuencia se declara que el registro tiene datos faltantes
				REPLACE aplica_b WITH ORDINARY_CASE_WITOUT_CD
			ELSE
				IF !bInmediateNotification AND EST_INGR = 2 THEN
					*Se trata de una notificación rutinaria y el registro ha sido cargado desde un sistema externo y aún ;
					no ha completado todas las variables pertinentes de datos básicos y/o datos complementarios;
					En consecuencia se declara que el registro tiene datos faltantes
					REPLACE aplica_b WITH INCOMPLETE_FOREIGN_CASE
				ELSE
					*Busca los datos complementarios del registro siempre y cuando el proceso De notificación actual no corresponda a ;
					uno de notificación imediata desencadenado por el usuario siguiendo la ruta ;
					Menú --> Procesos --> Recepción y transferencia de archivos planos --> Transferencia/Notificar INMEDIATA!
					IF !bInmediateNotification
						REPLACE aplica_b WITH ORDINARY_CASE_WITOUT_CD
					ENDIF

					DO SelectTable WITH oCurrentEvent.sDC_TABLA IN (PATH_TO_COMMON_LIB + 'TablesHandler')
					SEEK sLinkedDataToLookFor ORDER TAG LINKER_IDX
					IF FOUND() &CON_AJ
						SELECT XPTE
						IF !bInmediateNotification 
						     REPLACE aplica_b WITH aplica_b+1
						ENDIF
					ELSE
						SELECT XPTE
						IF bInmediateNotification AND oCurrentEvent.MandatoryComplementaryData2() THEN 
							REPLACE aplica_b WITH ORDINARY_CASE_WITOUT_CD
						ENDIF 
					ENDIF
				ENDIF
			ENDIF
		ELSE
			IF !oCurrentEvent.bHasComplementaryData AND ((VAL(AJST)=0 AND AJST!='D')) THEN
				IF !bInmediateNotification AND EST_INGR = 2 THEN
					*Se trata de una notificación rutinaria y el registro ha sido cargado desde un sistema externo y aún ;
					no ha completado todas las variables pertinentes de datos básicos; En consecuencia se declara que el ;
					registro tiene datos faltantes
					REPLACE aplica_b WITH INCOMPLETE_FOREIGN_CASE
				ENDIF
			ENDIF
		ENDIF
		
	   	SELECT XPTE
		*JLGB no tiene datos complementario y tiene plazo para notificar datos complementarios
	   	IF !bInmediateNotification AND aplica_b = ORDINARY_CASE_WITOUT_CD AND oCurrentEvent.nLATENCIA_DC > 0 THEN
	   		IF DATE() < fecha_notifica + (7 * oCurrentEvent.nLATENCIA_DC) THEN
	   			REPLACE aplica_b WITH aplica_b+1
	   		ENDIF
	   	ENDIF
	   	
		*Estable cuántos registros de laboratorio asociados tiene el caso identificado por sLinkedDataToLookFor (actualiza xpte.lab)
	   	nCaseLabsData=0
		SELECT LABORATORIOS
		SEEK sLinkedDataToLookFor ORDER TAG LINKER_IDX
		IF FOUND()
			DO WHILE !EOF() AND LABORATORIOS.AÑO + LABORATORIOS.SEMANA + LABORATORIOS.COD_EVE + LABORATORIOS.TIP_IDE + LABORATORIOS.NUM_IDE + ;
				LABORATORIOS.COD_PRE + LABORATORIOS.COD_SUB = sLinkedDataToLookFor
				nCaseLabsData = nCaseLabsData + 1
				SKIP
			ENDDO
			SELECT XPTE
			REPLACE lab WITH nCaseLabsData
		ENDIF
*SET STEP ON 	
		SELECT XPTE
		*JLGB, Exige datos de laboratorio para los registros que correspondan a casos confirmados por laboratorio 
		IF oCurrentEvent.CompulsoryLabs() AND !bInmediateNotification AND nCaseLabsData = 0 AND !XPTE.LAB_INCRUS AND ;
			VAL(SUBSTR(VERSION,12,4)) >= LABS_CONFIRMATION_RULE_VERSION AND ;
			(XPTE.AJUSTE= LAB_CONFIRMATION_ADJUSTMENT OR ((EMPTY(XPTE.AJUSTE) OR XPTE.AJUSTE='0') AND XPTE.TIP_CAS=LAB_CONFIRMATION_ADJUSTMENT)) THEN
			
			IF aplica_b != ORDINARY_CASE_WITOUT_CD THEN
				REPLACE aplica_b WITH ORDINARY_CASE_WITOUT_LABS
			ENDIF
		ENDIF
		
		
		SKIP
	ENDDO

	*Comentado por: Wilson Aguilar Fecha: 17/02/2010;
	La siguiente parte de código da lugar a que se generen registros duplicados en las tablas eventos_xx para ;
	los eventos concódigos 205, 458 y 650. Esta duplicidad impide la notificación de estos eventos;
	(ver error con IdError=94); por tanto, se decidió suprimirla
	&& x=_dir+'\genera_b'
	&& INDEX ON cod_eve TO (x) FOR aplica_b<>2
	&& GO top
	&& area_p=SELECT()
	&& DO WHILE !EOF()
	&&    crea_ficha_nva=0
	&&    DO case
	&&       CASE cod_eve='205 '
	&&          SCATTER memvar
	&&          SELECT eventos_27
	&&          crea_ficha_nva=1
	&&       CASE cod_eve='458 '
	&&          SCATTER memvar
	&&          SELECT eventos_26
	&&          crea_ficha_nva=1
	&&       CASE cod_eve='650 '
	&&          SCATTER memvar
	&&          SELECT eventos_24
	&&          crea_ficha_nva=1
	&&    ENDCASE 
	&&    IF crea_ficha_nva=1
	&&       INSERT INTO (ALIAS()) FROM MEMVAR
	&&       gnFieldcount = AFIELDS(gaMyArray)
	&&       FOR x = 8 TO gnFieldcount-3
	&&          IF gaMyArray(x,2)='C'
	&&             m='repl '+gaMyArray(x,1)+' with "2"'
	&&             &M
	&&          endif
	&&       ENDFOR
	&&    endif
	&&    SELECT (area_p)
	&&    skip
	&& ENDDO
	&& SET index to

	&& REPLACE aplica_b WITH 2 ALL FOR cod_eve='458 ' AND año='2007' 
	&& REPLACE aplica_b WITH 2 ALL FOR cod_eve='205 ' AND año='2007' 
	&& REPLACE aplica_b WITH 2 ALL FOR cod_eve='650 ' AND año='2007' 


	x=_dir+'\pr_su1'
	SELECT COD_PRE, COD_SUB FROM (xpt) GROUP BY COD_PRE, COD_SUB INTO TABLE (x)

	SELECT 300
	APPEND FROM (x)

	SELECT 53
	USE EVENTOS_23 ORDER 2
	xpt=_dir+'\xbro'
	SELECT *, 0 as aplica_b, 0 as carab from brotes WHERE &sNotificationFilter INTO TABLE (xpt)
	tot_bro=_tally
	area_p=SELECT()

	DELETE FOR xbro.flu_not<VAL(_tip_uni)

	DO permisos_eventos WITH area_p
	fic_sin_per=fic_sin_per+_tally

	GO top
	DO WHILE !EOF()
	   sLinkedDataToLookFor = XBRO.COD_EVE + XBRO.AÑO + ALLTRIM(XBRO.SEMANA) + ALLTRIM(XBRO.COD_MUN) + ALLTRIM(XBRO.NUM_CON) + ALLTRIM(XBRO.COD_PRE) + ALLTRIM(XBRO.COD_SUB)
	   IF COD_EVE='350 ' AND _tip_uni!='1'
	      REPLACE aplica_b WITH ORDINARY_CASE_WITOUT_CD
	      SELECT EVENTOS_23 
	      SEEK sLinkedDataToLookFor 
	      IF FOUND()
	         SELECT (area_p)
	         REPLACE aplica_b WITH aplica_b+1
	      endif
	   endif
	   SELECT (area_p)
	   skip
	enddo

	x=_dir+'\pr_su'
	SELECT COD_PRE, COD_SUB FROM (xpt) GROUP BY COD_PRE, COD_SUB INTO TABLE (x)

	SELECT 300
	APPEND FROM (x)
	SCAN
	   cp=cod_pre+cod_sub
	   SELECT 1
	   SEEK cp
	   IF found()
	      SCATTER MEMVAR
	      SELECT 300
	      GATHER MEMVAR
	   endif
	   SELECT 300
	ENDSCAN
	COUNT TO tot_upgd &&=RECCOUNT()
	GO top

	x=_dir+'\xth'
	SELECT * from tal_hum WHERE notifica=7 INTO TABLE (X)
	area_TH=SELECT()
	SELECT 300
	SCAN
	   cp=cod_pre+cod_sub
	   SELECT 2
	   SEEK cp
	   DO WHILE !EOF() AND  cod_pre+cod_sub=cp
	      SCATTER memvar
	      SELECT (area_th)
	      INSERT INTO (ALIAS()) FROM MEMVAR
	      SELECT 2
	      SKIP
	   ENDDO
	   SELECT 300
	ENDSCAN
	GO top
	SELECT (area_th)
	GO top
	tot_th=RECCOUNT()

	x=_dir+'\xlab'
	SELECT * from laboratorios WHERE &sNotificationFilter INTO TABLE (X)
	tot_lab=_tally

	DELETE FOR xlab.flu_not<VAL(_tip_uni)

	DO permisos_eventos WITH SELECT()
	fic_sin_per=fic_sin_per+_tally

	*Comentado por:	Wilson Aguilar	Fecha: 24/07/2012;
	Por solicitud del Grupo SIVIGILA del INS se decidió eliminar esta parte del código
	&&IF fic_sin_per>0
	&&   t_i('EXISTEN '+NC(fic_sin_per)+' FICHAS QUE NO SE PUEDEN EXPORTAR'+CHR(13)+;
	&&   '(Entre: Datos Básicos, Complementarios, Colectivos o Laboratorios)'+CHR(13)+CHR(13)+;
	&&   'DEBIDO A LAS RESTRICCIONES ASIGNADAS POR UN ADMINISTRADOR DEL SISTEMA')
	&&ENDIF

	RELEASE oCurrentEvent
ENDPROC

