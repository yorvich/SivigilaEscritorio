*version: 2010
*Modificaciones: (?) Actualizar al número total de Eventos _MaxEvents
DO llave WITH 'REI_BD '
IF !FOUND() OR acceso!='S'
   =t_i(_ucs)
   RETURN
ENDIF
T_SN('IMPORTANTE, Si acepta este proceso, '+CHR(13)+'SE ELIMINARAN TODOS LOS REGISTROS DIGITADOS O IMPORTADOS'+chr(13)+'EN LAS BASE DE DATOS DEL SIVIGILA')
if _rta=7
   return
ENDIF

CLOSE ALL

*Intenta abrir la tabla PACIENTE sSourceTableName en modo exclusivo para verificar que no existan otros usuarios en red;
accesando el Sistema
vExclusiveState=SET("Exclusive")

SET EXCLUSIVE ON

sSourceTableName='PACIENTE'
bSourceTableIsOpenByAnotherUser = .F.
TRY
	USE (sSourceTableName) IN 0
	SELECT (sSourceTableName)	
CATCH TO oException
	*sSourceTableName no se pudo abrir en modo exclusivo y por tanto se restablecen las condiciones ;
	de datos previas al intento. El error que se produce es el 1705 - File access is denied.
	bSourceTableIsOpenByAnotherUser = .T.
	SET EXCLUSIVE &vExclusiveState
ENDTRY

SET PROCEDURE TO SIVIGILAMessenger ADDITIVE

IF bSourceTableIsOpenByAnotherUser THEN
	sErrorMsg='IMPOSIBLE ELIMINAR DATOS'+CHR(13)+'Otros usuarios tienen abiertas una o más de las tablas de datos del Sistema'
ELSE
	
	USE UPGD
	ZAP
	
	USE UPGD_UCIS
	ZAP

	USE TAL_HUM
	ZAP

	use laboratorios
	ZAP

	use brotes
	ZAP

	USE paciente
	zap

	use NOTIFICADOS
	zap

	USE AJUSTES
	ZAP

	use CONTACTOS
	ZAP

	use SEGUIMIENTOCONTACTOS
	ZAP

	USE tbCasosAnomalosDetectados
	ZAP
	
	OPEN DATABASE BDSivigila
	USE BDSivigila!vTablasDeDC IN 0
	SELECT vTablasDeDC
	SCAN
	    sEventTablename = ALLTRIM(vTablasDeDC.DC_TABLA)
	    USE (sEventTablename) IN 0
	    SELECT (sEventTablename)
	    ZAP
	    SELECT vTablasDeDC
	ENDSCAN
	

	sErrorMsg='BASES DE DATOS REINICIALIZADAS CON CERO REGISTROS!'
ENDIF

=showErrorMessage(sErrorMsg,0)
SET EXCLUSIVE &vExclusiveState

DO Bases

