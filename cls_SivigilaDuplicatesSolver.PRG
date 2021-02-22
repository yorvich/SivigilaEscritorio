#INCLUDE DevEnvironment.h

DEFINE CLASS SivigilaDuplicatesSolver as AnomaliesSolver OF PATH_TO_DEVELOPMENT_ENVIRONMENT + 'AnomaliesSolver'

FUNCTION setDefaultKey  as String
	
	*Retorna en un string separado por comas los campos de la clave por defecto establecida ;
	para la tabla de datos básicos del Sistema Sivigila según se encuentra definida en la tabla SivigilaPKs
	
	SET PROCEDURE TO SivigilaUtilities ADDITIVE 
	
	This.sDefaultKey = CHRTRAN(getKeyFields(JUSTSTEM('PACIENTE')), '+', ',')
ENDFUNC

ENDDEFINE
