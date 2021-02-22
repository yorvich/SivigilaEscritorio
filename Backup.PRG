#INCLUDE SIVIGILA.H

PROCEDURE makeSivigilaBackup()
	*Efectua un backup de todos los archivos DBF y CDX que se encuentren en el directorio .\SivigilaData y de los archivos de ;
	las bases de datos BDSIVIGILA y anomaliesDB. El backup quedará almacenado en un archivo ZIP con nombre ;
	dd_mm_yyyy_BackupSivigila.ZIP en la carpeta determinada por el PROCEDURE makeBackupDir

	STORE '' TO sErrorMessage

	STORE ON('ERROR') TO sOldOnError
	
	LOCAL sResultMsg as String
	LOCAL sBackupPath as String 

	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
	
	ON ERROR DO (PATH_TO_COMMON_LIB + '\SimpleErrorHandler') WITH MESSAGE(),MESSAGE(1),PROGRAM(),LINENO(),.T.
	
	DO makeBackupDir WITH sBackupPath, APP_NAME, sErrorMessage  
	sBackupPath = ADDBS(sBackupPath)

	IF !EMPTY(sErrorMessage) THEN 
		=showErrorMessage(sErrorMessage, 2)
	ELSE 
		sBackupFileName = CHRTRAN(dtoc(DATE()),'/','_') + '_BackupSivigila'
		sErrorMessage = ''
		DO ArchiveFiles WITH '.\SivigilaData', '*.*', sBackupPath, sBackupFileName, sErrorMessage
	
		IF EMPTY(sErrorMessage) THEN
			* Agregar al archivo de backup, los archivos de la base de datos CustomizationDB
			DO ArchiveFiles WITH '.\', , sBackupPath , sBackupFileName, sErrorMessage, "CustomizationDataFiles.txt"

			IF EMPTY(sErrorMessage) THEN
				* Agrega al archivo de backup, los archivos de las bases de datos anomaliesDB
				DO ArchiveFiles WITH '.\', , sBackupPath , sBackupFileName, sErrorMessage, "AnomaliesDataFiles.txt"
				IF EMPTY(sErrorMessage) THEN
					sSQLCmd = 'UPDATE "' + sBackupPath + 'backup.dbf' + '" SET FECHA = DATE()'
					&sSQLCmd 
					USE IN 'Backup'

					=showErrorMessage('Copia de respaldo finalizada!'+CHR(13)+CHR(13)+;
										'Se ha generado en la carpeta:'+CHR(13)+sBackupPath, 0 )
				ENDIF
			ENDIF 
		ENDIF

		IF !EMPTY(sErrorMessage) THEN
			sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a este proceso: " +  CHR(13) +  CHR(10) +;
						"Mensaje del problema : "  + (sErrorMessage) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
						"No se puede efectuar la operación."
			=showErrorMessage(sResultMsg, 2)
		ENDIF
	ENDIF 
	
	ON ERROR &sOldOnError
ENDPROC 



PROCEDURE makeBackupDir(sBackupPath as String, sSivigilaAppName as String, sErrorMsg as String)
	
	*Crea una carpeta hacia donde debería efectuarse un backup del sistema sivigila identificado por sSivigilaAppName. ;
	sBackupPath contendrá la ruta completa de la carpeta -incluyendo el drive- y sErrorMsg contendrá un mensaje de error, ;
	si es que se encuentra alguno durante el proceso de creación de la carpeta.;
	Si se ha podido crear la carpeta, verifica que en ella exista una tabla Backup.dbf; si no es así, la crea con un campo;
	Fecha D(10) y  un registro vacío.
	
	LOCAL sOldProc as string
	sOldProc = SET("Procedure")
	
	LOCAL oNotifierUnit as Object 
	oNotifierUnit = NEWOBJECT("NotifierUnit","NotifierUnit.fxp",.NULL.,.F.)
	
	LOCAL bContinue as Boolean 
	bContinue = .T.
	
	LOCAL sBackupDrive  as String, sBackupDir as String
	
	SET PROCEDURE TO Utilities
	sMyDocumentsDir = ADDBS(getMyDocumentsDir()) + sSivigilaAppName 
	
	sErrorMsg = ''
	
	*establece cuál es el nombre de la carpeta por default -sin incluir el drive-  destinada para hacer backups
	sBackupDir = ALLTRIM(SUBSTR(sMyDocumentsDir,3,LEN(sMyDocumentsDir))) + '\COPIAS DE SEGURIDAD'

	
	*Verifica que la configuración de backup sea consistente con la máquina cliente
	sBackupDrive = 'C:'
	IF oNotifierUnit.sBackupDrive != 'C' THEN 
		IF DIRECTORY(ADDBS(oNotifierUnit.sBackupDrive + ':')) THEN   
			sBackupDrive = oNotifierUnit.sBackupDrive  + ':'
		ELSE
			sErrorMsg = ERR_BACKUP_DRIVE_NOT_FOUND
			bContinue = .F.
		ENDIF
	ENDIF

	IF bContinue THEN 
		sBackupPath = sBackupDrive + sBackupDir
		IF !DIRECTORY(sBackupPath)
			MKDIR (sBackupPath)
		ENDIF

		IF !FILE(sBackupPath+'\backup.dbf')
			sOldDir = SET("Directory")
			SET DIRECTORY TO "&sBackupPath" 
			CREATE TABLE backup FREE (fecha d(10)) 
			APPEND blank 
			SET DIRECTORY TO "&sOldDir" 
		ENDIF 
	ENDIF 
		
	RELEASE oNotifierUnit 
	SET PROCEDURE TO &sOldProc
ENDPROC



PROCEDURE makeBackupDirTst()
	LOCAL sBackupPath as String
	LOCAL sErrMsg as String
	
	SET  STEP ON 
	SET PROCEDURE TO 
	
	sErrMsg = ''
	DO makeBackupDir WITH sBackupPath, 'SIVIGILA - 2018 - 1.6.32', sErrMsg
	IF EMPTY(sErrMsg) THEN 
		?sBackupPath
	ELSE
		?sErrMsg
	ENDIF
	
ENDPROC 


PROCEDURE ArchiveFiles(sSourcePath, sSourceFileMask, sTargetPath, sTargetZipFileName, sErrorMsg as String, ;
						sUseListFile as string, sWithPwd as Boolean)

	* sSourcePath: ruta en donde se encuentran los archivos sSourceFileMask a empaquetar
	* sSourceFileMask: máscara de los archivos que deberán incluirse en el ARCHIVO ZIP
	* sTargetPath: ruta en donde debe guardarse el archivo Zip resultante
	* sTargetZipFileName: nombre y extensión del archivo ZIP que resultará del procesamiento
	* sErrorMsg : Si ocurre algún error, contiene el mensaje de error correspondiente; en otro caso, es vacío


	sErrorMsg = ''
*SET STEP ON 
	IF VARTYPE(sUseListFile)='C' AND !EMPTY(sUseListFile) THEN 
		IF VARTYPE(sWithPwd) = 'C' AND !EMPTY(sWithPwd) THEN 
			sPackCmd = '7zG u "' + ADDBS(sTargetPath) + JUSTSTEM(sTargetZipFileName)  + '.zip" -ssw -p' + sWithPwd + ' @' + sUseListFile 
		ELSE
			sPackCmd = '7zG u "' + ADDBS(sTargetPath) + JUSTSTEM(sTargetZipFileName)  + '.zip" -ssw "' + '@' + sUseListFile 
		ENDIF 
	ELSE
		IF VARTYPE(sWithPwd) = 'C' AND !EMPTY(sWithPwd) THEN 
			sPackCmd = '7zG u "' + ADDBS(sTargetPath) + JUSTSTEM(sTargetZipFileName)  + '.zip" -ssw -p' + sWithPwd + ' "' + ADDBS(sSourcePath) + sSourceFileMask + '"'
		ELSE
			sPackCmd = '7zG u "' + ADDBS(sTargetPath) + JUSTSTEM(sTargetZipFileName)  + '.zip" -ssw "' + ADDBS(sSourcePath) + sSourceFileMask + '"'
		ENDIF 
		*sPackCmd = 'RUN 7zG u "' + ADDBS(sTargetPath) + sTargetZipFileName  + '" -ssw "' + ADDBS(sSourcePath) + sSourceFileMask + '"'
	ENDIF 
	TRY 
		*&sPackCmd
		DO runBatFile WITH sPackCmd IN Utilities
	CATCH TO oException
		sErrorMsg = oException.Message
	ENDTRY 
	 
ENDPROC


PROCEDURE ArchiveFilesTst
	sErrMsg = ''
SET STEP ON 	
	*sSourcePath = "C:\Users\wilson.aguilar\Documents\wilsonaguilar\ProyectoSivigila\Desarrollo_2012_V1.1.0"
	sSourcePath = ".\SivigilaData"
	*sSourcePath = "C:\Users\wilson.aguilar\Documents\SIVIGILA 2012\ARCHIVOS XLS"	
	*sSourcePath = "c:\temp"
	*sSourcePath = "C:\Users\wilson.aguilar\Documents\SIVIGILA - 2015\ARCHIVOS PLANOS\PLANOS_SEMANA_RA_2015"
	
	*sTargetPath = "c:\Users\wilson.aguilar\Documents\SIVIGILA 2012\COPIAS DE SEGURIDAD\Sabado"
	*sTargetPath = "C:\Users\Sivigila2010\Documents\SIVIGILA 2010\COPIAS DE SEGURIDAD"
	*sTargetPath = "C:\Users\wilson.aguilar\Documents\SIVIGILA 2012\ARCHIVOS XLS"	
	*sTargetPath = "C:\Users\wilson.aguilar\Documents\SIVIGILA 2012\ARCHIVOS PLANOS"
	sTargetPath = "d:\tmp"
	
	*DO ZIP WITH sSourcePath, '*.DBF', sTargetPath, '28_07_2012_BackupSivigila.zip', sErrMsg
	*DO ZIP WITH sSourcePath, '*.CDX', sTargetPath, '28_07_2012_BackupSivigila.zip', sErrMsg, .t.
	*DO ZIP WITH sSourcePath, '*.TXT', sTargetPath, 'Txt.zip', sErrMsg, .T. 
	DO ArchiveFiles WITH sSourcePath, '*.*', sTargetPath, 'TstArchive', sErrMsg, .T. 
ENDPROC
