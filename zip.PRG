PARAMETERS sSourcePath, sSourceFileMask, sTargetPath, sTargetZipFileName, sErrorMsg as String, AppendTo as Boolean

* sSourcePath: ruta en donde se encuentran los archivos sSourceFileMask a empaquetar
* sSourceFileMask: máscara de los archivos que deberán incluirse en el ARCHIVO_ZIP
* sTargetPath: ruta en donde debe guardarse el archivo Zip resultante
* sTargetZipFileName: nombre y extensión del archivo ZIP que resultará del procesamiento
* sErrorMsg : Si ocurre algún error, contiene el mensaje de error correspondiente; en otro caso, es vacío


sErrorMsg = ''
sCurrentDrive = SYS(5)
sCurrentDir = SYS(2003)
*SET STEP ON 
nSourceFiles = ADIR(aSourceFiles, ADDBS(sSourcePath) + sSourceFileMask)
IF nSourceFiles >0 THEN
	SET LIBRARY TO "vfpcompression.fll" ADDITIVE
	SET DEFAULT TO '&sTargetPath'
	IF AppendTo THEN
		=ZipOpen(sTargetZipFileName,'',.T.)
	ELSE
		=ZipOpen(sTargetZipFileName)
	ENDIF
	
	FOR iTargetFile=1 TO nSourceFiles 
		WAIT WIND NOWAIT 'Generando copia de Seguridad ' + sTargetZipFileName + ', Por favor espere...'+CHR(13)+'Comprimiendo el Archivo: '+aSourceFiles[iTargetFile,1]
		TRY
			=ZipFile(ADDBS(sSourcePath) + aSourceFiles[iTargetFile,1], .T.)
		CATCH TO oException
			sErrorMsg = oException.Message
		ENDTRY
	ENDFOR
	=ZipClose()
	sSetDefaultCmd = "SET DEFAULT TO '" + sCurrentDrive + sCurrentDir + "'"
	&sSetDefaultCmd
	WAIT CLEAR
ENDIF
SET LIBRARY TO
RETURN


PROCEDURE zipTst
	sErrMsg = ''
*SET STEP ON 	
	*sSourcePath = "C:\Users\wilson.aguilar\Documents\wilsonaguilar\ProyectoSivigila\Desarrollo_2012_V1.1.0"
	*sSourcePath = "C:\ins\sivigila2012"
	*sSourcePath = "C:\Users\wilson.aguilar\Documents\SIVIGILA 2012\ARCHIVOS XLS"	
	*sSourcePath = "c:\temp"
	sSourcePath = "C:\Users\wilson.aguilar\Documents\SIVIGILA - 2015\ARCHIVOS PLANOS\PLANOS_SEMANA_RA_2015"
	
	*sTargetPath = "c:\Users\wilson.aguilar\Documents\SIVIGILA 2012\COPIAS DE SEGURIDAD\Sabado"
	*sTargetPath = "C:\Users\Sivigila2010\Documents\SIVIGILA 2010\COPIAS DE SEGURIDAD"
	*sTargetPath = "C:\Users\wilson.aguilar\Documents\SIVIGILA 2012\ARCHIVOS XLS"	
	*sTargetPath = "C:\Users\wilson.aguilar\Documents\SIVIGILA 2012\ARCHIVOS PLANOS"
	sTargetPath = "c:\temp"
	
	*DO ZIP WITH sSourcePath, '*.DBF', sTargetPath, '28_07_2012_BackupSivigila.zip', sErrMsg
	*DO ZIP WITH sSourcePath, '*.CDX', sTargetPath, '28_07_2012_BackupSivigila.zip', sErrMsg, .t.
	DO ZIP WITH sSourcePath, '*.TXT', sTargetPath, 'Txt.zip', sErrMsg, .T. 
ENDPROC
