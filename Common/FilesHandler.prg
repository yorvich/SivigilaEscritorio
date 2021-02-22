#include DevEnvironment.h

PROCEDURE copyFiles

	LPARAMETERS sTargetPath AS String, sFilesToCopyTableName as String
	
	*Copia los archivos especificados en sFilesToCopyTableName hacia la carpeta sTargetPath.
	
	* sFilesToCopyTableName debe ser una tabla con la menos los siguientes campos: ;
		- FileName (y path, si es necesario);
		- Overwrite ;
		- Active ;
		- Copiable;
	Los archivos que se copiarán hacia la carpeta destino serán aquellos que cumplan con ;
	ACTIVE=1 OR COPIABLE=1; por otra parte, si Overwrite=0, el archivo se copiará siempre y;
	cuando NO exista en la carpeta destino; en caso contrario, el archivo se sobreescribirá ;
	en el correspondiente en la carpeta destino
	
	
	LOCAL sTargetFile as String, sOldSetSafety as String
	
	sSetCmd = "SET PROCEDURE TO '" + PATH_TO_COMMON_LIB + "TablesHandler' ADDITIVE"
	&sSetCmd
	
	IF useTable(sFilesToCopyTableName ) THEN
		sOldSetSafety = SET("Safety")
		SET SAFETY OFF 
		sFilesToCopyTableName = JUSTFNAME(sFilesToCopyTableName)
		SELECT (sFilesToCopyTableName)
		SCAN FOR ACTIVE=1 OR COPIABLE=1
			sTargetFile = "'" + ADDBS(sTargetPath) + JUSTFNAME(&sFilesToCopyTableName..FileName) + "'"
			sCopyCmd = "COPY FILE '" + &sFilesToCopyTableName..FileName + "' TO " + sTargetFile 
			IF Overwrite=0 THEN 
				*Se actualiza el objeto siempre y cuando no exista en el sistema destino
				IF !FILE("&sTargetFile") THEN
					&sCopyCmd
				ENDIF
			ELSE
				&sCopyCmd
			ENDIF			
		ENDSCAN
		SET SAFETY &sOldSetSafety 
	ENDIF
	
ENDPROC

PROCEDURE MKDirs(sDirList as String)

	*Crea cada uno de los directorios especificados en la lista separada por comas sDirList.  
	
	LOCAL i as Byte, nDirsToMake as Number
	
	nDirsToMake = ALINES(aDirsToMake,sDirList ,1,",")
	FOR i=1 TO nDirsToMake
		IF !DIRECTORY(aDirsToMake(i)) THEN
			TRY
				MKDIR (aDirsToMake(i))
			CATCH TO oException
			ENDTRY
		ENDIF
	ENDFOR
	
ENDPROC

