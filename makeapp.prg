#INCLUDE SIVIGILA.H

LPARAMETERS pathToApp, nVersion, sSIANIESPProductionPackId as string

IF VARTYPE(sSIANIESPProductionPackId)='C' AND !EMPTY(sSIANIESPProductionPackId) THEN
	SET PATH TO ".\SivigilaData" additive 
	
	OPEN DATABASE BDSIVIGILA
	PACK DATABASE
	CLOSE ALL
	
	OPEN DATABASE SIVIGILATemp
	PACK DATABASE
	CLOSE ALL

	COMPILE CLASSLIB *.vcx
	
	COMPILE FORM *.scx
	
	SET SAFETY OFF

	IF !DIRECTORY([&pathToApp\Preproduccion]) THEN
		MKDIR &pathToApp\Preproduccion
	ENDIF
	DELETE FILE [&pathToApp\Preproduccion\*.*]
	DELETE FILE [&pathToApp\Preproduccion\Reportes\Tmp\*.*]

	*Limpia las tablas de datos del Sistema
	DO Batch_Clean WITH .T.

	*Limpia las tablas de datos de varios componentes del Sistema
	SET SAFETY OFF
	USE DatosHomologados
	ZAP
	USE

	USE ErrorLog
	ZAP
	USE

	USE DirListing
	ZAP
	USE

	USE ASEGURADORASPWDs
	ZAP
	USE

	USE SendMailTransfer
	ZAP
	USE

	USE UpdatesTracking
	ZAP
	USE

	USE tbCasosAnomalosDetectados
	ZAP
	USE

	USE SivigilaDownloaderLog
	ZAP
	USE

	USE Plano
	ZAP
	USE

	CLOSE ALL

	*Limpia objetos temporales
	RUN DEL /Q .\REPORTES\TMP\*.*
		
	*SET STEP ON 
	*Copia hacia el ambiente de pre-producción la BD del Sistema - BDSivigila
	IF !DIRECTORY([&pathToApp\Preproduccion\SivigilaData]) THEN
		MKDIR &pathToApp\Preproduccion\SivigilaData
	ENDIF
	DELETE FILE [&pathToApp\Preproduccion\SivigilaData\*.*]
	RUN rar.exe u CurrentBDSivigila @SivigilaDataFiles.txt
	sTranserDBToPreProduction="RUN rar.exe e -ep CurrentBDSivigila " + pathToApp + "\Preproduccion\SivigilaData"
	&sTranserDBToPreProduction
	DELETE FILE CurrentBDSivigila.rar

	*Copia hacia el ambiente de pre-producción la BD de Anomalias del Sistema
	RUN rar.exe u CurrentAnomaliesBDSivigila @AnomaliesDataFiles.txt
	sTranserDBToPreProduction="RUN rar.exe e -ep CurrentAnomaliesBDSivigila " + pathToApp + "\Preproduccion"
	&sTranserDBToPreProduction
	DELETE FILE CurrentAnomaliesBDSivigila.rar
	sCopyCmd = 'COPY FILE AnomaliesDataFiles.txt TO ' + (pathToApp) + '\Preproduccion\AnomaliesDataFiles.txt'
	&sCopyCmd

	*Copia hacia el ambiente de pre-producción la BD de configuración del Sistema
	RUN rar.exe u CurrentCustomizationDB @CustomizationDataFiles.txt
	sTranserDBToPreProduction="RUN rar.exe e -ep CurrentCustomizationDB " + pathToApp + "\Preproduccion"
	&sTranserDBToPreProduction
	DELETE FILE CurrentCustomizationDB.rar
	sCopyCmd = 'COPY FILE CustomizationDataFiles.txt TO ' + (pathToApp) + '\Preproduccion\CustomizationDataFiles.txt'
	&sCopyCmd

	*Copia hacia el ambiente de pre-producción la BD de concordancia con datos externos
	DELETE FILE [&pathToApp\Preproduccion\AgreementData\*.*]
	RUN rar.exe u -ep AgreementDB @AgreementDataFiles.txt
	sTranserDBToPreProduction="RUN rar.exe e -ep AgreementDB " + pathToApp + "\Preproduccion\AgreementData"
	&sTranserDBToPreProduction
	DELETE FILE AgreementDB.rar

	*Copia hacia el ambiente de pre-producción la BD de Lookups
	RUN rar.exe u CurrentLookupsDB @LookupsDataFiles.txt
	sTranserDBToPreProduction="RUN rar.exe e -ep CurrentLookupsDB " + pathToApp + "\Preproduccion"
	&sTranserDBToPreProduction
	DELETE FILE CurrentLookupsDB.rar

	*Copia hacia el ambiente de pre-producción la BD temporal del sistema
	RUN rar.exe u CurrentTempDB @SivigilaTempDataFiles.txt
	sTranserDBToPreProduction="RUN rar.exe e -ep CurrentTempDB " + pathToApp + "\Preproduccion"
	&sTranserDBToPreProduction
	DELETE FILE CurrentTempDB.rar

	*Copia hacia el ambiente de pre-producción el componente ETL del sistema
	sCopyCmd = 'COPY FILE ' + PATH_TO_SIVIGILAAgreements + 'SivigilaETL.app TO ' + (pathToApp) + '\Preproduccion\SivigilaETL.app'
	&sCopyCmd
	
	*Copia hacia el ambiente de pre-producción queries de carga del componente ETL del sistema
	sCopyCmd = 'COPY FILE ' + PATH_TO_SIVIGILAAgreements + '\*.QPX TO ' + (pathToApp) + '\Preproduccion\*.QPX'
	&sCopyCmd
	
	*Copia hacia el ambiente de pre-producción el componente SIANIESP-RIPS del sistema
	sTranserFOXUSERToPreProduction="RUN rar.exe x -u " + PATH_TO_SIANIESP_RIPS + "Sianiesp_RIPS" + sSIANIESPProductionPackId + ".rar " + pathToApp + "\Preproduccion"
	&sTranserFOXUSERToPreProduction

	*Copia hacia el ambiente de pre-producción la Tabla FOXUSER
	sTranserFOXUSERToPreProduction="RUN rar.exe e -ep FoxUser.rar " + pathToApp + "\Preproduccion"
	&sTranserFOXUSERToPreProduction

	*Copia hacia el ambiente de pre-producción los archivos INI del Sistema
	COPY FILE SivigilaRelatedApps.INI TO (pathToApp) + '\Preproduccion\SivigilaRelatedApps.INI'
	COPY FILE SivigilaDownloader.INI TO (pathToApp) + '\Preproduccion\SivigilaDownloader.INI'
	COPY FILE SivigilaHelp.INI TO (pathToApp) + '\Preproduccion\SivigilaHelp.INI'
	COPY FILE SivigilaReporter.INI TO (pathToApp) + '\Preproduccion\SivigilaReporter.INI'
	sCopyCmd = 'COPY FILE ' + PATH_TO_SIVIGILAAgreements + 'ETLSivigila.INI TO ' + (pathToApp) + '\Preproduccion\ETLSivigila.INI'
	&sCopyCmd 

	*Copia hacia el ambiente de pre-producción el SivigilaDownloader
	COPY FILE SivigilaDownloader.exe TO (pathToApp) + '\Preproduccion\SivigilaDownloader.exe'

	*Copia hacia el ambiente de pre-producción el SivigilaCrypto
	COPY FILE SivigilaCrypto.exe TO (pathToApp) + '\Preproduccion\SivigilaCrypto.exe'

	*Copia hacia el ambiente de pre-producción los archivos txt necesarios para hacer limpieza de archivos "basura" de instalaciones previas
	COPY FILE FiltrarArchivos_Distribucion.bat TO (pathToApp) + '\Preproduccion\FiltrarArchivos.bat'
	COPY FILE sivigilaDataFiles_Distribucion.txt TO (pathToApp) + '\Preproduccion\SivigilaDataFiles.txt'

	*Copia hacia el ambiente de pre-producción el archivo txt necesarios para hacer backups personalizados
	COPY FILE CustomizationDataFilesProduccion.txt TO (pathToApp) + '\Preproduccion\CustomizationDataFilesProduccion.txt'
	
	*Copia hacia el ambiente de pre-producción el componente de terceros 7z
	COPY FILE 7z.dll TO (pathToApp) + '\Preproduccion\7z.dll'
	COPY FILE 7zG.exe TO (pathToApp) + '\Preproduccion\7zG.exe'
	
	*COPY FILE SVG.* TO (pathToApp) + '\Preproduccion'
	*DELETE FILE ((pathToApp) + '\Preproduccion\SVG.MPR')

	sRunCmd="RUN XCOPY /Y /E .\REPORTES\*.* " + (pathToApp) + '\Preproduccion\REPORTES'
	&sRunCmd	

	sRunCmd="RUN XCOPY /Y .\MAPAS\*.* " + (pathToApp) + '\Preproduccion\MAPAS'
	&sRunCmd	

	sRunCmd="RUN XCOPY /Y /E .\DatosValidados\*.* " + (pathToApp) + '\Preproduccion\DatosValidados'
	&sRunCmd	


	MODIFY PROJECT INS NOWAIT
	targetProject=Application.Projects(1)

	FOR EACH oFile IN targetProject.Files
		DO CASE
			CASE oFile.Type='D'
				*Se trata de una tabla libre
				USE (oFile.Name)
				COPY TO (pathToApp) + '\Preproduccion\' + ALIAS() WITH CDX
				USE
			CASE oFile.Type='Q' OR oFile.Type='L'
				*Se trata de un archivo Query o un archivo FLL
				fileNameWithoutPath=STREXTRACT(oFile.Name,'\','',OCCURS('\',oFile.Name))
				targetFileAndPath=(pathToApp) + '\Preproduccion\' + (fileNameWithoutPath)
				COPY FILE (oFile.Name) TO (targetFileAndPath)
		ENDCASE
	NEXT
	DELETE FILE "&pathToApp\Preproduccion\GENERAL.DBF"
	DELETE FILE "&pathToApp\Preproduccion\NOTIFICADOS.*"
	
	*Hace una adaptación de tal forma que se distribuya el qpx de listaEventosindividualesSeguimientos y no el qpr
	DELETE FILE (pathToApp) + '\Preproduccion\listaEventosindividualesSeguimientos.qpr'
	COPY FILE listaEventosindividualesSeguimientos.qpx TO (pathToApp) + '\Preproduccion\listaEventosindividualesSeguimientos.qpx'
	
	*Borra del ambiente de preproduccion la tabla de configuración de servidores de actualización por cuanto ella está integrada al ;
	ejecutable del Sistema
	DELETE FILE (pathToApp) + '\Preproduccion\SivigilaDownloaderServers.DBF'

	*Borra del ambiente de preproduccion la tabla de pwds de cuentas de email por cuanto ella está integrada al ;
	ejecutable del Sistema
	DELETE FILE (pathToApp) + '\Preproduccion\tbEmail.DBF'

	COPY FILE gdiplus.Dll TO (pathToApp) + '\Preproduccion\gdiplus.Dll'
	COPY FILE VB6STKit.Dll TO (pathToApp) + '\Preproduccion\VB6STKit.Dll'
	COPY FILE Net05.ico TO (pathToApp) + '\Preproduccion\Net05.ico'
	COPY FILE Msvcr71.dll TO (pathToApp) + '\Preproduccion\Msvcr71.dll'
	COPY FILE enumproc.Dll TO (pathToApp) + '\Preproduccion\enumproc.Dll'
	COPY FILE VFP9Dlls\vfp9resn.dll  TO (pathToApp) + '\Preproduccion'
	COPY FILE VFP9Dlls\vfp9renu.dll  TO (pathToApp) + '\Preproduccion'
	COPY FILE VFP9Dlls\vfp9r.dll  TO (pathToApp) + '\Preproduccion'
	COPY FILE VFP9Dlls\vfp9t.dll  TO (pathToApp) + '\Preproduccion'

	appExeFileName='Sivigila2018_V' + nVersion
	targetProject.Build(appExeFileName,3,.F.,.T.)
	COPY FILE (appExeFileName) + '.exe' TO (pathToApp) + '\Preproduccion\Sivigila.exe'

	COPY FILE SIVIGILAProductionFiles.txt  TO (pathToApp) + '\Preproduccion'
	sCurrentDir = SYS(2003)
	SET DEFAULT TO (pathToApp) + '\Preproduccion'
	makeSivigilaPackage = "rar u Sivigila2018_V"  + STRTRAN(nVersion,"_",".") + ".rar" + " @SIVIGILAProductionFiles.txt"
	sRunCmd="RUN " + makeSivigilaPackage 
	&sRunCmd
	SET DEFAULT TO &sCurrentDir

	SET SAFETY ON

	CLOSE ALL
ELSE
	MESSAGEBOX( "Debe suministrar el sufijo ID de SIANIESP-RIPS", 0 + 48 + 256, "Error de ejecución")
ENDIF
