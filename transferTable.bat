@echo off

IF "%1"=="/?" GOTO ShowHelp

echo on
cls

copy %1.dbf \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo
copy %1.cdx \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo
copy %1.fpt \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo

copy %1.dbf \proyectoSivigila\pruebas
copy %1.cdx \proyectoSivigila\pruebas
copy %1.fpt \proyectoSivigila\pruebas

rar.exe u \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%1_V%2 %1.dbf
rar.exe u \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%1_V%2 %1.cdx
rar.exe u \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%1_V%2 %1.fpt  

@echo off

IF "%3"=="" GOTO End
rem Else
	copy %1.dbf %3
	copy %1.cdx %3
	copy %1.fpt %3
rem EndIF

goto End

:ShowHelp
echo Transfiere los archivos de una tabla (dbf + cdx + fpt) hacia las carpetas NovedadesDesarrollo, Pruebas y la especificada en el tercer parámetro, si es que se suministra ese parámetro.
echo (---------------------------)
echo Uso:	TransferTable filename newVersionId [secondTargetFolder]

:End
