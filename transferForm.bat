@echo off

IF "%1"=="/?" GOTO ShowHelp

echo on
cls
copy %1.scx \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo
copy %1.sct \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo

copy %1.scx \proyectoSivigila\pruebas
copy %1.sct \proyectoSivigila\pruebas

rar.exe u \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%1_V%2 %1.scx
rar.exe u \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%1_V%2 %1.sct
@echo off



IF "%3"=="" GOTO End
rem Else
	copy %1.scx %3
	copy %1.sct %3
rem EndIF

goto End

:ShowHelp
echo Transfiere los archivos de un Form (scx + sct) hacia las carpetas NovedadesDesarrollo, Pruebas y la especificada en el tercer parámetro, si es que se suministra ese parámetro.
echo (---------------------------)
echo Uso:	TransferForm filename newVersionId [secondTargetFolder]

:End  