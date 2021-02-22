@echo off

if "%1"=="/?" goto ShowHelp

echo on
cls

copy %1.prg \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo
copy %1.fxp \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo

@echo off
if "%3"=="1" goto rarFiles
echo on
copy %1.prg \proyectoSivigila\pruebas
copy %1.fxp \proyectoSivigila\pruebas

:rarFiles
@echo off
For %%A in ("%1%") do (
    Set FileNameOnly=%%~nxA
)
echo on
rar.exe u "\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%FileNameOnly%_V%2" "%1.prg"
rar.exe u "\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%FileNameOnly%_V%2" "%1.fxp"

@goto End

:ShowHelp
echo Transfiere los archivos de un m�dulo (prg + fxp)  hacia las carpetas NovedadesDesarrollo y Pruebas. Si se suministra el tercer par�metro y es igual a 1 (True), NO se copiaran los archivos hacia la carpeta Pruebas
echo (---------------------------)
echo Uso:	TransferPrg filename newVersionId [IsCommon]

:End
  