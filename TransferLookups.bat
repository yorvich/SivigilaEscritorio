cls
(
FOR /F %%G IN (LookupsDataFiles.txt) DO (
copy %%G \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo
copy %%G  \proyectoSivigila\pruebas
rar.exe u \proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\Lookups_V%1 %%G
)
)

   