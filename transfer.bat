cls

copy %1.dbf C:\Users\wilson.aguilar\Documents\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo
copy %1.cdx C:\Users\wilson.aguilar\Documents\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo
copy %1.sct C:\Users\wilson.aguilar\Documents\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo
copy %1.scx C:\Users\wilson.aguilar\Documents\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo

copy %1.dbf C:\Users\wilson.aguilar\Documents\proyectoSivigila\pruebas
copy %1.cdx C:\Users\wilson.aguilar\Documents\proyectoSivigila\pruebas
copy %1.sct C:\Users\wilson.aguilar\Documents\proyectoSivigila\pruebas
copy %1.scx C:\Users\wilson.aguilar\Documents\proyectoSivigila\pruebas

rar.exe u C:\Users\wilson.aguilar\Documents\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%1_V%2 %1.dbf
rar.exe u C:\Users\wilson.aguilar\Documents\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%1_V%2 %1.cdx
rar.exe u C:\Users\wilson.aguilar\Documents\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%1_V%2 %1.sct
rar.exe u C:\Users\wilson.aguilar\Documents\proyectoSivigila\%SIVIGILAMainDevelopmentDir%\NovedadesDesarrollo\%1_V%2 %1.scx
   