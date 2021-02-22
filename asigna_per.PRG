TIT('Asignando Periodos a las Semanas de Notificación')
GO TOP
DO WHILE !EOF()
   SM=AÑO+nc(VAL(SEMANA))
   SELECT 12
   SEEK SM
   P=IIF(FOUND(),PERIODO,0)
   SELECT(area_as_per)
   REPLACE PERIODO WITH P
   SKIP
ENDDO
