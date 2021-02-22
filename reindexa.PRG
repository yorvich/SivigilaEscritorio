* Version: Sivigila 2010

if _nivel!='1'
   RETURN
ENDIF

t_i('Para un mejor y optimo rendimiento,'+chr(13)+'Este proceso se debe ejecutar directamente en el Servidor.')

t_sn('Se debe cerrar el aplicativo en todas las estaciones de red...'+chr(13)+;
   'Desea continuar')
if _rta<>6
   return
endif

DO updateSIVIGILATableIndexes IN sivigilaIndexesHandler
