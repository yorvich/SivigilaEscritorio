parameter dato

SELE 10
USE ENTRA_02 ORDER 2
SEEK DATO
_ucs='No se le ha asignado acceso para '+iif(found(),allt(acceso_a),'ejecutar esta opción')+chr(13)+'Por favor solicite autorización al administrador del sistema'

sele 11
USE ENTRA_03 ORDER 2
seek _cod_acc+dato
