#INCLUDE DevEnvironment.h

#DEFINE MAX_UCIS_BY_LINE 9
*Número máximo de UCI que se pueden tener en una línea para una UPGD dada identificada con los campos COD_PRE,COD_SUB

DO TransposeTable WITH 'SivigilaTemp', 'UPGD_UCIS', 'COD_PRE,COD_SUB', .T., ;
	'NOTIFICA,PERIODO,FLU_NOT,INMEDIATA,FECHACARGA,ESTADOTRAN,VERSION,AJUSTE,FEC_AJU,EST_NOTIF', MAX_UCIS_BY_LINE  IN PATH_TO_COMMON_LIB + 'TablesHandler'

