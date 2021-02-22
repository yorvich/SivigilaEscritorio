#DEFINE CD_TABLE_NAME 'EVENTOS_07' 

*Efectua una transformación del campo EVENTOS_07.EST_CLI de tal forma que sea equivalente al campo PACIENTE.CON_FIN: ;
PACIENTE.CON_FIN='1' es equivalente a EVENTOS_07.EST_CLI='1' OR EVENTOS_07.EST_CLI='2' ;
PACIENTE.CON_FIN='2' es equivalente a EVENTOS_07.EST_CLI='3'


SELECT *,IIF(EST_CLI='1' OR EST_CLI='2','1','2') AS CON_FIN_ FROM (CD_TABLE_NAME) INTO CURSOR rsEventos_07_To_XLS