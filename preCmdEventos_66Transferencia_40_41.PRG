*Ejecuta las operaciones necesarias para dar solución a la incidencia ID=1260

LOCAL oSivigilaDuplicatesSolver as Object 

LOCAL sOldDeleted as String
sOldDeleted = SET("Deleted")

glDisableRules = .T.

SET PROCEDURE TO SivigilaUtilities ADDITIVE 

SET DELETED ON 
*Corrige en datos complementarios los casos con condición final 2 (Muerto) y Tipo exposición 3 (Suicidio) asignándoles el nuevo valor 11 establecido para ;
la opción Suicidio. El proceso se lleva a cabo en dos pasos
*P1: Selección de los casos objeto de corrección
SELECT distinct p.año+p.Semana+p.Cod_Eve+p.Tip_Ide+p.Num_Ide+p.Cod_Pre+p.Cod_Sub as pid FROM EVENTOS_66 E ;
	INNER JOIN PACIENTE P ON ;
	P.año=E.Año AND P.Semana=E.Semana AND P.Cod_Eve=E.Cod_Eve AND P.Tip_Ide=E.Tip_Ide AND P.Num_Ide=E.Num_Ide AND P.Cod_Pre=E.Cod_Pre AND P.Cod_Sub=E.Cod_sub ;
	WHERE P.Cod_Eve='365' AND P.AÑO='2018' AND P.CON_FIN='2' AND E.TIP_EXP='3' INTO CURSOR rsTmp nofilter
*P2: Corrección de los casos objetivo
UPDATE Eventos_66 SET EVENTOS_66.TIP_EXP='11' WHERE TIP_EXP='3' AND año+Semana+Cod_Eve+Tip_Ide+Num_Ide+Cod_Pre+Cod_Sub IN ;
	(Select pid from rsTmp)


*Corrige los casos con condición final 1 (Vivo) y Tipo exposición 3 (Suicidio) descartándolos por error de digitación. El proceso se lleva a cabo en dos pasos
*P1: Inserción de un registro de ajuste 7 en datos básicos para asignar principalmente el Nombre del que diligencia la ficha (=SISTEMA SIVIGILA)
SELECT distinct P.* FROM EVENTOS_66 E INNER JOIN PACIENTE P ON ;
	P.año=E.Año AND P.Semana=E.Semana AND P.Cod_Eve=E.Cod_Eve AND P.Tip_Ide=E.Tip_Ide AND P.Num_Ide=E.Num_Ide AND P.Cod_Pre=E.Cod_Pre AND P.Cod_Sub=E.Cod_sub ;
	WHERE P.Cod_Eve='365' AND P.AÑO='2018' AND P.CON_FIN!='2' AND E.TIP_EXP='3' INTO CURSOR rsAjustar READWRITE
oSivigilaDuplicatesSolver = NEWOBJECT("SivigilaDuplicatesSolver","cls_SivigilaDuplicatesSolver.fxp",.null.,'rsAjustar', .T.)
WITH oSivigilaDuplicatesSolver
	.sXlsFilePath = SYS(2023)
	.deleteDuplicates(.T.)
ENDWITH
RELEASE oSivigilaDuplicatesSolver

UPDATE rsAjustar SET AJUSTE='7', NOM_DIL_FI='SISTEMA SIVIGILA', FEC_AJU=DATE(), NOTIFICA=88
INSERT INTO Paciente SELECT * FROM rsAjustar 

*P2: Inserción de un registro de ajuste D en datos básicos para descartar el caso
UPDATE rsAjustar SET AJUSTE='D', FEC_AJU=DATE(), NOTIFICA=88
INSERT INTO Paciente SELECT * FROM rsAjustar 

USE IN rsAjustar

SET DELETED &sOldDeleted 
glDisableRules = .F.