SET STEP ON 

USE cie10
SET ORDER TO 1

codToLookFor=''
IF !SEEK(codToLookFor)
   do bus_rap WITH 'CIE10',2,75,342,0,0,'Codigo','Nombre','','NOM_CIE','CIE'
   this.Value=cod_CIE
   SELECT 14
   RETURN 0
ENDIF
tit(ALLTRIM(NOM_CIE))