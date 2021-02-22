*modulo de bus_activa
tit('Aplicando filtro a cronicas segun Tipo de Diagnóstico...')
SELECT (AREA_FINAL)
SET DELETED OFF
x=_dir+'\rips_1'
SELECT tip_doc, num_doc, fecha, tip_dia, RECNO() AS REGISTRO;
   FROM (abt);
   ORDER BY 1,2,3;
   INTO TABLE (x)
area_fil=SELECT()
INDEX ON tip_doc+num_doc tag doc
DELETE all

x=_dir+'\rips_2'
SELECT tip_doc, num_doc FROM (abt) GROUP BY 1,2 INTO TABLE (x)
area_doc=SELECT()

enc=0
GO top
DO WHILE !EOF()
   llv=tip_doc+num_doc
   SELECT (area_fil)
   SET KEY TO llv
   vlr='2'
   DO bus_caso
   IF enc=0
      vlr='3'
      DO bus_caso
      IF enc=0
         vlr='4'
         DO bus_caso
         IF enc=0
            vlr='1'
            DO bus_caso
         endif
      endif
   endif
   SELECT (area_doc)
   skip
ENDDO

SELECT (AREA_FINAL)
DELETE ALL

SELECT (area_fil)
SET KEY TO
GO TOP
SCAN
   IF !DELETED()
      R=REGISTRO
      SELECT (AREA_FINAL)
      GO R
      RECALL
      SELECT (area_fil)
   ENDIF
ENDSCAN
SET DELETED ON
SELECT (AREA_FINAL)
COUNT TO tot_reg_bus FOR !DELETED()
GO top
tit('Ok')


PROCEDURE bus_caso
   enc=0
   GO top
   SCAN
      IF tip_dia=vlr
         recall
         enc=enc+1
      ENDIF
   ENDSCAN
   IF enc>1
      SET DELETED ON
      GO TOP
      SKIP
      DO WHILE !EOF()
         DELETE
         SKIP
      ENDDO
      SET DELETED OFF
   endif
