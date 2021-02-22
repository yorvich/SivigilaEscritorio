USE paciente
REPLACE notifica WITH 0 all
*zap
USE laboratorios
REPLACE notifica WITH 0 all
*zap
USE brotes
REPLACE notifica WITH 0 all
*zap

FOR e=1 TO 24
   b='eventos_'+TRANSFORM(e, '@l 99')
   USE (b)
   REPLACE notifica WITH 0 all
*zap
ENDFOR
