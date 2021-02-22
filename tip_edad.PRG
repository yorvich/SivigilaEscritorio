PARAMETERS tt_ee
DO case
   CASE tt_ee='1'
   tt_ee='Años'
   CASE tt_ee='2'
   tt_ee='Meses'
   CASE tt_ee='3'
   tt_ee='Días'
   CASE tt_ee='4'
   tt_ee='Horas'
   CASE tt_ee='5'
   tt_ee='Minutos'
ENDCASE
RETURN tt_ee
