SELE 10
x=_dir+'\tmp_per.dbf'
copy stru to (x)
sele 10
use (x)
*index on orden tag orden

x=_dir_ini+'\entra_02'
appen from (x)


SELE 10
inser into (alias()) (orden, acceso_a) values (1999, '------------------------------------->>>  ACCESO A EVENTOS')
sele 7
go top
x=2000

do while !eof()
   c1='EVE'+cod_eve
   n1='EVENTO - '+cod_eve+nom_eve
   sele 10
   inser into (alias()) (orden, clave, acceso_a) values (x, c1, n1)
   x=x+1
   sele 7
   skip
enddo

CAPSLOCK(.T.)
SELE 11
USE entra_03 ORDER 1
SEEK m.cod_usu
IF FOUND()
   USE entra_03 ORDER 2
   x=0
   SELE 10
   GO TOP
   DO WHILE !EOF()
      clav=clave
      SELE 11
      SEEK m.cod_usu+clav
      IF FOUND()
         na=acceso
      ELSE
         na=''
      ENDIF
      SELE 10
      REPL acceso WITH na
      SKIP
   ENDDO
   GO TOP
   bas_per=SELECT()
   do form asig_per
ELSE
   x=1
   SELE 10
   REPL acceso WITH IIF(m.nivel='1','S','N') ALL
   GO TOP
   bas_per=SELECT()
   do form asig_per
ENDIF
sele 10
x=_dir_ini+'\entra_02'
use (x) order 1

