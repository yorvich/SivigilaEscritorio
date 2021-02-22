dd=ALLTRIM(desc_dat)
lon_cam=LEN(dd)

pos=1

dig=LEN(valor)

car=SUBSTR(dd,pos,dig)
ya=0
pi=0
DO WHILE lon_cam>=pos and ya=0
   IF car=valor
      pi=pos
      DO WHILE LEFT(car,1)!=',' AND lon_cam>=pos
         pos=pos+1
         car=SUBSTR(dd,pos,1)
         ya=1
      ENDDO
      pf=pos
      exit
   endif
   pos=pos+1
   car=SUBSTR(dd,pos,dig)
ENDDO

REGISTRO=SUBSTR(dd,pi,(pos-pi))
IF EMPTY(ALLTRIM(REGISTRO))
   REGISTRO='Valor '+valor+' registrado no esta dentro de lo permitido!'
ENDIF
RETURN registro
