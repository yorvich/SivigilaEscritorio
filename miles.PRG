parameter defecto
decimales=''

IF val(RIGHT(NC(defecto*100),2))>0
  decimales='.'+RIGHT(NC(defecto*100),2)
ENDIF

DEFECTO=INT(DEFECTO)
cadena=''
miles=allt(str(defecto))
lm=len(miles)
y=0
X=0
do while lm>=Y
   y=y+3
   cadena=substr(miles,lm-y+1,3)+iif(empty(cadena),'',','+cadena)
   x=x+1
enddo
x=x-1
t=lm-(x*3)
if t=0
   cadena=substr(cadena,2,len(cadena))
else
   cadena=substr(miles,1,t)+cadena
endif

cadena=cadena+decimales
return cadena
