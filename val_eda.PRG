PARAMETERS dat_cap

*Comentado por : Wilson Aguilar	Fecha: 03/03/2010;
El código de esta función es confuso y dá lugar, en consecuencia, a que su mantenimiento sea dificil; 
por tanto se decidió sustituirlo por una función completamente nueva llamada calculateAge incluida en el módulo Utilities
dn=VAL(SUBSTR(DTOC(dat_cap),1,2))
mn=VAL(SUBSTR(DTOC(dat_cap),4,2))
an=VAL(SUBSTR(DTOC(dat_cap),7,4))

dc=VAL(SUBSTR(DTOC(DATE()),1,2))
mc=VAL(SUBSTR(DTOC(DATE()),4,2))
ac=VAL(SUBSTR(DTOC(DATE()),7,4))

IF dc>=dn
   dia=dc-dn
ELSE
   dc=dc+30
   mc=mc-1
   dia=dc-dn
ENDIF
IF mc>mn
   vmes=mc-mn
ELSE
   mc=mc+12
   ac=ac-1
   vmes=mc-mn
ENDIF
m.edad=ac-an
IF dia=30
   vmes=vmes+1
ENDIF
IF vmes=12
   vmes=0
   m.edad=m.edad+1
ENDIF
te='Anos'
if m.edad<1
   if vmes=0
      m.edad=dia
      te='Días'
      m.uni_med='3'
   else
      m.edad=vmes
      te='Meses'
      m.uni_med='2'
   endif
else
   m.uni_med='1'
endif
if dat_cap=date()
   m.edad=0
   m.uni_med='1'
   te='Días'
endif
m.edad=allt(str(m.edad))

