PUBLIC oReporter as Object
*This must be defined as PUBLIC as several reports depend on properties of this object established by the invoked method
SET STEP ON 
oReporter=NEWOBJECT("SIVIGILAReporter","SIVIGILAReporter.PRG")

*oReporter.makeFullfilmentReport('between(VAL(semana),1,26) and año="2016"','2016')

*oReporter.makeNegativeNotificationReport_V02("2017",'tst')

oReporter.notificationFromQueryToXLS('607','.t.',,,,,'datos_basicos_607','_'+DTOC(DATE()))
*oReporter.notificationFromQueryToXLS('235','.t.',,,,,'datos_basicos_607','_'+DTOC(DATE()))

RELEASE oReporter 