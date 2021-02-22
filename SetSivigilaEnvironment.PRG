SET NEAR ON
SET STATUS BAR ON
SET CLOCK STATUS
SET TALK OFF
SET SAFETY OFF
SET ESCAPE OFF
SET LOCK OFF
SET BELL OFF
SET DATE TO DMY
SET CENTURY ON
SET EXCLUSIVE OFF
SET AUTOSAVE ON
SET OPTIMIZE ON
SET readb OFF
SET DELE ON
SET REFRESH TO 1,1
SET EXACT ON 
SET SECONDS OFF
SET ENGINEBEHAVIOR 70

ON SHUTDOWN QUIT
*SET RESOURCE OFF

*Comentado por: Wilson Aguilar Fecha: 13/11/2009;
Fija el intervalo de tiempo que se debe esperar por la siguiente pulsación de tecla en los ;
controles que admiten la propiedad búsqueda incremental como los combo box. Este valor es necesario ;
para que funcionen corectamente las búsquedas incrementales.
_INCSEEK = 5	&& (segundos)

SET MULTILOCKS OFF
*= NUMLOCK(.T.)

SET PATH TO '.\SivigilaData' ADDITIVE 
OPEN DATABASE Lookups
OPEN DATABASE BDSivigila
