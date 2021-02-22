 PARAMETER based, caMpo, llAve, orDen
 SET EXACT ON
 acTtab = ALIAS()
 USE IN 0 (based) AGAIN ALIAS coNcurre
 SELECT coNcurre
 SET ORDER TO (orDen)
 SELECT coNcurre
 SEEK llAve
 vaR = EVALUATE(caMpo)
 SET EXACT OFF
* IF  .NOT. EOF()
 if found()
      USE IN coNcurre
      IF  .NOT. EMPTY(acTtab)
           SELECT (acTtab)
      ENDIF
      RETURN vaR
 ELSE
      IF  .NOT. EMPTY(acTtab)
           SELECT (acTtab)
      ENDIF
      USE IN coNcurre
      RETURN ''
 ENDIF
