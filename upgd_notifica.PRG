PARAMETERS bbaa
*dependeiente de form rep_sem

   SELECT (bbaa)
   area_as_per=SELECT()
   DO ASIGNA_PER
   SELECT (bbaa)
   GO TOP
   DO WHILE !EOF()
      IF año=nc(m.vigencia) AND &pf
         CP=COD_PRE
         CS=COD_SUB
         SELECT (AREA_SN)
         SEEK CP+CS
         IF !FOUND()
            &INSERTA
         ENDIF
         SELECT (bbaa)
      ENDIF
      SKIP
   ENDDO
