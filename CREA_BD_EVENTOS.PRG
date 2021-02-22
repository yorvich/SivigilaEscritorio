   FOR evto=1 TO 32
      IF evto<>23
      ev1='eventos_'+TRANSFORM(evto,'@l 99')
      ev2=_dir+'\eventos_'+TRANSFORM(evto,'@l 99')
      num_bas1='select '+nc(60+evto)
      num_bas2='select '+nc(20+evto)
      &num_bas1
      USE
      &num_bas2
      USE
      SELECT 20
      USE
      USE (ev1)
      COPY STRUCTURE TO (ev2)
      USE (ev2)
      INDEX ON año+semana+cod_eve+tip_ide+num_ide TAG pte
      USE
      &num_bas1
      USE (ev2)
      ENDIF
   ENDFOR

   SELECT 34
   USE laboratorios ORDER 1
   x=_dir+'\Laboratorios'
   COPY STRUCTURE TO (x)
   SELECT 84
   USE (x)




