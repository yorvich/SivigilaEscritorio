PARAMETERS bbaa
*dependiente de form sem_not, rep_sem

SELECT (bbaa)
GO top
DO WHILE !EOF()
   IF año=nc(m.vigencia) AND semana=nc(m.semana) &MNC
      cp=cod_pre
      cs=cod_sub
      se=nc(VAL(semana))
      SELECT (area_sn)
      IF !SEEK(cp+cs+se)
         INSERT INTO (ALIAS()) (cod_pre,cod_sub, semana, original) ;
            VALUES (cp, cs, se, 1)
      endif
      SELECT (bbaa)
   endif
   skip
enddo
