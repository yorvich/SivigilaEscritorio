_LLAVE="SEEK M.AÑO + M.SEMANA + M.COD_EVE + M.TIP_IDE + M.NUM_IDE"

         GO TOP
         DO WHILE !EOF()
            AJ=AJUSTE
            SCATTER MEMVAR
            IF VAL(AJ)<>0
               RE_AC=RECN()
               VE1=EDAD
               VE2=UNI_MED
               M_R=COD_MUN
               M_P=MUN_PRO
               C_P=COD_PRE
               C_S=COD_SUB
               cl_ini=tip_cas
               raju=IIF(ajuste='7',', TIP_CAS with cl_ini','')
               defun=cer_def
               fe_na=fecha_nto
               tel=telefono
               &_LLAVE
               DO CASE
               CASE BETWEEN(VAL(AJ),3,6)
                  IF FOUND()
                     IF VAL(ajuste)>0
                        DO WHILE AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE=M.AÑO+M.SEMANA+M.COD_EVE+M.TIP_IDE+M.NUM_IDE
                           SKIP
                        ENDDO
                        SKIP -1
                     ENDIF
                     IF VAL(AJUSTE)=0
                        REPLACE TIP_CAS WITH AJ
                     ELSE
                        IF BETWEEN(VAL(aj),3,6)
                           REPLACE ajuste WITH '0', tip_cas WITH aj
                        ENDIF
                     ENDIF
                     GO RE_AC
                  ELSE
                     GO RE_AC
                     IF BETWEEN(VAL(aj),3,6)
                        REPLACE ajuste WITH '0', tip_cas WITH aj
                     ENDIF
                  ENDIF
               CASE AJ='7'
                  IF FOUND() AND RECNO()<>re_ac
                     REPLACE EDAD WITH VE1, UNI_MED WITH VE2, COD_MUN WITH M_R, MUN_PRO WITH M_P,;
                        COD_PRE WITH C_P, COD_SUB WITH C_S &raju, cer_def WITH defun, fecha_nto WITH fe_na, telefono WITH tel
                     IF VAL(ajuste)=7 
                        IF BETWEEN(VAL(TIP_CAS),3,6)
                           REPLACE ajuste WITH '0'
                        ENDIF
                     endif
                     GO RE_AC
                  ELSE
                     GO RE_AC
                     REPLACE ajuste WITH '0'
               ENDIF
               ENDCASE
            ENDIF
            SKIP
         ENDDO
