_LLAVE="SEEK M.AÑO + M.SEMANA + M.COD_EVE + M.TIP_IDE + M.NUM_IDE"

TIT('Procesando ajustes...')
SET TALK ON
SELECT FEC_NOT, SEMANA, AÑO, COD_EVE, TIP_IDE, NUM_IDE, sexo, uni_med, edad, cod_mun, MUN_PRO, TIP_CAS, ajuste;
   FROM PACIENTE;
   WHERE año=nc(m.vigencia);
   INTO TABLE (TABLA_PTE_TMP)
SET TALK OFF
LLAVE='AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE'
INDEX ON &LLAVE TAG SIRE
GO TOP
DO WHILE !EOF()
   AJ=AJUSTE
   SCATTER MEMVAR
   IF VAL(AJ)<>0
      RE_AC=RECN()
      CL_INI=TIP_CAS
      RAJU=IIF(AJUSTE='7','repl TIP_CAS with cl_ini','')
      &_LLAVE
      DO CASE
      CASE BETWEEN(VAL(AJ),3,6)
         IF FOUND()
            REPLACE TIP_CAS WITH AJ
         ENDIF
         GO RE_AC
      CASE AJ='7'
         IF FOUND()
            &RAJU
         ENDIF
         GO RE_AC
      ENDCASE
   ENDIF
   SKIP
ENDDO
DELETE ALL FOR VAL(AJUSTE)<>0
IF scconf=1
   DELETE ALL FOR !BETWEEN(VAL(tip_cas),3,5)
endif
pack
TIT('Ok')
