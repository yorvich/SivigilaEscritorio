FUNCTION FieldRuleFor_eventos_34_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_A�O
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(A�O))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_FEC_INV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ENTID_INVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENTID_INVE,'1','2','3')) AND ( .NOT. EMPTY(ENTID_INVE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_CAUS_MUER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUS_MUER,'1','2','3')) AND ( .NOT. EMPTY(CAUS_MUER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_SIT_MUER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_MUER,'1','2','3','4','5')) AND ( .NOT. EMPTY(SIT_MUER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_NOM_ENTREV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_ENTREV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_TIP_IDE_EN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_EN,'CC','CE','PA','AS')) AND ( .NOT. EMPTY(TIP_IDE_EN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_NUM_IDE_EN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_EN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_PARENT_ENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARENT_ENT,'1','2','3','4','5')) AND ( .NOT. EMPTY(PARENT_ENT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_PARENT_CUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARENT_CUI,'1','2','5')) AND ( .NOT. EMPTY(PARENT_CUI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ETNIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ETNIA,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(ETNIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ESC_CUID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESC_CUID,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESC_CUID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ESTRATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTRATO,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(ESTRATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ESTAD_CUID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTAD_CUID,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESTAD_CUID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_OCUPAC_CUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(OCUPAC_CUI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ESQ_VACU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESQ_VACU,'1','2','3')) AND ( .NOT. EMPTY(ESQ_VACU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_CARNE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARNE,'1','2')) AND ( .NOT. EMPTY(CARNE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_DESNUTRICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESNUTRICI,'1','2')) AND ( .NOT. EMPTY(DESNUTRICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_BAJO_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BAJO_PESO,'1','2')) AND ( .NOT. EMPTY(BAJO_PESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_HACINAMIEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HACINAMIEN,'1','2')) AND ( .NOT. EMPTY(HACINAMIEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_HAB_INADEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HAB_INADEC,'1','2')) AND ( .NOT. EMPTY(HAB_INADEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_PISO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PISO,'1','2')) AND ( .NOT. EMPTY(PISO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_INSECTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INSECTOS,'1','2')) AND ( .NOT. EMPTY(INSECTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_INA_MAN_AL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INA_MAN_AL,'1','2')) AND ( .NOT. EMPTY(INA_MAN_AL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_FUEN_AGUA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FUEN_AGUA,'1','2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(FUEN_AGUA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_DISP_EXCRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISP_EXCRE,'1','2','3','4','5')) AND ( .NOT. EMPTY(DISP_EXCRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_RECON_SIG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECON_SIG,'1','2','3')) AND ( .NOT. EMPTY(RECON_SIG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_BUSC_AYUDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BUSC_AYUDA,'1','2','3')) AND ( .NOT. EMPTY(BUSC_AYUDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_FACIL_TRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FACIL_TRAN,'1','2','3')) AND ( .NOT. EMPTY(FACIL_TRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_RECON_DIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECON_DIA,'1','2')) AND ( .NOT. EMPTY(RECON_DIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_RECIBI_SRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECIBI_SRO,'1','2')) AND ( .NOT. EMPTY(RECIBI_SRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_TERAP_NO_M
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TERAP_NO_M,'1','2')) AND ( .NOT. EMPTY(TERAP_NO_M)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_DIFIC_ADM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIFIC_ADM,'1','2','3')) AND ( .NOT. EMPTY(DIFIC_ADM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_CUAL_DIFIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUAL_DIFIC,'1','2','3')) OR ( EMPTY(CUAL_DIFIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ATN_OPORTU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATN_OPORTU,'1','2','3')) AND ( .NOT. EMPTY(ATN_OPORTU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_CALIDAD_AT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CALIDAD_AT,'1','2','3','4')) AND ( .NOT. EMPTY(CALIDAD_AT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


