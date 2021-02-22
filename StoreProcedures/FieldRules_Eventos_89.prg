FUNCTION FieldRuleFor_Eventos_89_CUADRO_CLI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUADRO_CLI,'1','2')) AND ( .NOT. EMPTY(CUADRO_CLI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_NEX_EPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEX_EPI,'1','2')) AND ( .NOT. EMPTY(NEX_EPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RADIOLOGIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RADIOLOGIC,'1','2')) AND ( .NOT. EMPTY(RADIOLOGIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ADA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADA,'1','2')) AND ( .NOT. EMPTY(ADA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TUBERCULIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TUBERCULIN,'1','2')) AND ( .NOT. EMPTY(TUBERCULIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PRUEB_MOLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRUEB_MOLE,'1','2')) AND ( .NOT. EMPTY(PRUEB_MOLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RES_PR_MOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_PR_MOL,'1','2')) OR ( EMPTY(RES_PR_MOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ESP_PMOLEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_PMOLEC,'1','2','3','4','5')) OR ( EMPTY(ESP_PMOLEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_DIABETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIABETES,'1','2')) AND ( .NOT. EMPTY(DIABETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_SILICOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SILICOSIS,'1','2')) AND ( .NOT. EMPTY(SILICOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ENFE_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENFE_RENAL,'1','2')) AND ( .NOT. EMPTY(ENFE_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_EPOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPOC,'1','2')) AND ( .NOT. EMPTY(EPOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ENFE_HEPAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENFE_HEPAT,'1','2')) AND ( .NOT. EMPTY(ENFE_HEPAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_A�O
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(A�O))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_COND_TUBER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COND_TUBER,'1','2')) AND ( .NOT. EMPTY(COND_TUBER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TIP_TUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_TUB,'1','2')) AND ( .NOT. EMPTY(TIP_TUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_LOCTBREXTR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOCTBREXTR,'1','2','3','4','5','7','8','9','10','11','12')) OR ( EMPTY(LOCTBREXTR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_CLAS_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_ANT,'1','2')) AND ( .NOT. EMPTY(CLAS_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_CLASCASO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLASCASO,'2','3','4','5','6','7')) OR ( EMPTY(CLASCASO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TRAB_SALUD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAB_SALUD,'1','2')) AND ( .NOT. EMPTY(TRAB_SALUD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PREV_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PREV_VIH,'1','2')) AND ( .NOT. EMPTY(PREV_VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PESO_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_ACT>=2.0  AND PESO_ACT <=250.0 AND ISNUMERIC(PESO_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TALLA_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA_ACT>=0.20  AND TALLA_ACT <=2.5 AND ISNUMERIC(TALLA_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_BACILOSCOP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BACILOSCOP,'1','2')) AND ( .NOT. EMPTY(BACILOSCOP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RES_BK
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_BK,'1','2')) OR ( EMPTY(RES_BK)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_CULTIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CULTIVO,'1','2')) AND ( .NOT. EMPTY(CULTIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RESCULTIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESCULTIVO,'1','2','3')) OR ( EMPTY(RESCULTIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_HISTOPATOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HISTOPATOL,'1','2')) AND ( .NOT. EMPTY(HISTOPATOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RESHISTOPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESHISTOPA,'1','2')) OR ( EMPTY(RESHISTOPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_CANCER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CANCER,'1','2')) AND ( .NOT. EMPTY(CANCER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ARTRITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRITIS,'1','2')) AND ( .NOT. EMPTY(ARTRITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_DESNUTRICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESNUTRICI,'1','2')) AND ( .NOT. EMPTY(DESNUTRICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PSF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSF,'1','2')) OR ( EMPTY(PSF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TIPO_RESIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_RESIS,'1','2','3','4','6','7','8')) OR ( EMPTY(TIPO_RESIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ESTREPTOMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTREPTOMI,'1','2','3')) OR ( EMPTY(ESTREPTOMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ISONIAZIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ISONIAZIDA,'1','2','3')) OR ( EMPTY(ISONIAZIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RIFAMPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RIFAMPI,'1','2','3')) OR ( EMPTY(RIFAMPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ETAMBUTOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ETAMBUTOL,'1','2','3')) OR ( EMPTY(ETAMBUTOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PIRAZINAMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIRAZINAMI,'1','2','3')) OR ( EMPTY(PIRAZINAMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_QUINOLAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUINOLAS,'1','2','3')) OR ( EMPTY(QUINOLAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_INYECTABLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INYECTABLE,'1','2','3')) OR ( EMPTY(INYECTABLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


