FUNCTION FieldRuleFor_eventos_84_SIND_BRONQ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIND_BRONQ,'1','2')) OR ( EMPTY(SIND_BRONQ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EV_CORONAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EV_CORONAR,'1','2')) OR ( EMPTY(EV_CORONAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SEAN_SSSN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEAN_SSSN,'1','2')) OR ( EMPTY(SEAN_SSSN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOS,'1','2')) OR ( EMPTY(TOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DISNEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISNEA,'1','2')) OR ( EMPTY(DISNEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DIF_RESPIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIF_RESPIR,'1','2')) OR ( EMPTY(DIF_RESPIR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DOLOR_TORA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_TORA,'1','2')) OR ( EMPTY(DOLOR_TORA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NAUSEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NAUSEA,'1','2')) OR ( EMPTY(NAUSEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) OR ( EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) OR ( EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DOLOR_ABDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_ABDO,'1','2')) OR ( EMPTY(DOLOR_ABDO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OTRA_CLINI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRA_CLINI,'1','2')) OR ( EMPTY(OTRA_CLINI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_FR_USO_CIG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FR_USO_CIG,'1','2','3','4')) OR ( EMPTY(FR_USO_CIG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NICOTINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NICOTINA,'1','2')) OR ( EMPTY(NICOTINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SABOR_AROM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SABOR_AROM,'1','2')) OR ( EMPTY(SABOR_AROM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DER_CANABI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DER_CANABI,'1','2')) OR ( EMPTY(DER_CANABI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OTRASSUSTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRASSUSTA,'1','2')) OR ( EMPTY(OTRASSUSTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_INTOXICACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INTOXICACI,'1','2')) OR ( EMPTY(INTOXICACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_QUEMADURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUEMADURA,'1','2')) OR ( EMPTY(QUEMADURA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ALERGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALERGIA,'1','2')) OR ( EMPTY(ALERGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_CIGAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_CIGAR,'1','2')) OR ( EMPTY(CONS_CIGAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_MARIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_MARIH,'1','2')) OR ( EMPTY(CONS_MARIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_COCAI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_COCAI,'1','2')) OR ( EMPTY(CONS_COCAI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_BAZUC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_BAZUC,'1','2')) OR ( EMPTY(CONS_BAZUC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_HEROI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_HEROI,'1','2')) OR ( EMPTY(CONS_HEROI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ASMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASMA,'1','2')) OR ( EMPTY(ASMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EPOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPOC,'1','2')) OR ( EMPTY(EPOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ALERG_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALERG_RESP,'1','2')) OR ( EMPTY(ALERG_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_FIBROS_QUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIBROS_QUI,'1','2')) OR ( EMPTY(FIBROS_QUI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ENF_CORONA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENF_CORONA,'1','2')) OR ( EMPTY(ENF_CORONA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NOMBRE_ELE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOMBRE_ELE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_A�O
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(A�O))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OCASIO_POR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OCASIO_POR,'1','4')) AND ( .NOT. EMPTY(OCASIO_POR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ASFIXIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASFIXIA,'1','2')) OR ( EMPTY(ASFIXIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ESTRANGULA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTRANGULA,'1','2')) OR ( EMPTY(ESTRANGULA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_HERIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HERIDA,'1','2')) OR ( EMPTY(HERIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TRAUMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAUMA,'1','2')) OR ( EMPTY(TRAUMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CHOQ_ELECT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHOQ_ELECT,'1','2')) OR ( EMPTY(CHOQ_ELECT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_FRACTURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FRACTURA,'1','2')) OR ( EMPTY(FRACTURA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_POLITRAUMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(POLITRAUMA,'1','2')) OR ( EMPTY(POLITRAUMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_AMPUTACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AMPUTACION,'1','2')) OR ( EMPTY(AMPUTACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_QUEMADURAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUEMADURAS,'1','2')) OR ( EMPTY(QUEMADURAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_INTOXICAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INTOXICAC,'1','2')) OR ( EMPTY(INTOXICAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_INFECCION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFECCION,'1','2')) OR ( EMPTY(INFECCION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SEPSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEPSIS,'1','2')) OR ( EMPTY(SEPSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PERFORACIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PERFORACIO,'1','2')) OR ( EMPTY(PERFORACIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_HEMORRAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMORRAGIA,'1','2')) OR ( EMPTY(HEMORRAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NECROSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NECROSIS,'1','2')) OR ( EMPTY(NECROSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EMBOLIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EMBOLIA,'1','2')) OR ( EMPTY(EMBOLIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DEPRE_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEPRE_RESP,'1','2')) OR ( EMPTY(DEPRE_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CRANEO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRANEO,'1','2')) OR ( EMPTY(CRANEO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CARA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARA,'1','2')) OR ( EMPTY(CARA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OJOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OJOS,'1','2')) OR ( EMPTY(OJOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NARIZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NARIZ,'1','2')) OR ( EMPTY(NARIZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OREJAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OREJAS,'1','2')) OR ( EMPTY(OREJAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_BOCA_DIENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BOCA_DIENT,'1','2')) OR ( EMPTY(BOCA_DIENT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CUELLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUELLO,'1','2')) OR ( EMPTY(CUELLO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_BRAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BRAZO,'1','2')) OR ( EMPTY(BRAZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ANTEBRAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANTEBRAZO,'1','2')) OR ( EMPTY(ANTEBRAZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MANO,'1','2')) OR ( EMPTY(MANO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DEDOS_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEDOS_MANO,'1','2')) OR ( EMPTY(DEDOS_MANO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TORX_ANTER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TORX_ANTER,'1','2')) OR ( EMPTY(TORX_ANTER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TORX_POST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TORX_POST,'1','2')) OR ( EMPTY(TORX_POST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MAMAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAMAS,'1','2')) OR ( EMPTY(MAMAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ABDOMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ABDOMEN,'1','2')) OR ( EMPTY(ABDOMEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PELV_PERIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PELV_PERIN,'1','2')) OR ( EMPTY(PELV_PERIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_GENITALES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GENITALES,'1','2')) OR ( EMPTY(GENITALES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MUSLOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUSLOS,'1','2')) OR ( EMPTY(MUSLOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PIERNAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIERNAS,'1','2')) OR ( EMPTY(PIERNAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PIES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIES,'1','2')) OR ( EMPTY(PIES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DEDOS_PIES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEDOS_PIES,'1','2')) OR ( EMPTY(DEDOS_PIES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ORG_INTERN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ORG_INTERN,'1','2')) OR ( EMPTY(ORG_INTERN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PIEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIEL,'1','2')) OR ( EMPTY(PIEL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MAQUINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAQUINA,'1','2')) OR ( EMPTY(MAQUINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MEDIOS_TRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MEDIOS_TRA,'1','2')) OR ( EMPTY(MEDIOS_TRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_JUGUETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JUGUETES,'1','2')) OR ( EMPTY(JUGUETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ELECTR_ILU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ELECTR_ILU,'1','2')) OR ( EMPTY(ELECTR_ILU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_VEST_ACCES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VEST_ACCES,'1','2')) OR ( EMPTY(VEST_ACCES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_UTIL_ESCOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UTIL_ESCOL,'1','2')) OR ( EMPTY(UTIL_ESCOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_UTEN_COMED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UTEN_COMED,'1','2')) OR ( EMPTY(UTEN_COMED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ACC_INFAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACC_INFAN,'1','2')) OR ( EMPTY(ACC_INFAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EQU_DEPORT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EQU_DEPORT,'1','2')) OR ( EMPTY(EQU_DEPORT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ELECT_AUDI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ELECT_AUDI,'1','2')) OR ( EMPTY(ELECT_AUDI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_BELLEZA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BELLEZA,'1','2')) OR ( EMPTY(BELLEZA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MEDICAMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MEDICAMEN,'1','2')) OR ( EMPTY(MEDICAMEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_APAR_ESTET
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APAR_ESTET,'1','2')) OR ( EMPTY(APAR_ESTET)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EQU_BIOMED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EQU_BIOMED,'1','2')) OR ( EMPTY(EQU_BIOMED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_HOGAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HOGAR,'1','2')) OR ( EMPTY(HOGAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_COLEGIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COLEGIO,'1','2')) OR ( EMPTY(COLEGIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CALLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CALLE,'1','2')) OR ( EMPTY(CALLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PARQUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARQUE,'1','2')) OR ( EMPTY(PARQUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_INDUSTRIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INDUSTRIA,'1','2')) OR ( EMPTY(INDUSTRIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CENTR_ESTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CENTR_ESTE,'1','2')) OR ( EMPTY(CENTR_ESTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SPA,'1','2')) OR ( EMPTY(SPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_IPS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IPS,'1','2')) OR ( EMPTY(IPS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PROC_QUIRU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROC_QUIRU,'1','2','3','4')) OR ( EMPTY(PROC_QUIRU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TIPO_PROF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_PROF,'1','2','3','4','5','6')) OR ( EMPTY(TIPO_PROF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_HOSPITALIZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HOSPITALIZ,'1','2')) OR ( EMPTY(HOSPITALIZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_UCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UCI,'1','2')) OR ( EMPTY(UCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PROD_QUIMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROD_QUIMI,'1','2')) OR ( EMPTY(PROD_QUIMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ESTABLECIM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTABLECIM,'1','2')) OR ( EMPTY(ESTABLECIM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_GLUTEOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GLUTEOS,'1','2')) OR ( EMPTY(GLUTEOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


