#INCLUDE SIVIGILA.H

DEFINE CLASS SivigilaIniMgr as CUSTOM
	
	PROCEDURE init()
		DO iniMgr WITH SIVIGILA_INI_FILE_NAME,.T. 
		IF EMPTY(goIniMgr.GetIniFile()) THEN
			RELEASE goIniMgr
			DO iniMgr WITH SIVIGILA_INI_FILE_NAME
			
			* Entradas por defecto para SIVIRips
			goIniMgr.SetValue(SIVIRIPS_SECTION_NAME,"TITLE",SIVIRIPS_SECTION_NAME)
			goIniMgr.SetValue(SIVIRIPS_SECTION_NAME,"EXE_FILE_NAME_AND_PATH","Undefined")	
					
			*Entradas por defecto para EPIMAP
			goIniMgr.SetValue(EPIMAP_SECTION_NAME,"EXE_FILE_NAME_AND_PATH","C:\EPI_INFO\EPIMAP.EXE")
		ENDIF
		THIS.checkDefaultSivigilaRelatedApps()

	ENDPROC
	
	PROCEDURE checkDefaultSivigilaRelatedApps
		IF EMPTY(goIniMgr.GetValue(SIVIRIPS_SECTION_NAME,"EXE_FILE_NAME_AND_PATH"))
			goIniMgr.SetValue(SIVIRIPS_SECTION_NAME,"TITLE",SIVIRIPS_SECTION_NAME)
			goIniMgr.SetValue(SIVIRIPS_SECTION_NAME,"EXE_FILE_NAME_AND_PATH","Undefined")			
			* Entradas por defecto para SIVIRips
		ENDIF
		IF EMPTY(goIniMgr.GetValue(EPIMAP_SECTION_NAME,"EXE_FILE_NAME_AND_PATH"))
			goIniMgr.SetValue(EPIMAP_SECTION_NAME,"EXE_FILE_NAME_AND_PATH","C:\EPI_INFO\EPIMAP.EXE")
			*Entradas por defecto para EPIMAP
		ENDIF
		IF EMPTY(goIniMgr.GetValue(WEBSITES_SECTION_NAME,"URL"))
			goIniMgr.SetValue(WEBSITES_SECTION_NAME,"URL","https://docs.google.com/spreadsheets/d/1wO2G7m55M4O46tnRmIAPfS4t-cq5224WGNmekwT5A38/edit?usp=sharing")
			*Entradas por defecto para sitio web del listado de UPGDs del INS
		ENDIF
	ENDPROC
	
	PROCEDURE destroy()
		RELEASE goIniMgr
	ENDPROC
	
	
ENDDEFINE
