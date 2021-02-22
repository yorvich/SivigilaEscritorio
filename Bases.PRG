CLOSE TABLES ALL
CLOSE DATABASES ALL

*19/06/2012 Se eliminó esta parte del código al corregir un error en la producción del reporte individual
*tabla_imp=_dir+'\datos_c'
*CREATE TABLE (tabla_imp)  FREE (nom_var c(20), dat_reg c(200), desc_var c(254))

SELECT 1
USE upgd ORDER 1

SELECT 2
USE tal_hum ORDER 1

SELECT 3
USE articulos ORDER 1

SELECT 4
USE ocupaciones ORDER 1

SELECT 5
USE deptos ORDER 1

SELECT 6
USE municipios ORDER 1

SELECT 7
USE eventos ORDER 1

SELECT 8
USE paciente ORDER 1

SELECT 9
USE entra_01 ORDER 1

SELECT 10
USE entra_02 ORDER 1

SELECT 11
USE entra_03 ORDER 1

SELECT 12
USE calendario ORDER 1

SELECT 13
USE aseguradoras ORDER 1

SELECT 14
USE cie10 ORDER 1

SELECT 15
USE BROTES ORDER 1

SELECT 17
USE paises ORDER 1

SELECT 34
USE laboratorios ORDER 1

SELECT 35
USE pruebas ORDER 1

SELECT 36
USE SECTORES ORDER 1

SELECT 37
USE agentes ORDER 1

SELECT 38
USE agentes_etas ORDER 1

SELECT 39
USE NOTIFICADOS ORDER 1

SELECT 40
USE UPGD_HAB ORDER 1

USE general IN 0
SELECT GENERAL	
_cod_pre=LEFT(UNI_NOT,10)
_cod_sub=RIGHT(UNI_NOT,2)
_raz_soc=ALLTRIM(raz_soc)

*Crea un cursor para efectuar las búsquedas de municipios para ser utilizado en todas las variables que capturan ese dato
DO listaMunicipiosConDepartamentos.qpr
INDEX ON COD_MUN TAG COD
INDEX ON NOM_MUN TAG NOM

*Crea un cursor para efectuar las búsquedas de los códigos de eventos de Brotes para ser utilizado;
en todas las variables que capturan ese dato
DO listaEventosBrotes.qpr
INDEX ON COD_EVE TAG COD
INDEX ON NOM_EVE TAG NOM

SET PROCEDURE TO EventsHandler ADDITIVE
SET PROCEDURE TO SivigilaMessenger ADDITIVE
SET PROCEDURE TO SivigilaUtilities ADDITIVE
SET PROCEDURE TO BusinessRules ADDITIVE
