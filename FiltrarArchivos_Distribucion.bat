cls
@echo off
(
FOR /F %%G IN (SivigilaDataFiles.txt) DO (
IF NOT %%G==sivigilaDataFiles.txt Del /f/q %%G
)
)

   