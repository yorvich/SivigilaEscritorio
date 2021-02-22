set oldDir=%CD%
copy SivigilaDataFilesExtended.txt %1%
cd %1%
rar u backupSivigilaDataFiles @SivigilaDataFilesExtended.txt
CD %oldDir%