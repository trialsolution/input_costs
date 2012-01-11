* main file of gdx to csv conversion
* credits go to Wietse Dol

*=============================================================================
$include 2csv_settings.gms


* --- Create CSV data for R batch process

display "Create CSV data for R batch process"
$include "InputToR_Capreg.gms"


display "Cleanup files"
$include  'Cleanup_conversion.gms'

*============================   End Of File   ================================
