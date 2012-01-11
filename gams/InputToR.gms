*  Conversion of GDX and GDX to CSV conversion

* temporary file with the pre-defined parameter
execute_unload "GAMSandR.gdx",Data2;

* --- Convert gdx file in csv file for R (executable csv_gdx_tools.exe, see: manual Metabase, page 63)

$onecho  >  "%system.fp%parameters.txt"
GAMSandR.GDX
no_gref
Data2
%projectfolder%data/Data2.csv
/layout=0.######
/indexquote=yes
/valuequote=no
/header=yes
/headernames="Country,Stage,dim3,CostItem,Year"
/delimiter=","
/outputtype=0
/outputsep=" - "
/method=gdxcsv
/CROSSTAB=Year
/crosstype=0
/CROSSSEP=" - "
$offecho

execute '=%MetabaseTools%csv_gdx_tools.exe @"%system.fp%parameters.txt"'
*============================   End Of File   ================================
