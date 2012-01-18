$title converts coco data to an R-readable format

* --- settings
$setglobal projectfolder d:\User-DATA\capri\inputc\

* --- load in CoCo


*A) create filter for input costs variables

* the data parameter has 5 dimensions; here we specify filters to load the required subset
set years /1984*2010/;

set countries/
*EU15
       BL000000  "Belgium and Luxembourg"
       DK000000  "Denmark"
       DE000000  "Germany"
       EL000000  "Greece"
       ES000000  "Spain"
       FR000000  "France"
       IR000000  "Irland"
       IT000000  "Italy"
       NL000000  "The Netherlands"
       AT000000  "Austria"
       PT000000  "Portugal"
       SE000000  "Sweden"
       FI000000  "Finland"
       UK000000  "United Kingdom"

*EU10
    CY000000  "Cyprus"
    CZ000000  "Czech Republic"
    EE000000  "Estonia"
    HU000000  "Hungary"
    LT000000  "Lithuania"
    LV000000  "Latvia"
    MT000000  "Malta"
    PL000000  "Poland"
    SI000000  "Slovenia"
    SK000000  "Slovak Republic"

*EU2
    RO000000  "Romania"
    BG000000  "Bulgaria"
   /;


* time-series until 2010 for 'nowcasting' in captrd (see svn log)
set coco_stages "CoCo processing stage of interest" /'COCO1', 'COCO2'/;



* monetary inputs for fertilizer, seeds, plant protection, feeds, pharmaceutical inputs,
* repairs, ag. services, energy and other inputs --- as calculated in CoCo


* Remarks:
* -- young animal costs are defined for the animal activities
* -- other costs are given by member state (not by ag. activity)

set cost_items "cost items of interest"/

      CAOF   Calcium in fertiliser
*
*     *** Other crop specific
*
      SEED   Seed
      PLAP   Plant protection

*
*     *** General inputs
*
      REPM   Maintenance materials
      REPB   Maintenance buildings
      ELEC   Electricity
      EGAS   Heating gas and oil
      EFUL   Fuels
      ELUB   Lubricants
      WATR   Water balance or deficit
      INPO   Other inputs
      SERI   Services input

*     ***
      IPHA   Pharmaceutical inputs,


* --- intersectorally produced inputs
        NITF,  PHOF , POTF,
*       Feedingsstuff + young animals
        FCER , FPRO , FENE , FMIL , FOTH , FGRA,  FCOM, FSGM, FMAI, FROO, FOFA, FSTR,
        ICAM,  ICAF,  IHEI , ICOW , IPIG , IBUL,  ILAM

/;


* we only list the animal activites here for the young animal costs (see remark above)
set activities "agricultural activities of interets"/
*some animal activities with coco data
*
      SCOW   Suckler cows production activity
      HEIR   Heifers raising activity
      CAMF   Calves male fattening activity
      CAFF   Calves female fattening activity
      CAMR   Calves male raising activity
      CAFR   Calves female raising activity
*
      PIGF   Pig fattening activity
      SOWS   Sows for piglet production
*
      SHGM   Sheep and goats activity for milk production
      SHGF   Sheep and goats activity for fattening
*
      HENS   Laying hens production activity
      POUF   Poultry fattening activity
/;


set unit_values "unit value columns" /

*
*     *** Prices
*
      UVAP    Unit value EAA producer price
      UVAB    Unit value EAA basic price
      UVAG    Unit value EAA gross producer price
/;

set dim3 "dimension three to be load in" /
set.unit_values
set.activities
/;


*B) define subset to be loaded later on
parameter Data2(countries, coco_stages, dim3, cost_items, years);

*C) load data from gdx
$if not exist %projectfolder%data\coco2_output.gdx $abort 'CoCo data file not found'
*
execute_load '%projectfolder%data\coco2_output.gdx', DATA2;


* --- conversion to .csv
* using conversion tool from Wietse
$include '2csv.gms'


