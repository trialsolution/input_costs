$title converts capreg time series to an R-readable format

* --- settings
$setglobal projectfolder s:\inputc\input_costs\

* --- load in Capreg time series


*A) create filter for input costs variables

* the data parameter has 5 dimensions; here we specify filters to load the required subset
set years /1984*2009/;

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


* monetary inputs for seeds, plant protection, feeds, pharmaceutical inputs,
* repairs, ag. services, energy and other inputs --- as broken down by Capreg

* applied fertilizer quantities are calculated by Capreg as well


* Remarks:

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
        NITF,  PHOF , POTF

*       Feedingsstuff + young animals
*        FCER , FPRO , FENE , FMIL , FOTH , FGRA,  FCOM, FSGM, FMAI, FROO, FOFA, FSTR,
*        ICAM,  ICAF,  IHEI , ICOW , IPIG , IBUL,  ILAM

/;


* we only list the animal activites here for the young animal costs (see remark above)
set activities "agricultural activities of interets"/
*cropping activities

      SWHE   "Soft wheat production activity"
      DWHE   "Durum wheat production activity"
      RYEM   "Rye and meslin production activity"
      BARL   "Barley production activity"
      OATS   "Oats and summer cereal mixes production activity without triticale"
      MAIZ   "Grain maize production activity"
      OCER   "Other cereals production activity including triticale"

      RAPE   "Rape production activity"
      SUNF   "Sunflower production activity"
      SOYA   "Soya production activity"

      PARI   Paddy rice production activity,

      OLIV   Olive production activity for the oil industry
*
      PULS   Pulses production activity
      POTA   Potatoes production activity
      SUGB   Sugar beet production activity
      TEXT   Flax and hemp production activity
      TOBA   Tobacco production activity
*
      TOMA   Tomatoes production activity
      OVEG   Other vegetables production activity
      APPL   Apples  pears and peaches production activity
      OFRU   Other fruits production activity
      CITR   Citrus fruits production activity
      TAGR   Table grapes production activity
      TABO   Table olives production activity
      TWIN   Wine production activity
*
      FALL   Fallow land
*
*     Fodder production on arable land
*
      MAIF   "Fodder maize production activity"
      ROOF   "Fodder root crops production activity"
      OFAR   "Fodder other on arable land production activity"
*
*     Production on grassland
*
      GRAE   "Gras and grazings production activity extensive"
      GRAI   "Gras and grazings production activity intensive"


*endogenous animal activities

* different intensitites are not avilable in CoCo (only Capreg
      DCOL   Dairy cows production activity low yield
      DCOH   Dairy cows  production activityhigh yield
      BULL   Male adult fattening activity low final weight
      BULH   Male adult fattening activity high final weight
      HEIL   Heifers fattening activity low final weight
      HEIH   Heifers fattening activity high final weight
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

set dim2 "dimension two to be load in" /
set.unit_values
set.activities
/;


*B) define subset to be loaded later on
parameter CapregData(countries, dim2, cost_items, years);

*C) load data from gdx
$if not exist %projectfolder%data\res_time_series.gdx $abort 'Capreg data file not found'
*
execute_load '%projectfolder%data\res_time_series.gdx', CapregData=Data1;


* --- conversion to .csv
* using conversion tool from Wietse
$include '2csv_capreg.gms'

* --- unload set definitions
execute_unload '%projectfolder%gams\set_definitions.gdx', activities, unit_values, cost_items, countries;