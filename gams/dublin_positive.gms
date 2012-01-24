********************************************************************************
$ontext

   CAPRI project file

   GAMS file : dublin_positive.GMS

   @purpose  : scenario file for the publication:
               'Increasing volatility in EU agriculture'
               EAAE Seminar Dublin (2012)
               Positive increase in input prices
   @author   : Mihaly Himics (mihaly.himics@ec.europa.eu)
   @date     : 13.12.11
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
*********************************************************************************
*
* --- reference scenario
$include 'pol_input\mtr_rd.gms'



*1) regional definitions: EU countries, regions etc.

set eu_countries(rall) "countries that are subject of the analysis";

* only EU27, i.e. WBA, Turkey and Norway are excluded
* TODO: include non-EU countries???  --- R scripts needs to be modified accordingly

eu_countries(rall) $ sum(nuts0_eu15, sameas(rall, nuts0_eu15)) = yes;
eu_countries(rall) $ sum(nuts0_eu10, sameas(rall, nuts0_eu10)) = yes;
eu_countries(rall) $ sum(nuts0_bur , sameas(rall, nuts0_bur))  = yes;
display eu_countries, IY;





*$goto fertCosts
*2) directly yield dependent inputs -- linear part of the objective function

* --- data input from R result files
parameter p_tempDataIY(Rall, ROWS) 'temporary parameter for the coefficient of variation'
/
$ondelim
$include '..\inputc\inputc_cov.csv'
$offdelim
/;


DATA(eu_countries,"UVAG",IY,"PercentageChange") $ p_tempDataIY(eu_countries, IY) = p_tempDataIY(eu_countries, IY) * 100;
DATA(eu_countries,"UVAP",IY,"PercentageChange") = DATA(eu_countries,"UVAG",IY,"PercentageChange");

* map it to all regions in the current run (from MS to NUTS2)
DATA(RU,"UVAG",IY,"PercentageChange") = sum(eu_countries $ map_rr(eu_countries,RU), DATA(eu_countries,"UVAG",IY,"PercentageChange"));
DATA(RU,"UVAP",IY,"PercentageChange") = sum(eu_countries $ map_rr(eu_countries,RU), DATA(eu_countries,"UVAP",IY,"PercentageChange"));





*$goto skipfertCosts
$label fertCosts
*3) fertilizer costs -- defined as cost of nutrients

* --- data input from R result files
parameter p_tempDataFNUT(Rall, ROWS) 'temporary parameter for the coefficient of variation'
/
$ondelim
$include '..\inputc\fertilizers_cov.csv'
$offdelim
/;

*be aware: it only has a direct effect on mineral fertilizer costs
DATA(eu_countries,"UVAG",FNUT,"ChangeFactor") $ p_tempDataFNUT(eu_countries, FNUT) = 1 + p_tempDataFNUT(eu_countries, FNUT);

* map it to all regions in the current run (from MS to NUTS2)
DATA(RU,"UVAG",FNUT,"ChangeFactor") = sum(eu_countries $ map_rr(eu_countries,RU), DATA(eu_countries,"UVAG",FNUT,"ChangeFactor"));

$label skipfertCosts
*4) check if shocks came out
parameter p_ChangesToCheck(Rall,Rows,*) "shocks to be checked";
p_ChangesToCheck(Rall,FNUT,"") $ DATA(Rall,"UVAG",FNUT,"ChangeFactor")   = DATA(Rall,"UVAG",FNUT,"ChangeFactor");
p_ChangesToCheck(Rall,IY,"UVAG")   $ DATA(Rall,"UVAG",IY,"PercentageChange") = DATA(Rall,"UVAG",IY,"PercentageChange");
p_ChangesToCheck(Rall,IY,"UVAP")   $ DATA(Rall,"UVAP",IY,"PercentageChange") = DATA(Rall,"UVAP",IY,"PercentageChange");

display p_ChangesToCheck;
execute_unload "..\temp\%result_type%_scenAssumptions.gdx", p_ChangesToCheck;
*$stop

