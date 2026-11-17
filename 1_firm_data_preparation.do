/*

This file prepares firm-level data for later merge in sample_creation.do.

*/

version 13
clear all
capture log close

*Header do-file with path definitions, those end up in local macros
log using ${PATH_LOG}/firm_data_preparation.log, replace

use benr bundesland nace08_2 gueltig_von gueltig_bis using ${PATH_IN_RAW}/2022/dg.dta

drop if yofd(gueltig_bis) < 1980
replace gueltig_von = dofy(1980) if gueltig_von < dofy(1980)
replace gueltig_bis = dofy(2021) - 1 if gueltig_bis > dofy(2021) - 1
gen years = yofd(gueltig_bis) - yofd(gueltig_von) + 1
bysort benr (gueltig_von): gen spellindex = _n
expand years
bysort benr spellindex: gen contryear = yofd(gueltig_von) + _n - 1
gen yearlength = min(dofy(contryear+1)-1, gueltig_bis)-max(dofy(contryear), gueltig_von)
bysort benr contryear (yearlength): keep if _n == _N
drop gueltig_von gueltig_bis spellindex yearlength years
compress
*save_signature `"${PATH_DATA}/dg_full_merge.signature"'
save ${PATH_DATA}/dg_full_merge.dta, replace

log close
