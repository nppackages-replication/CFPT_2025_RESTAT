global path "YOUR_PATH"
global data "$path/data"
global raw "$path/raw"

********************************************************************************
* Create panel of countries
********************************************************************************

use "$raw/PTpanelRESTAT.dta", clear

replace countryname = "P.N.G." if countryname == "Papua New Guinea"
replace countryname = "C.A.R." if countryname == "Central African Republic"

drop if mi(liberalization) // drop countries for which we cannot classify the economic system
drop if mi(rgdp) // drop years for which we do not have gdp data
drop if year < 1963 // most countries have data available from 1963 onwards

* keep countries with the whole GDP series
bys countryname: gen observed = _N
drop if observed < 35

* adjust Kenya's libearalization date
replace liberalization = 0 if countryname == "Kenya" & year < 1993
replace liberalization = 1 if countryname == "Kenya" & year >= 1993

* drop if country's economy has been liberalized before 1968 
bys countryname liberalization: egen minT = min(year)
replace minT = . if !liberalization
bys countryname: egen trDate = max(minT)
drop if trDate < 1968

replace rgdp = rgdp/1000

replace countryname = "North Macedonia" if countryname == "Macedonia"
replace countryname = "Cabo Verde" if countryname == "Cape Verde"
merge m:1 countryname using "$raw/countryregion.dta"
drop if _merge < 3
drop _merge

bys countryname: egen aux = total(liberalization)
g treated = aux > 0

sort countryname year

keep countryname year liberalization rgdppp democracy trDate continent treated school2 inv_ratio inflation

save "$data/final_data.dta", replace

la var trDate "Year the country was liberalized"
la var continent "Continent"
la var treated "The country undergoes liberalization"

replace trDate = 9999 if mi(trDate)

save "$data/final_data.dta", replace
