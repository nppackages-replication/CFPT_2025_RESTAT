*****************************************************************************************
* Replication file for Cattaneo, Feng, Palomba, and Titiunik (2025)
*
* This code replicates the main analysis of the paper using Stata and it serves for 
* illustrative purposes only. All the robustness exercises are conducted only in R.
*****************************************************************************************

*****************************************************************************************
** net install scpi, from(https://raw.githubusercontent.com/nppackages/scpi/master/stata) replace
*****************************************************************************************

global path "YOUR_PATH"
global data "$path/data"
global gphs "$path/fig/other"

* Load dataset
use "$data/final_data.dta", clear
gen lgdp = log(rgdppp)
global sims = 200
global scpi_version_checked = "ue"

keep if continent == "Africa"

* drop members of arab league
drop if inlist(countryname, "Algeria", "Egypt", "Libya", "Morocco", "Sudan")
drop if inlist(countryname, "Tunisia", "Djibouti", "Mauritania", "Somalia")


qui levelsof countryname if treated == 1 & trDate <= 1994 
global aux = r(levels)

global WaveAll
local i = 1
foreach unit of global aux {
	if `i' == 1 {
		global WaveAll "`unit'"
	} 
	else {
		global WaveAll "$WaveAll, `unit'" 		
	}
	local ++i
}

qui levelsof countryname if treated == 1 & trDate <= 1994 & countryname != "Mauritius"
global aux = r(levels)

global WaveAllRed
local i = 1
foreach unit of global aux {
	if `i' == 1 {
		global WaveAllRed "`unit'"
	} 
	else {
		global WaveAllRed "$WaveAllRed, `unit'" 		
	}
	local ++i
}


qui levelsof countryname if treated == 1 & trDate < 1987 & countryname != "Mauritius"
global aux = r(levels)

global Wave1
local i = 1
foreach unit of global aux {
	if `i' == 1 {
		global Wave1 "`unit'"
	} 
	else {
		global Wave1 "$Wave1, `unit'" 		
	}
	local ++i
}

qui levelsof countryname if treated == 1 & trDate >= 1987 & trDate <= 1991
global aux = r(levels)

global Wave2
local i = 1
foreach unit of global aux {
	if `i' == 1 {
		global Wave2 "`unit'"
	} 
	else {
		global Wave2 "$Wave2, `unit'" 		
	}
	local ++i
}

qui levelsof countryname if treated == 1 & trDate > 1991 & trDate <= 1994 
global aux = r(levels)

global Wave3
local i = 1
foreach unit of global aux {
	if `i' == 1 {
		global Wave3 "`unit'"
	} 
	else {
		global Wave3 "$Wave3, `unit'" 		
	}
	local ++i
}


*****************************************************
* TSUS \tau_{ik} - All countries
*****************************************************

scdatamulti lgdp lsc, dfname("python_scdatamulti") id(countryname) outcome(lgdp)       ///
			time(year) treatment(liberalization) cointegrated("True") constant("True") ///
			post_est(5) units_est($WaveAll) effect("unit-time")
			
scpi, dfname("python_scdatamulti") name("L1-L2") sims($sims) set_seed(8894)
scplotmulti, uncertainty("gaussian") ptype("series") yscalefree xscalefree ///
	gphcombineoptions(ytitle("(log) GDP per capita (USD thsd.)") xtitle("year"))
graph export "$gphs/Africa_WaveAll_indiv_L1-L2_stata.png", replace

*****************************************************
* TAUS \tau_{i.} - All countries
*****************************************************

scdatamulti lgdp lsc, dfname("python_scdatamulti") id(countryname) outcome(lgdp)       ///
			time(year) treatment(liberalization) cointegrated("True") constant("True") ///
			post_est(5) units_est($WaveAll) effect("unit")
			
scpi, dfname("python_scdatamulti") name("L1-L2") sims($sims) set_seed(8894)
scplotmulti, uncertainty("gaussian") ptype("series") yscalefree xscalefree ///
	gphcombineoptions(ytitle("(log) GDP per capita (USD thsd.)") xtitle("year"))
graph export "$gphs/Africa_WaveAll_unit_L1-L2_stata.png", replace

*****************************************************
* TSUA \tau_{.t} - All countries
*****************************************************

scdatamulti lgdp lsc, dfname("python_scdatamulti") id(countryname) outcome(lgdp)       ///
			time(year) treatment(liberalization) cointegrated("True") constant("True") ///
			post_est(5) units_est($WaveAllRed) effect("time")
			
scpi, dfname("python_scdatamulti") name("L1-L2") sims($sims) set_seed(8894)
scplotmulti, uncertainty("gaussian") ptype("series") yscalefree xscalefree ///
	gphcombineoptions(ytitle("(log) GDP per capita (USD thsd.)") xtitle("year"))
graph export "$gphs/Africa_WaveAll_time_L1-L2_stata.png", replace

*****************************************************
* TSUA \tau_{.t} - Before 1987
*****************************************************

scdatamulti lgdp lsc, dfname("python_scdatamulti") id(countryname) outcome(lgdp)       ///
			time(year) treatment(liberalization) cointegrated("True") constant("True") ///
			post_est(5) units_est($Wave1) effect("time")
			
scpi, dfname("python_scdatamulti") name("L1-L2") sims($sims) set_seed(8894)
scplotmulti, uncertainty("gaussian") ptype("series") yscalefree xscalefree ///
	gphcombineoptions(ytitle("(log) GDP per capita (USD thsd.)") xtitle("year"))
graph export "$gphs/Africa_Wave1_time_L1-L2_stata.png", replace

*****************************************************
* TSUA \tau_{.t} - Between 1987 and 1991
*****************************************************

scdatamulti lgdp lsc, dfname("python_scdatamulti") id(countryname) outcome(lgdp)       ///
			time(year) treatment(liberalization) cointegrated("True") constant("True") ///
			post_est(5) units_est($Wave2) effect("time")
			
scpi, dfname("python_scdatamulti") name("L1-L2") sims($sims) set_seed(8894)
scplotmulti, uncertainty("gaussian") ptype("series") yscalefree xscalefree ///
	gphcombineoptions(ytitle("(log) GDP per capita (USD thsd.)") xtitle("year"))
graph export "$gphs/Africa_Wave2_time_L1-L2_stata.png", replace


*****************************************************
* TSUA \tau_{.t} - After 1991
*****************************************************

scdatamulti lgdp lsc, dfname("python_scdatamulti") id(countryname) outcome(lgdp)       ///
			time(year) treatment(liberalization) cointegrated("True") constant("True") ///
			post_est(5) units_est($Wave3) effect("time")
			
scpi, dfname("python_scdatamulti") name("L1-L2") sims($sims) set_seed(8894)
scplotmulti, uncertainty("gaussian") ptype("series") yscalefree xscalefree ///
	gphcombineoptions(ytitle("(log) GDP per capita (USD thsd.)") xtitle("year"))
graph export "$gphs/Africa_Wave3_time_L1-L2_stata.png", replace
