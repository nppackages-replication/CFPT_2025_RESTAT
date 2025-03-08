#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 23 16:28:10 2024

@author: fpalomba
"""

###################################################################
# Replication file for Cattaneo, Feng, Palomba, and Titiunik (2025)
# This code replicates the main analysis of the paper using Python
# and it serves for illustrative purposes only. All the robustness
# exercises are conducted only in R.
###################################################################

########################################
# Load modules
import pandas
import numpy
import os
from warnings import filterwarnings
from plotnine import theme

from scpi_pkg.scdataMulti import scdataMulti
from scpi_pkg.scpi import scpi
from scpi_pkg.scplotMulti import scplotMulti

# set seed, paths, number of simulations, and cores
filterwarnings("ignore")
numpy.random.seed(8894)

os.chdir('YOUR_PATH_HERE')

sims = 200
cores = 1

########################################
# Load database
data = pandas.read_stata('data/BNdata.dta')
data['lgdp'] = numpy.log(data['rgdppp'])
data['year'] = pandas.DatetimeIndex(data['year']).year  # from datetime64 to int

# remove countries not in africa and not in the arab league
data = data.loc[data['continent'] == 'Africa']
arab_league = ["Algeria", "Egypt", "Libya", "Morocco", "Sudan",
               "Tunisia", "Djibouti", "Mauritania", "Somalia"]
data = data[~data['countryname'].isin(arab_league)]

# prepare four main sets of treated units
WaveAll = data.query("treated == 1 and trDate <= 1994")["countryname"].unique().tolist()
Wave1 = data.query("treated == 1 and trDate < 1987")["countryname"].unique().tolist()
Wave2 = data.query("treated == 1 and 1987 <= trDate <= 1991")["countryname"].unique().tolist()
Wave3 = data.query("treated == 1 and 1991 < trDate <= 1994")["countryname"].unique().tolist()

units_dict = {'WaveAll': WaveAll,
              'Wave1': Wave1,
              'Wave2': Wave2,
              'Wave3': Wave3
              }

###############################################
# TSUS \tau_{ik} - All countries
###############################################
aux = scdataMulti(df=data,
                  id_var='countryname',
                  treatment_var='liberalization',
                  outcome_var='lgdp',
                  time_var='year',
                  constant=True,
                  cointegrated_data=True,
                  post_est=5,
                  units_est=units_dict["WaveAll"],
                  effect='unit-time')

res = scpi(aux, w_constr={'name': 'L1-L2'}, sims=sims,
           u_order=1, u_lags=1, cores=cores)
p = scplotMulti(res, ptype='series', scales='free', ncols=4)
p = p + theme(legend_position='bottom')
plotname = 'fig/other/Africa_WaveAll_indiv_L1-L2_py.png'
p.save(plotname)

###############################################
# TAUS \tau_{i.} - All countries
###############################################
aux = scdataMulti(df=data,
                  id_var='countryname',
                  treatment_var='liberalization',
                  outcome_var='lgdp',
                  time_var='year',
                  constant=True,
                  cointegrated_data=True,
                  post_est=5,
                  units_est=units_dict["WaveAll"],
                  effect='unit')

res = scpi(aux, w_constr={'name': 'L1-L2'}, sims=sims,
           u_order=1, u_lags=1, cores=cores)
p = scplotMulti(res, ptype='series', scales='free', ncols=4)
p = p + theme(legend_position='bottom')
plotname = 'fig/other/Africa_WaveAll_unit_L1-L2_py.png'
p.save(plotname)

###############################################
# TSUA \tau_{.t} - All countries
###############################################
treated_units_time = [tr for tr in units_dict["WaveAll"] if tr != "Mauritius"]

aux = scdataMulti(df=data,
                  id_var='countryname',
                  treatment_var='liberalization',
                  outcome_var='lgdp',
                  time_var='year',
                  constant=True,
                  cointegrated_data=True,
                  post_est=5,
                  units_est=treated_units_time,
                  effect='time')

res = scpi(aux, w_constr={'name': 'L1-L2'}, sims=sims,
           u_order=1, u_lags=1, cores=cores)
p = scplotMulti(res, ptype='series', scales='free', joint=True)
p = p + theme(legend_position='bottom')
plotname = 'fig/other/Africa_WaveAll_time_L1-L2_py.png'
p.save(plotname)

###############################################
# TSUA \tau_{.t} - Before 1991
###############################################
treated_units_time = [tr for tr in units_dict["Wave1"] if tr != "Mauritius"]

aux = scdataMulti(df=data,
                  id_var='countryname',
                  treatment_var='liberalization',
                  outcome_var='lgdp',
                  time_var='year',
                  constant=True,
                  cointegrated_data=True,
                  post_est=5,
                  units_est=treated_units_time,
                  effect='time')

res = scpi(aux, w_constr={'name': 'L1-L2'}, sims=sims,
           u_order=1, u_lags=1, cores=cores)
p = scplotMulti(res, ptype='series', scales='free', joint=True)
p = p + theme(legend_position='bottom')
plotname = 'fig/other/Africa_Wave1_time_L1-L2_py.png'
p.save(plotname)

###############################################
# TSUA \tau_{.t} - Between 1987 and 1991
###############################################
aux = scdataMulti(df=data,
                  id_var='countryname',
                  treatment_var='liberalization',
                  outcome_var='lgdp',
                  time_var='year',
                  constant=True,
                  cointegrated_data=True,
                  post_est=5,
                  units_est=units_dict["Wave2"],
                  effect='time')

res = scpi(aux, w_constr={'name': 'L1-L2'}, sims=sims,
           u_order=1, u_lags=1, cores=cores)
p = scplotMulti(res, ptype='series', scales='free', joint=True)
p = p + theme(legend_position='bottom')
plotname = 'fig/other/Africa_Wave2_time_L1-L2_py.png'
p.save(plotname)

###############################################
# TSUA \tau_{.t} - After 1991
###############################################
aux = scdataMulti(df=data,
                  id_var='countryname',
                  treatment_var='liberalization',
                  outcome_var='lgdp',
                  time_var='year',
                  constant=True,
                  cointegrated_data=True,
                  post_est=5,
                  units_est=units_dict["Wave3"],
                  effect='time')

res = scpi(aux, w_constr={'name': 'L1-L2'}, sims=sims,
           u_order=1, u_lags=1, cores=cores)
p = scplotMulti(res, ptype='series', scales='free', joint=True)
p = p + theme(legend_position='bottom')
plotname = 'fig/other/Africa_Wave3_time_L1-L2_py.png'
p.save(plotname)
