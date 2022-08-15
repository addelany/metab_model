library(tidyverse)
library(ggplot2)
library(zoo)

## CREATE FIGURES FOR THESIS

#MENDOTA
me_epi_vars <- read_csv('~/output/nhdhr_143249470/epi_vars.csv')
me_hypo_vars <- read_csv('~/output/nhdhr_143249470/hypo_vars.csv')
me_epi_fluxes <- read_csv('~/output/nhdhr_143249470/epi_fluxes.csv')
me_hypo_fluxes <- read_csv('~/output/nhdhr_143249470/hypo_fluxes.csv')
me_conc_vars <- read_csv('~/output/nhdhr_143249470/conc_vars.csv')
me_driving <- read_csv('~/output/nhdhr_143249470/driving_info.csv')

me_do_epi_obs <- read_csv('~/output/nhdhr_143249470/obs_data/do_epi_obs.csv')
me_do_hypo_obs <- read_csv('~/output/nhdhr_143249470/obs_data/do_hypo_obs.csv')
me_doc_epi_obs <- read_csv('~/output/nhdhr_143249470/obs_data/doc_epi_obs.csv')
me_doc_hypo_obs <- read_csv('~/output/nhdhr_143249470/obs_data/doc_hypo_obs.csv')
me_secchi_obs <- read_csv('~/output/nhdhr_143249470/obs_data/secchi_obs.csv')
me_tp <- read_csv('~/output/nhdhr_143249470/obs_data/tp_interp_obs.csv')

me_epi_vars_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_143249470/epi_vars.csv')
me_hypo_vars_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_143249470/hypo_vars.csv')
me_epi_fluxes_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_143249470/epi_fluxes.csv')
me_hypo_fluxes_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_143249470/hypo_fluxes.csv')

me_epi_vars_uncertainty <- cbind(me_driving$sampledate,me_epi_vars_uncertainty) %>% rename(sampledate = `me_driving$sampledate`)
me_hypo_vars_uncertainty <- cbind(me_driving$sampledate,me_hypo_vars_uncertainty) %>% rename(sampledate = `me_driving$sampledate`)
me_epi_fluxes_uncertainty <- cbind(me_driving$sampledate,me_epi_fluxes_uncertainty) %>% rename(sampledate = `me_driving$sampledate`)
me_hypo_fluxes_uncertainty <- cbind(me_driving$sampledate,me_hypo_fluxes_uncertainty) %>% rename(sampledate = `me_driving$sampledate`)

# MONONA 
mo_epi_vars <- read_csv('~/output/nhdhr_143249640/epi_vars.csv')#stringsAsFactors = FALSE)
mo_hypo_vars <- read_csv('~/output/nhdhr_143249640/hypo_vars.csv')
mo_epi_fluxes <- read_csv('~/output/nhdhr_143249640/epi_fluxes.csv')
mo_hypo_fluxes <- read_csv('~/output/nhdhr_143249640/hypo_fluxes.csv')
mo_conc_vars <- read_csv('~/output/nhdhr_143249640/conc_vars.csv')
mo_driving <- read_csv('~/output/nhdhr_143249640/driving_info.csv')

mo_do_epi_obs <- read_csv('~/output/nhdhr_143249640/obs_data/do_epi_obs.csv')
mo_do_hypo_obs <- read_csv('~/output/nhdhr_143249640/obs_data/do_hypo_obs.csv')
mo_doc_epi_obs <- read_csv('~/output/nhdhr_143249640/obs_data/doc_epi_obs.csv')
mo_doc_hypo_obs <- read_csv('~/output/nhdhr_143249640/obs_data/doc_hypo_obs.csv')
mo_secchi_obs <- read_csv('~/output/nhdhr_143249640/obs_data/secchi_obs.csv')
mo_tp <- read_csv('~/output/nhdhr_143249640/obs_data/tp_interp_obs.csv')


# mo_epi_vars_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_143249640/epi_vars.csv')
# mo_hypo_vars_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_143249640/hypo_vars.csv')
# mo_epi_fluxes_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_143249640/epi_fluxes.csv')
# mo_hypo_fluxes_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_143249640/hypo_fluxes.csv')

mo_epi_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_143249640/epi_vars.csv')
mo_hypo_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_143249640/hypo_vars.csv')
mo_epi_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_143249640/epi_fluxes.csv')
mo_hypo_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_143249640/hypo_fluxes.csv')

mo_epi_vars_uncertainty <- cbind(mo_driving$sampledate,mo_epi_vars_uncertainty) %>% rename(sampledate = `mo_driving$sampledate`)
mo_hypo_vars_uncertainty <- cbind(mo_driving$sampledate,mo_hypo_vars_uncertainty) %>% rename(sampledate = `mo_driving$sampledate`)
mo_epi_fluxes_uncertainty <- cbind(mo_driving$sampledate,mo_epi_fluxes_uncertainty) %>% rename(sampledate = `mo_driving$sampledate`)
mo_hypo_fluxes_uncertainty <- cbind(mo_driving$sampledate,mo_hypo_fluxes_uncertainty) %>% rename(sampledate = `mo_driving$sampledate`)

mo_driving <- mo_driving %>% filter(sampledate < '2015-01-01')
# ALLEQUASH  
al_epi_vars <- read_csv('~/output/nhdhr_69886156/epi_vars.csv')#stringsAsFactors = FALSE)
al_hypo_vars <- read_csv('~/output/nhdhr_69886156/hypo_vars.csv')
al_epi_fluxes <- read_csv('~/output/nhdhr_69886156/epi_fluxes.csv')
al_hypo_fluxes <- read_csv('~/output/nhdhr_69886156/hypo_fluxes.csv')
al_conc_vars <- read_csv('~/output/nhdhr_69886156/conc_vars.csv')
al_driving <- read_csv('~/output/nhdhr_69886156/driving_info.csv')

al_do_epi_obs <- read_csv('~/output/nhdhr_69886156/obs_data/do_epi_obs.csv')
al_do_hypo_obs <- read_csv('~/output/nhdhr_69886156/obs_data/do_hypo_obs.csv')
al_doc_epi_obs <- read_csv('~/output/nhdhr_69886156/obs_data/doc_epi_obs.csv')
al_doc_hypo_obs <- read_csv('~/output/nhdhr_69886156/obs_data/doc_hypo_obs.csv')
al_secchi_obs <- read_csv('~/output/nhdhr_69886156/obs_data/secchi_obs.csv')
al_tp <- read_csv('~/output/nhdhr_69886156/obs_data/tp_interp_obs.csv')

# al_epi_vars_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_69886156/epi_vars.csv')
# al_hypo_vars_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_69886156/hypo_vars.csv')
# al_epi_fluxes_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_69886156/epi_fluxes.csv')
# al_hypo_fluxes_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_69886156/hypo_fluxes.csv')

al_epi_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886156/epi_vars.csv')
al_hypo_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886156/hypo_vars.csv')
al_epi_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886156/epi_fluxes.csv')
al_hypo_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886156/hypo_fluxes.csv')

al_epi_vars_uncertainty <- cbind(al_driving$sampledate,al_epi_vars_uncertainty) %>% rename(sampledate = `al_driving$sampledate`)
al_hypo_vars_uncertainty <- cbind(al_driving$sampledate,al_hypo_vars_uncertainty) %>% rename(sampledate = `al_driving$sampledate`)
al_epi_fluxes_uncertainty <- cbind(al_driving$sampledate,al_epi_fluxes_uncertainty) %>% rename(sampledate = `al_driving$sampledate`)
al_hypo_fluxes_uncertainty <- cbind(al_driving$sampledate,al_hypo_fluxes_uncertainty) %>% rename(sampledate = `al_driving$sampledate`)


# TROUT 
tr_epi_vars <- read_csv('~/output/nhdhr_69886228/epi_vars.csv')#stringsAsFactors = FALSE)
tr_hypo_vars <- read_csv('~/output/nhdhr_69886228/hypo_vars.csv')
tr_epi_fluxes <- read_csv('~/output/nhdhr_69886228/epi_fluxes.csv')
tr_hypo_fluxes <- read_csv('~/output/nhdhr_69886228/hypo_fluxes.csv')
tr_conc_vars <- read_csv('~/output/nhdhr_69886228/conc_vars.csv')
tr_driving <- read_csv('~/output/nhdhr_69886228/driving_info.csv')

tr_do_epi_obs <- read_csv('~/output/nhdhr_69886228/obs_data/do_epi_obs.csv')
tr_do_hypo_obs <- read_csv('~/output/nhdhr_69886228/obs_data/do_hypo_obs.csv')
tr_doc_epi_obs <- read_csv('~/output/nhdhr_69886228/obs_data/doc_epi_obs.csv')
tr_doc_hypo_obs <- read_csv('~/output/nhdhr_69886228/obs_data/doc_hypo_obs.csv')
tr_secchi_obs <- read_csv('~/output/nhdhr_69886228/obs_data/secchi_obs.csv')
tr_tp <- read_csv('~/output/nhdhr_69886228/obs_data/tp_interp_obs.csv')

tr_epi_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886228/epi_vars.csv')
tr_hypo_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886228/hypo_vars.csv')
tr_epi_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886228/epi_fluxes.csv')
tr_hypo_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886228/hypo_fluxes.csv')

tr_epi_vars_uncertainty <- cbind(tr_driving$sampledate,tr_epi_vars_uncertainty) %>% rename(sampledate = `tr_driving$sampledate`)
tr_hypo_vars_uncertainty <- cbind(tr_driving$sampledate,tr_hypo_vars_uncertainty) %>% rename(sampledate = `tr_driving$sampledate`)
tr_epi_fluxes_uncertainty <- cbind(tr_driving$sampledate,tr_epi_fluxes_uncertainty) %>% rename(sampledate = `tr_driving$sampledate`)
tr_hypo_fluxes_uncertainty <- cbind(tr_driving$sampledate,tr_hypo_fluxes_uncertainty) %>% rename(sampledate = `tr_driving$sampledate`)


# BIG MUSKIE 
bm_epi_vars <- read_csv('~/output/nhdhr_69886284/epi_vars.csv')#stringsAsFactors = FALSE)
bm_hypo_vars <- read_csv('~/output/nhdhr_69886284/hypo_vars.csv')
bm_epi_fluxes <- read_csv('~/output/nhdhr_69886284/epi_fluxes.csv')
bm_hypo_fluxes <- read_csv('~/output/nhdhr_69886284/hypo_fluxes.csv')
bm_conc_vars <- read_csv('~/output/nhdhr_69886284/conc_vars.csv')
bm_driving <- read_csv('~/output/nhdhr_69886284/driving_info.csv')

bm_do_epi_obs <- read_csv('~/output/nhdhr_69886284/obs_data/do_epi_obs.csv')
bm_do_hypo_obs <- read_csv('~/output/nhdhr_69886284/obs_data/do_hypo_obs.csv')
bm_doc_epi_obs <- read_csv('~/output/nhdhr_69886284/obs_data/doc_epi_obs.csv')
bm_doc_hypo_obs <- read_csv('~/output/nhdhr_69886284/obs_data/doc_hypo_obs.csv')
bm_secchi_obs <- read_csv('~/output/nhdhr_69886284/obs_data/secchi_obs.csv')
bm_tp <- read_csv('~/output/nhdhr_69886284/obs_data/tp_interp_obs.csv')

# bm_epi_vars_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_69886284/epi_vars.csv')
# bm_hypo_vars_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_69886284/hypo_vars.csv')
# bm_epi_fluxes_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_69886284/epi_fluxes.csv')
# bm_hypo_fluxes_uncertainty <- read_csv('~/optimization/uncertainty/nhdhr_69886284/hypo_fluxes.csv')

bm_epi_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886284/epi_vars.csv')
bm_hypo_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886284/hypo_vars.csv')
bm_epi_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886284/epi_fluxes.csv')
bm_hypo_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886284/hypo_fluxes.csv')

#add dates
bm_epi_vars_uncertainty <- cbind(bm_driving$sampledate,bm_epi_vars_uncertainty) %>% rename(sampledate = `bm_driving$sampledate`)
bm_hypo_vars_uncertainty <- cbind(bm_driving$sampledate,bm_hypo_vars_uncertainty) %>% rename(sampledate = `bm_driving$sampledate`)
bm_epi_fluxes_uncertainty <- cbind(bm_driving$sampledate,bm_epi_fluxes_uncertainty) %>% rename(sampledate = `bm_driving$sampledate`)
bm_hypo_fluxes_uncertainty <- cbind(bm_driving$sampledate,bm_hypo_fluxes_uncertainty) %>% rename(sampledate = `bm_driving$sampledate`)


# SPARKLING
sp_epi_vars <- read_csv('~/output/nhdhr_69886444/epi_vars.csv')#stringsAsFactors = FALSE)
sp_hypo_vars <- read_csv('~/output/nhdhr_69886444/hypo_vars.csv')
sp_epi_fluxes <- read_csv('~/output/nhdhr_69886444/epi_fluxes.csv')
sp_hypo_fluxes <- read_csv('~/output/nhdhr_69886444/hypo_fluxes.csv')
sp_conc_vars <- read_csv('~/output/nhdhr_69886444/conc_vars.csv')
sp_driving <- read_csv('~/output/nhdhr_69886444/driving_info.csv')

sp_do_epi_obs <- read_csv('~/output/nhdhr_69886444/obs_data/do_epi_obs.csv')
sp_do_hypo_obs <- read_csv('~/output/nhdhr_69886444/obs_data/do_hypo_obs.csv')
sp_doc_epi_obs <- read_csv('~/output/nhdhr_69886444/obs_data/doc_epi_obs.csv')
sp_doc_hypo_obs <- read_csv('~/output/nhdhr_69886444/obs_data/doc_hypo_obs.csv')
sp_secchi_obs <- read_csv('~/output/nhdhr_69886444/obs_data/secchi_obs.csv')
sp_tp <- read_csv('~/output/nhdhr_69886444/obs_data/tp_interp_obs.csv')

sp_epi_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886444/epi_vars.csv')
sp_hypo_vars_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886444/hypo_vars.csv')
sp_epi_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886444/epi_fluxes.csv')
sp_hypo_fluxes_uncertainty <- read_csv('~/optimization/uncertainty_test/nhdhr_69886444/hypo_fluxes.csv')

#add dates
sp_epi_vars_uncertainty <- cbind(sp_driving$sampledate,sp_epi_vars_uncertainty) %>% rename(sampledate = `sp_driving$sampledate`)
sp_hypo_vars_uncertainty <- cbind(sp_driving$sampledate,sp_hypo_vars_uncertainty) %>% rename(sampledate = `sp_driving$sampledate`)
sp_epi_fluxes_uncertainty <- cbind(sp_driving$sampledate,sp_epi_fluxes_uncertainty) %>% rename(sampledate = `sp_driving$sampledate`)
sp_hypo_fluxes_uncertainty <- cbind(sp_driving$sampledate,sp_hypo_fluxes_uncertainty) %>% rename(sampledate = `sp_driving$sampledate`)

##state variables over time (6-panel plots)
#DO
#Mendota
me_do_epi_obs_short <- me_do_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_do_hypo_obs_short <- me_do_hypo_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_doc_epi_obs_short <- me_doc_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_secchi_obs_short <- me_secchi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_epi_vars_short <- me_epi_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_hypo_vars_short <- me_hypo_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_conc_vars_short <- me_conc_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_driving_short <- me_driving %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_hypo_driving_short <- me_driving %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

me_epi_vars_uncertainty_short <- me_epi_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_hypo_vars_uncertainty_short <- me_hypo_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_epi_fluxes_uncertainty_short <- me_epi_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
me_hypo_fluxes_uncertainty_short <- me_hypo_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

#Monona
mo_do_epi_obs_short <- mo_do_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_do_hypo_obs_short <- mo_do_hypo_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_doc_epi_obs_short <- mo_doc_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_secchi_obs_short <- mo_secchi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_epi_vars_short <- mo_epi_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_hypo_vars_short <- mo_hypo_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_conc_vars_short <- mo_conc_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_driving_short <- mo_driving %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

mo_epi_vars_uncertainty_short <- mo_epi_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_hypo_vars_uncertainty_short <- mo_hypo_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_epi_fluxes_uncertainty_short <- mo_epi_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
mo_hypo_fluxes_uncertainty_short <- mo_hypo_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

#Allequash
al_do_epi_obs_short <- al_do_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_do_hypo_obs_short <- al_do_hypo_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_doc_epi_obs_short <- al_doc_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_secchi_obs_short <- al_secchi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_epi_vars_short <- al_epi_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_hypo_vars_short <- al_hypo_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_conc_vars_short <- al_conc_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_driving_short <- al_driving %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

al_epi_vars_uncertainty_short <- al_epi_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_hypo_vars_uncertainty_short <- al_hypo_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_epi_fluxes_uncertainty_short <- al_epi_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
al_hypo_fluxes_uncertainty_short <- al_hypo_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

#Trout
tr_do_epi_obs_short <- tr_do_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_do_hypo_obs_short <- tr_do_hypo_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_doc_epi_obs_short <- tr_doc_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_secchi_obs_short <- tr_secchi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_epi_vars_short <- tr_epi_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_hypo_vars_short <- tr_hypo_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_conc_vars_short <- tr_conc_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_driving_short <- tr_driving %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

tr_epi_vars_uncertainty_short <- tr_epi_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_hypo_vars_uncertainty_short <- tr_hypo_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_epi_fluxes_uncertainty_short <- tr_epi_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
tr_hypo_fluxes_uncertainty_short <- tr_hypo_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')


#Big Muskie
bm_do_epi_obs_short <- bm_do_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_do_hypo_obs_short <- bm_do_hypo_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_doc_epi_obs_short <- bm_doc_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_secchi_obs_short <- bm_secchi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_epi_vars_short <- bm_epi_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_hypo_vars_short <- bm_hypo_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_conc_vars_short <- bm_conc_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_driving_short <- bm_driving %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

bm_epi_vars_uncertainty_short <- bm_epi_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_hypo_vars_uncertainty_short <- bm_hypo_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_epi_fluxes_uncertainty_short <- bm_epi_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
bm_hypo_fluxes_uncertainty_short <- bm_hypo_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

#Sparkling
sp_do_epi_obs_short <- sp_do_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_do_hypo_obs_short <- sp_do_hypo_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_doc_epi_obs_short <- sp_doc_epi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_secchi_obs_short <- sp_secchi_obs %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_epi_vars_short <- sp_epi_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_hypo_vars_short <- sp_hypo_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_conc_vars_short <- sp_conc_vars %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_driving_short <- sp_driving %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')

sp_epi_vars_uncertainty_short <- sp_epi_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_hypo_vars_uncertainty_short <- sp_hypo_vars_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_epi_fluxes_uncertainty_short <- sp_epi_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')
sp_hypo_fluxes_uncertainty_short <- sp_hypo_fluxes_uncertainty %>% filter(sampledate < '2010-01-01') %>% filter(sampledate > '2004-12-31')


min_date_year <-  '2005-01-01' 
max_date_year <- '2005-12-30'

find_dates <- function(date_min,date_max,driving_df){
interest_year <- driving_df %>% 
  filter(sampledate < date_max) %>% 
  filter(sampledate > date_min) %>% 
  filter(!is.na(thermocline_depth))

min_day <- interest_year$sampledate[1]
max_day <- interest_year$sampledate[nrow(interest_year)]
date_list <- c(min_day,max_day)
return(date_list)
} 

#ME
me_dates <- find_dates(min_date_year,max_date_year,me_driving)
me_epi_vars_year <- me_epi_vars %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])
me_hypo_vars_year <- me_hypo_vars %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])
me_hypo_vars_year$do_value <- ifelse(me_hypo_vars_year$do_value == 0,NA,me_hypo_vars_year$do_value)

me_epi_fluxes_year <- me_epi_fluxes %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])
me_hypo_fluxes_year <- me_hypo_fluxes %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])

me_do_obs_epi_year <- me_do_epi_obs %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])
me_do_obs_hypo_year <- me_do_hypo_obs %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])

me_driving_year <- me_driving %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])

me_epi_vars_uncertainty_year <- me_epi_vars_uncertainty %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])
me_hypo_vars_uncertainty_year <- me_hypo_vars_uncertainty %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])
me_epi_fluxes_uncertainty_year <- me_epi_fluxes_uncertainty %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])
me_hypo_fluxes_uncertainty_year <- me_hypo_fluxes_uncertainty %>% filter(sampledate < me_dates[2]) %>% filter(sampledate > me_dates[1])

#MO
mo_dates <- find_dates(min_date_year,max_date_year,mo_driving)
mo_epi_vars_year <- mo_epi_vars %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])
mo_hypo_vars_year <- mo_hypo_vars %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])
mo_hypo_vars_year$do_value <- ifelse(mo_hypo_vars_year$do_value == 0,NA,mo_hypo_vars_year$do_value)

mo_epi_fluxes_year <- mo_epi_fluxes %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])
mo_hypo_fluxes_year <- mo_hypo_fluxes %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])

mo_do_obs_epi_year <- mo_do_epi_obs %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])
mo_do_obs_hypo_year <- mo_do_hypo_obs %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])

mo_driving_year <- mo_driving %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])

mo_epi_vars_uncertainty_year <- mo_epi_vars_uncertainty %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])
mo_hypo_vars_uncertainty_year <- mo_hypo_vars_uncertainty %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])
mo_epi_fluxes_uncertainty_year <- mo_epi_fluxes_uncertainty %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])
mo_hypo_fluxes_uncertainty_year <- mo_hypo_fluxes_uncertainty %>% filter(sampledate < mo_dates[2]) %>% filter(sampledate > mo_dates[1])

#AL
al_dates <- find_dates(min_date_year,max_date_year,al_driving)
al_epi_vars_year <- al_epi_vars %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])
al_hypo_vars_year <- al_hypo_vars %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])
al_hypo_vars_year$do_value <- ifelse(al_hypo_vars_year$do_value == 0,NA,al_hypo_vars_year$do_value)

al_epi_fluxes_year <- al_epi_fluxes %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])
al_hypo_fluxes_year <- al_hypo_fluxes %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])

al_do_obs_epi_year <- al_do_epi_obs %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])
al_do_obs_hypo_year <- al_do_hypo_obs %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])

al_driving_year <- al_driving %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])

al_epi_vars_uncertainty_year <- al_epi_vars_uncertainty %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])
al_hypo_vars_uncertainty_year <- al_hypo_vars_uncertainty %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])
al_epi_fluxes_uncertainty_year <- al_epi_fluxes_uncertainty %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])
al_hypo_fluxes_uncertainty_year <- al_hypo_fluxes_uncertainty %>% filter(sampledate < al_dates[2]) %>% filter(sampledate > al_dates[1])

#TR
tr_dates <- find_dates(min_date_year,max_date_year,tr_driving)
tr_epi_vars_year <- tr_epi_vars %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])
tr_hypo_vars_year <- tr_hypo_vars %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])
tr_hypo_vars_year$do_value <- ifelse(tr_hypo_vars_year$do_value == 0,NA,tr_hypo_vars_year$do_value)

tr_epi_fluxes_year <- tr_epi_fluxes %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])
tr_hypo_fluxes_year <- tr_hypo_fluxes %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])

tr_do_obs_epi_year <- tr_do_epi_obs %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])
tr_do_obs_hypo_year <- tr_do_hypo_obs %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])

tr_driving_year <- tr_driving %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])

tr_epi_vars_uncertainty_year <- tr_epi_vars_uncertainty %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])
tr_hypo_vars_uncertainty_year <- tr_hypo_vars_uncertainty %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])
tr_epi_fluxes_uncertainty_year <- tr_epi_fluxes_uncertainty %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])
tr_hypo_fluxes_uncertainty_year <- tr_hypo_fluxes_uncertainty %>% filter(sampledate < tr_dates[2]) %>% filter(sampledate > tr_dates[1])

#BM
bm_dates <- find_dates(min_date_year,max_date_year,bm_driving)
bm_epi_vars_year <- bm_epi_vars %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])
bm_hypo_vars_year <- bm_hypo_vars %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])
bm_hypo_vars_year$do_value <- ifelse(bm_hypo_vars_year$do_value == 0,NA,bm_hypo_vars_year$do_value)

bm_epi_fluxes_year <- bm_epi_fluxes %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])
bm_hypo_fluxes_year <- bm_hypo_fluxes %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])

bm_do_obs_epi_year <- bm_do_epi_obs %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])
bm_do_obs_hypo_year <- bm_do_hypo_obs %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])

bm_driving_year <- bm_driving %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])

bm_epi_vars_uncertainty_year <- bm_epi_vars_uncertainty %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])
bm_hypo_vars_uncertainty_year <- bm_hypo_vars_uncertainty %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])
bm_epi_fluxes_uncertainty_year <- bm_epi_fluxes_uncertainty %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])
bm_hypo_fluxes_uncertainty_year <- bm_hypo_fluxes_uncertainty %>% filter(sampledate < bm_dates[2]) %>% filter(sampledate > bm_dates[1])

#SP
sp_dates <- find_dates(min_date_year,max_date_year,sp_driving)
sp_epi_vars_year <- sp_epi_vars %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])
sp_hypo_vars_year <- sp_hypo_vars %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])
sp_hypo_vars_year$do_value <- ifelse(sp_hypo_vars_year$do_value == 0,NA,sp_hypo_vars_year$do_value)

sp_epi_fluxes_year <- sp_epi_fluxes %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])
sp_hypo_fluxes_year <- sp_hypo_fluxes %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])

sp_do_obs_epi_year <- sp_do_epi_obs %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])
sp_do_obs_hypo_year <- sp_do_hypo_obs %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])

sp_driving_year <- sp_driving %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])

sp_epi_vars_uncertainty_year <- sp_epi_vars_uncertainty %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])
sp_hypo_vars_uncertainty_year <- sp_hypo_vars_uncertainty %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])
sp_epi_fluxes_uncertainty_year <- sp_epi_fluxes_uncertainty %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])
sp_hypo_fluxes_uncertainty_year <- sp_hypo_fluxes_uncertainty %>% filter(sampledate < sp_dates[2]) %>% filter(sampledate > sp_dates[1])
 

## PLOT FUNCTIONS ###
do_plot <- function(epi_vars_model,hypo_vars_model,epi_obs,hypo_obs,driving_df,plot_title,rmse_values,x_title,y_title){
  
  if (x_title == 0 & y_title == 0){
    # plot(epi_vars_model$sampledate, (epi_vars_model$do_value/driving_df$volume_epi), type = 'l', col = 'blue', lwd = 2,ylim = c(0,18), ann = FALSE)
    # lines(hypo_vars_model$sampledate, (hypo_vars_model$do_value/driving_df$volume_hypo), type = 'l', col = 'red', lwd = 2)
    
    plot(epi_obs$sampledate,epi_obs$do_epi, col = 'blue', ylim = c(0,18), xlab = NA, ylab = NA, cex.axis = 1.5, cex.lab = 1.5)#, ylim = c(0,14))
    points(hypo_obs$sampledate, hypo_obs$do_hypo, col = 'red')

  }else if (x_title == 1 & y_title == 0){

    plot(epi_obs$sampledate,epi_obs$do_epi, col = 'blue', ylim = c(0,18), xlab = 'Date', ylab = NA,cex.axis = 1.5, cex.lab = 1.5)#, ylim = c(0,14))
    points(hypo_obs$sampledate, hypo_obs$do_hypo, col = 'red')
  
  }else if (x_title == 0 & y_title == 1){
    
    plot(epi_obs$sampledate,epi_obs$do_epi, col = 'blue', ylim = c(0,18), xlab = NA, ylab = 'DO (g/m3)',cex.axis = 1.5, cex.lab = 1.5)#, ylim = c(0,14))
    points(hypo_obs$sampledate, hypo_obs$do_hypo, col = 'red')
    
  }else if (x_title == 1 & y_title == 1){
    
    plot(epi_obs$sampledate,epi_obs$do_epi, col = 'blue', ylim = c(0,18), xlab = 'Date', ylab = 'DO (g/m3)',cex.axis = 1.5, cex.lab = 1.5)#, ylim = c(0,14))
    points(hypo_obs$sampledate, hypo_obs$do_hypo, col = 'red')
    
  }else{
    print ('label error')
  }
  
  #EPI
  lines(driving_df$sampledate, epi_vars_model$do_q75,type = 'l',lty=3, col = 'blue')
  lines(driving_df$sampledate, epi_vars_model$do_q25,type = 'l',lty=3, 'blue')
  polygon(c(driving_df$sampledate, rev(driving_df$sampledate)),c(epi_vars_model$do_q75,rev(epi_vars_model$do_q25)),
         col = adjustcolor("darkgray",alpha.f=0.6) ,lty = 0)
  lines(driving_df$sampledate, epi_vars_model$do_mean,type = 'l', col = 'blue', lwd = 2)
  
  
  #Hypo
  hypo_vars_model_drop <- hypo_vars_model %>% drop_na()
  hypo_vars_model_drop$year <- format(as.Date(hypo_vars_model_drop$sampledate, format="%Y-%m-%d"),"%Y")
  driving_df_drop <- driving_df %>% drop_na(thermocline_depth)
  
  for (y in unique(hypo_vars_model_drop$year)){
    value_q75 <-  hypo_vars_model_drop %>% filter(year == y) %>% select(sampledate,do_q75)
    value_q25 <-  hypo_vars_model_drop %>% filter(year == y) %>% select(sampledate,do_q25)
    value_mean <- hypo_vars_model_drop %>% filter(year == y) %>% select(sampledate,do_mean)
    
    lines(value_q75$sampledate, value_q75$do_q75,type = 'l',lty=3, lwd = 1, col = 'red')
    lines(value_q25$sampledate, value_q25$do_q25,type = 'l',lty=3, lwd= 1, col = 'red')
    
    polygon(c(value_q75$sampledate, rev(value_q75$sampledate)),
            c(value_q75$do_q75,rev(value_q25$do_q25)),
            col = adjustcolor("darkgray",alpha.f=0.6),lty = 0)
    
    lines(value_mean$sampledate, value_mean$do_mean,type = 'l',lwd = 2, col = 'red')
  }

  # 5 years
  text(x=as.Date(epi_vars_model$sampledate[200]), y=17, plot_title, col='black', cex=1.8)
  text(x=as.Date(epi_vars_model$sampledate[nrow(epi_vars_model)-300]), y=17, rmse_values, col='black', cex=1.5)

  #full ts
  # text(x=as.Date(epi_vars_model$sampledate[450]), y=17, plot_title, col='black', cex=1.8)
  # text(x=as.Date(epi_vars_model$sampledate[nrow(epi_vars_model)-650]), y=17, rmse_values, col='black', cex=1.5)
  # #abline(v = '2010-01-01', lty=2,lwd=1)

  
  #plot anoxia threshold line (1 gDO/m3)
  abline(h = 1, lty=2,lwd=0.8)  
  
  
  ##add line for mixed periods 
  #remove stratified periods
  driving_vars_df <- driving_df %>% 
    full_join(epi_vars_model, by = c('sampledate'))
  
  driving_vars_df$do_mean <- ifelse(is.na(driving_vars_df$thermocline_depth),driving_vars_df$do_mean,NA)
  
  mix_do_df <- driving_vars_df %>% select(sampledate, do_mean)
  
  lines(driving_df$sampledate, mix_do_df$do_mean, type = 'l', lty=1, lwd = 1.5,col = 'black')
  
}


doc_plot <- function(model_df,obs_df,driving_df,plot_title,rmse_values,x_title,y_title){
  if (x_title == 0 & y_title == 0){
      plot(obs_df$sampledate,obs_df$doc_epi,col = 'blue',xlab = NA ,ylab = NA,ylim = c(2,10), cex.axis = 1.5, cex.lab = 1.5)
    
    }else if (x_title == 1 & y_title == 0){
      plot(obs_df$sampledate,obs_df$doc_epi,col = 'blue',xlab = 'Date' ,ylab = NA,ylim = c(2,10), cex.axis = 1.5, cex.lab = 1.5)
      
    }else if (x_title == 0 & y_title == 1){
      plot(obs_df$sampledate,obs_df$doc_epi,col = 'blue',xlab = NA ,ylab = 'DOC (g/m3)',ylim = c(2,10), cex.axis = 1.5, cex.lab = 1.5)
  
    }else if (x_title == 1 & y_title == 1){
      plot(obs_df$sampledate,obs_df$doc_epi,col = 'blue',xlab = 'Date',ylab = 'DOC (g/m3)',ylim = c(2,10), cex.axis = 1.5, cex.lab = 1.5)
    
    }else{
    print ('label error')
  }

  lines(driving_df$sampledate, model_df$doc_tot_q25,type = 'l',lty=1, lwd = 0.5)
  lines(driving_df$sampledate, model_df$doc_tot_q75,type = 'l',lty=1, lwd = 0.5)
  
  polygon(c(driving_df$sampledate, rev(driving_df$sampledate)),c(model_df$doc_tot_q75,rev(model_df$doc_tot_q25)),
          col = adjustcolor("darkgray",alpha.f=0.6),lty = 0) # gainsboro
  lines(driving_df$sampledate, model_df$doc_tot_mean,type = 'l', lwd = 1)
  
  #Full TS
  text(x=as.Date(model_df$sampledate[800]), y=9.25, plot_title, col='black', cex=1.8)
  text(x=as.Date(model_df$sampledate[nrow(model_df)-250]), y=9.25, rmse_values, col='black', cex=1.5)
}


doc_hypo_plot <- function(model_df,obs_df,driving_df,plot_title,x_title,y_title){
  if (x_title == 0 & y_title == 0){
    plot(obs_df$sampledate,obs_df$doc_hypo,col = 'blue',xlab = NA ,ylab = NA,ylim = c(2,10), cex.axis = 1.5, cex.lab = 1.5)
    
  }else if (x_title == 1 & y_title == 0){
    plot(obs_df$sampledate,obs_df$doc_hypo,col = 'blue',xlab = 'Date' ,ylab = NA,ylim = c(2,10), cex.axis = 1.5, cex.lab = 1.5)
    
  }else if (x_title == 0 & y_title == 1){
    plot(obs_df$sampledate,obs_df$doc_hypo,col = 'blue',xlab = NA ,ylab = 'DOC (g/m3)',ylim = c(2,10), cex.axis = 1.5, cex.lab = 1.5)
    
  }else if (x_title == 1 & y_title == 1){
    plot(obs_df$sampledate,obs_df$doc_hypo,col = 'blue',xlab = 'Date',ylab = 'DOC (g/m3)',ylim = c(2,10), cex.axis = 1.5, cex.lab = 1.5)
    
  }else{
    print ('label error')
  }
  
  lines(driving_df$sampledate, model_df$doc_tot_q25,type = 'l',lty=1, lwd = 0.5)
  lines(driving_df$sampledate, model_df$doc_tot_q75,type = 'l',lty=1, lwd = 0.5)

  lines(driving_df$sampledate, model_df$doc_tot_mean,type = 'l', lwd = 1)
  

  text(x=as.Date(model_df$sampledate[800]), y=9.25, plot_title, col='black', cex=1.8) 
}


poc_plot <- function(model_df,obs_df,driving_df,plot_title,x_title,y_title){
  if (x_title == 0 & y_title == 0){
    plot(driving_df$sampledate, model_df$poc_r_mean + model_df$poc_l_mean ,type = 'l',lty=1, lwd = 1.5, xlab = NA ,ylab = NA,ylim = c(0,2), cex.axis = 1.5, cex.lab = 1.5)

  }else if (x_title == 1 & y_title == 0){
    plot(driving_df$sampledate, model_df$poc_r_mean + model_df$poc_l_mean ,type = 'l',lty=1, lwd = 1.5,xlab = 'Date' ,ylab = NA,ylim = c(0,2), cex.axis = 1.5, cex.lab = 1.5)
  }else if (x_title == 0 & y_title == 1){
    plot(driving_df$sampledate, model_df$poc_r_mean + model_df$poc_l_mean ,type = 'l',lty=1, lwd = 1.5,xlab = NA ,ylab = 'POC (g/m3)',ylim = c(0,2), cex.axis = 1.5, cex.lab = 1.5)
  }else if (x_title == 1 & y_title == 1){
    plot(driving_df$sampledate, model_df$poc_r_mean + model_df$poc_l_mean ,type = 'l',lty=1, lwd = 1.5,xlab = 'Date',ylab = 'POC (g/m3)',ylim = c(0,2), cex.axis = 1.5, cex.lab = 1.5)
  }else{
    print ('label error')
  }
  
  text(x=as.Date(model_df$sampledate[800]), y=1.75, plot_title, col='black', cex=1.8) 
}


secchi_plot <- function(model_df,obs_df,plot_title,rmse_values,x_title,y_title){
  if (x_title == 0 & y_title == 0){
    plot(obs_df$sampledate,obs_df$secchi,col = 'blue',xlab = NA,ylab = NA, ylim = c(0,13), cex.axis = 1.5, cex.lab = 1.5)
    
  }else if (x_title == 1 & y_title == 0){
    plot(obs_df$sampledate,obs_df$secchi,col = 'blue',xlab = 'Date',ylab = NA,ylim = c(0,13), cex.axis = 1.5, cex.lab = 1.5)
    
  }else if (x_title == 0 & y_title == 1){
    plot(obs_df$sampledate,obs_df$secchi,col = 'blue',xlab = NA,ylab = 'Secchi (m)',ylim = c(0,13), cex.axis = 1.5, cex.lab = 1.5)
    
  }else if (x_title == 1 & y_title == 1){
    plot(obs_df$sampledate,obs_df$secchi,col = 'blue',xlab = 'Date',ylab = 'Secchi (m)',ylim = c(0,13), cex.axis = 1.5, cex.lab = 1.5)
    
  }else{
    print ('label error')
  }

  lines(model_df$sampledate, model_df$secchi_q25,type = 'l',lty=1, lwd = 0.5)
  lines(model_df$sampledate, model_df$secchi_q75,type = 'l',lty=1, lwd = 0.5)
  
  polygon(c(model_df$sampledate, rev(model_df$sampledate)),c(model_df$secchi_q75,rev(model_df$secchi_q25)),
          col = adjustcolor("darkgray",alpha.f=0.6),lty = 0)
  
  lines(model_df$sampledate, model_df$secchi_mean,type = 'l', lwd = 1)
  
text(x=as.Date(model_df$sampledate[800]), y=12, plot_title, col='black', cex=1.8)
text(x=as.Date(model_df$sampledate[nrow(model_df)-250]), y=12, rmse_values, col='black', cex=1.5)
}

#DO plot

#c(bottom, left, top, right)

# 5 year plot
par(mfrow=c(3,2), mai = c(1.2, 1.3, 0.05, 0.05), omi=c(0,0.1,0.05,0.4),cex = 1.5)#mai = c(0.8, 0.8, 0.3, 0.2)) #mar= c(4,4,0,1))
FileName = paste('~/Documents/masters_work/model/metab_model/figures/do_all_lakes_plot.jpg',sep='')

do_plot(me_epi_vars_uncertainty_short,me_hypo_vars_uncertainty_short,me_do_epi_obs_short,me_do_hypo_obs_short,me_driving_short,'a) ME',"(2.02, 2.42)",0,1)
do_plot(mo_epi_vars_uncertainty_short,mo_hypo_vars_uncertainty_short,mo_do_epi_obs_short,mo_do_hypo_obs_short,mo_driving_short,'b) MO',"(1.90, 3.06)",0,0)
do_plot(tr_epi_vars_uncertainty_short,tr_hypo_vars_uncertainty_short,tr_do_epi_obs_short,tr_do_hypo_obs_short,tr_driving_short,'c) TR',"(0.83, 1.65)",0,1)
do_plot(al_epi_vars_uncertainty_short,al_hypo_vars_uncertainty_short,al_do_epi_obs_short,al_do_hypo_obs_short,al_driving_short,'d) AL',"(1.45, 3.00)",0,0)
do_plot(bm_epi_vars_uncertainty_short,bm_hypo_vars_uncertainty_short,bm_do_epi_obs_short,bm_do_hypo_obs_short,bm_driving_short,'e) BM',"(0.81, 1.99)",1,1)
do_plot(sp_epi_vars_uncertainty_short,sp_hypo_vars_uncertainty_short,sp_do_epi_obs_short,sp_do_hypo_obs_short,sp_driving_short,'f) SP',"(0.88, 2.02)",1,0)

dev.copy(jpeg,FileName,width=500,height=285,units='mm',res=1000)
dev.off()


#full time series 
par(mfrow=c(3,2), mai = c(0.9, 0.9, 0.05, 0.05), omi=c(0,0.3,0.05,0.2),cex = 1.5)
FileName = paste('~/Documents/masters_work/model/metab_model/figures/do_all_lakes_all_years_plot.jpg',sep='')

do_plot(me_epi_vars_uncertainty,me_hypo_vars_uncertainty,me_do_epi_obs,me_do_hypo_obs,me_driving,'a) ME',"(2.02, 2.42)",0,1)
do_plot(mo_epi_vars_uncertainty,mo_hypo_vars_uncertainty,mo_do_epi_obs,mo_do_hypo_obs,mo_driving,'b) MO',"(1.90, 3.06)",0,0)
do_plot(tr_epi_vars_uncertainty,tr_hypo_vars_uncertainty,tr_do_epi_obs,tr_do_hypo_obs,tr_driving,'c) TR',"(0.83, 1.65)",0,1)
do_plot(al_epi_vars_uncertainty,al_hypo_vars_uncertainty,al_do_epi_obs,al_do_hypo_obs,al_driving,'d) AL',"(1.45, 3.00)",0,0)
do_plot(bm_epi_vars_uncertainty,bm_hypo_vars_uncertainty,bm_do_epi_obs,bm_do_hypo_obs,bm_driving,'e) BM',"(0.81, 1.99)",1,1)
do_plot(sp_epi_vars_uncertainty,sp_hypo_vars_uncertainty,sp_do_epi_obs,sp_do_hypo_obs,sp_driving,'f) SP',"(0.88, 2.02)",1,0)

dev.copy(jpeg,FileName,width=500,height=285,units='mm',res=1000)
dev.off()

#DOC Plot 
par(mfrow=c(3,2), mai = c(0.8, 1.3, 0.05, 0.3), omi=c(0.5,0.5,0.1,0.2),cex = 1.5)
FileName = paste('~/Documents/masters_work/model/metab_model/figures/doc_all_lakes_plot.jpg',sep='')

doc_plot(me_epi_vars_uncertainty_short,me_doc_epi_obs_short,me_driving_short,'a) ME',"(1.11)",0,1)
doc_plot(mo_epi_vars_uncertainty_short,mo_doc_epi_obs_short,mo_driving_short,'b) MO',"(1.04)",0,0)
doc_plot(tr_epi_vars_uncertainty_short,tr_doc_epi_obs_short,tr_driving_short,'c) TR',"(0.31)",0,1)
doc_plot(al_epi_vars_uncertainty_short,al_doc_epi_obs_short,al_driving_short,'d) AL',"(0.56)",0,0)
doc_plot(bm_epi_vars_uncertainty_short,bm_doc_epi_obs_short,bm_driving_short,'e) BM',"(0.53)",1,1)
doc_plot(sp_epi_vars_uncertainty_short,sp_doc_epi_obs_short,sp_driving_short,'f) SP',"(0.51)",1,0)

dev.copy(jpeg,FileName,width=500,height=285,units='mm',res=1000)
dev.off()

 # full time series 
par(mfrow=c(3,2), mai = c(1.2, 1.3, 0.05, 0.05), omi=c(0,0.1,0.05,0.4),cex = 1.5)#mai = c(0.8, 0.8, 0.3, 0.2)) #mar= c(4,4,0,1))
FileName = paste('~/Documents/masters_work/model/metab_model/figures/doc_all_lakes_all_years_plot.jpg',sep='')

doc_plot(me_epi_vars_uncertainty,me_doc_epi_obs,me_driving,'a) ME',"(1.11)",0,1)
doc_plot(mo_epi_vars_uncertainty,mo_doc_epi_obs,mo_driving,'b) MO',"(1.04)",0,0)
doc_plot(tr_epi_vars_uncertainty,tr_doc_epi_obs,tr_driving,'c) TR',"(0.31)",0,1)
doc_plot(al_epi_vars_uncertainty,al_doc_epi_obs,al_driving,'d) AL',"(0.56)",0,0)
doc_plot(bm_epi_vars_uncertainty,bm_doc_epi_obs,bm_driving,'e) BM',"(0.53)",1,1)
doc_plot(sp_epi_vars_uncertainty,sp_doc_epi_obs,sp_driving,'f) SP',"(0.52)",1,0)

dev.copy(jpeg,FileName,width=500,height=285,units='mm',res=1000)
dev.off()

### HYPO Plot
par(mfrow=c(3,2), mai = c(1.2, 1.3, 0.05, 0.05), omi=c(0,0.1,0.05,0.4),cex = 1.5)
FileName = paste('~/Documents/masters_work/model/metab_model/figures/doc_hypo_all_lakes_all_years_plot.jpg',sep='')

doc_hypo_plot(me_hypo_vars_uncertainty,me_doc_hypo_obs,me_driving,'a) ME',0,1)
doc_hypo_plot(mo_hypo_vars_uncertainty,mo_doc_hypo_obs,mo_driving,'b) MO',0,0)
doc_hypo_plot(tr_hypo_vars_uncertainty,tr_doc_hypo_obs,tr_driving,'c) TR',0,1)
doc_hypo_plot(al_hypo_vars_uncertainty,al_doc_hypo_obs,al_driving,'d) AL',0,0)
doc_hypo_plot(bm_hypo_vars_uncertainty,bm_doc_hypo_obs,bm_driving,'e) BM',1,1)
doc_hypo_plot(sp_hypo_vars_uncertainty,sp_doc_hypo_obs,sp_driving,'f) SP',1,0)

dev.copy(jpeg,FileName,width=500,height=285,units='mm',res=1000)
dev.off()


### POC Plot
par(mfrow=c(3,2), mai = c(1.2, 1.3, 0.05, 0.05), omi=c(0,0.1,0.2,0.4),cex = 1.5)
FileName = paste('~/Documents/masters_work/model/metab_model/figures/poc_all_lakes_all_years_plot.jpg',sep='')

poc_plot(me_epi_vars_uncertainty,me_doc_epi_obs,me_driving,'a) ME',0,1)
poc_plot(mo_epi_vars_uncertainty,mo_doc_epi_obs,mo_driving,'b) MO',0,0)
poc_plot(tr_epi_vars_uncertainty,tr_doc_epi_obs,tr_driving,'c) TR',0,1)
poc_plot(al_epi_vars_uncertainty,al_doc_epi_obs,al_driving,'d) AL',0,0)
poc_plot(bm_epi_vars_uncertainty,bm_doc_epi_obs,bm_driving,'e) BM',1,1)
poc_plot(sp_epi_vars_uncertainty,sp_doc_epi_obs,sp_driving,'f) SP',1,0)


dev.copy(jpeg,FileName,width=500,height=285,units='mm',res=1000)
dev.off()

### POC hypo Plot
par(mfrow=c(3,2), mai = c(1.2, 1.3, 0.05, 0.05), omi=c(0,0.1,0.2,0.4),cex = 1.5)
FileName = paste('~/Documents/masters_work/model/metab_model/figures/poc_hypo_all_lakes_all_years_plot.jpg',sep='')

poc_plot(me_hypo_vars_uncertainty,me_doc_epi_obs,me_driving,'a) ME',0,1)
poc_plot(mo_hypo_vars_uncertainty,mo_doc_epi_obs,mo_driving,'b) MO',0,0)
poc_plot(tr_hypo_vars_uncertainty,tr_doc_epi_obs,tr_driving,'c) TR',0,1)
poc_plot(al_hypo_vars_uncertainty,al_doc_epi_obs,al_driving,'d) AL',0,0)
poc_plot(bm_hypo_vars_uncertainty,bm_doc_epi_obs,bm_driving,'e) BM',1,1)
poc_plot(sp_hypo_vars_uncertainty,sp_doc_epi_obs,sp_driving,'f) SP',1,0)


dev.copy(jpeg,FileName,width=500,height=285,units='mm',res=1000)
dev.off()


#Secchi Plot
par(mfrow=c(3,2), mai = c(1.2, 1.3, 0.05, 0.05), omi=c(0,0.1,0.05,0.4),cex = 1.5)
FileName = paste('~/Documents/masters_work/model/metab_model/figures/secchi_all_lakes_plot.jpg',sep='')

secchi_plot(me_epi_vars_uncertainty_short,me_secchi_obs_short,'a) ME',"(2.38)",0,1)
secchi_plot(mo_epi_vars_uncertainty_short,mo_secchi_obs_short,'b) MO',"(1.98)",0,0)
secchi_plot(tr_epi_vars_uncertainty_short,tr_secchi_obs_short,'c) TR',"(0.85)",0,1)
secchi_plot(al_epi_vars_uncertainty_short,al_secchi_obs_short,'d) AL',"(0.54)",0,0)
secchi_plot(bm_epi_vars_uncertainty_short,bm_secchi_obs_short,'e) BM',"(0.72)",1,1)
secchi_plot(sp_epi_vars_uncertainty_short,sp_secchi_obs_short,'f) SP',"(1.11)",1,0)

dev.copy(jpeg,FileName,width=500,height=285,units='mm',res=1000)
dev.off()

# Full time series
par(mfrow=c(3,2), mai = c(1.2, 1.3, 0.05, 0.05), omi=c(0,0.1,0.05,0.4),cex = 1.5)
FileName = paste('~/Documents/masters_work/model/metab_model/figures/secchi_all_lakes_all_years_plot.jpg',sep='')

secchi_plot(me_epi_vars_uncertainty,me_secchi_obs,'a) ME',"(2.38)",0,1)
secchi_plot(mo_epi_vars_uncertainty,mo_secchi_obs,'b) MO',"(1.98)",0,0)
secchi_plot(tr_epi_vars_uncertainty,tr_secchi_obs,'c) TR',"(0.85)",0,1)
secchi_plot(al_epi_vars_uncertainty,al_secchi_obs,'d) AL',"(0.54)",0,0)
secchi_plot(bm_epi_vars_uncertainty,bm_secchi_obs,'e) BM',"(0.72)",1,1)
secchi_plot(sp_epi_vars_uncertainty,sp_secchi_obs,'f) SP',"(1.11)",1,0)

dev.copy(jpeg,FileName,width=500,height=285,units='mm',res=1000)
dev.off()



######## ANNUAL HYPO DYNAMICS -- DO profile, OC settling, Rtotal, Rsed over one summer (USED FOR FIG 10 CURRENTLY)

hypo_do_oc_plot <- function(hypo_vars_df,hypo_fluxes_df,epi_fluxes_df,driving_df,driving_df_full,plot_title,x_title,y_title){
## calculate curves 
#do
hypo_plot <- hypo_vars_df %>% select(sampledate,do_value)
##do_sat 
lake_do_sat <- driving_df %>% 
  #mutate(do_sat_conc = do_sat_value/(volume_epi[1] + volume_hypo[1])) %>% 
  mutate(do_sat_conc = do_sat_value/(volume_epi)) %>% 
  select(sampledate,do_sat_conc)
hypo_plot <- hypo_vars_df %>% left_join(lake_do_sat, by=c('sampledate'))
hypo_plot <- hypo_plot %>% mutate(do_sat_compare = (do_value/driving_df$volume_hypo)/do_sat_conc)
#resp
hypo_plot$resp_cum <- cumsum(na.omit(hypo_fluxes_df$resp_total) + na.omit(hypo_fluxes_df$resp_sed))
hypo_plot$resp_wc_cum <- cumsum(hypo_fluxes_df$resp_total)
hypo_plot$resp_sed_cum <- cumsum(hypo_fluxes_df$resp_sed)

##renamed from above -- just testing settle and entr dynamics
hypo_plot$pocl_settle_cum <- cumsum(na.omit(epi_fluxes_df$poc_l_settle)) - cumsum(na.omit(epi_fluxes_df$poc_l_entrainment))
hypo_plot$pocr_settle_cum <- cumsum(na.omit(epi_fluxes_df$poc_r_settle)) - cumsum(na.omit(epi_fluxes_df$poc_r_entrainment))

#plot in units of g/m2
lake_area <- driving_df_full$area_td[1]

if (x_title == 0 & y_title == 0){
  plot(hypo_plot$sampledate,(hypo_plot$resp_cum/lake_area),type = 'l', col = 'lightblue1',xlab = NA,ylab = NA, ylim = c(0,100),cex.axis = 2, cex.lab = 2)
  
}else if (x_title == 1 & y_title == 0){
  plot(hypo_plot$sampledate,(hypo_plot$resp_cum/lake_area),type = 'l', col = 'lightblue1',xlab = 'Date',ylab = NA, ylim = c(0,100),cex.axis = 2, cex.lab = 2)
  
}else if (x_title == 0 & y_title == 1){
  plot(hypo_plot$sampledate,(hypo_plot$resp_cum/lake_area),type = 'l', col = 'lightblue1',xlab = NA,ylab = 'Flux (g/m2), % DO Sat', ylim = c(0,100), cex.axis = 2, cex.lab = 2)
  
}else if (x_title == 1 & y_title == 1){
  plot(hypo_plot$sampledate,(hypo_plot$resp_cum/lake_area),type = 'l', col = 'lightblue1',xlab = 'Date',ylab = 'Flux (g/m2), % DO Sat', ylim = c(0,100),cex.axis = 2, cex.lab = 2)

}else{
  print ('label error')
}  
lines(hypo_plot$sampledate,(hypo_plot$resp_sed_cum/lake_area),type = 'l', col = 'burlywood1', lwd = 2)
lines(hypo_plot$sampledate,(hypo_plot$pocl_settle_cum/lake_area),type = 'l', col = 'darkgreen', lwd = 2)

zero_line = rep(0,nrow(hypo_plot))

##r_wc fill 
polygon(c(hypo_plot$sampledate, rev(hypo_plot$sampledate)), 
        c(hypo_plot$resp_cum/lake_area, rev(hypo_plot$resp_sed_cum/lake_area)), col = "lightblue1", lwd = 0.5, lty = 1)#, density = 10, angle = 45)

##r_sed fill
polygon(c(hypo_plot$sampledate, rev(hypo_plot$sampledate)),
        c(hypo_plot$resp_sed_cum/lake_area, rev(zero_line)), col = "burlywood1", lwd = 0.5, lty = 1)#, density = 10, angle = 45)

## OC_auto fill 
polygon(c(hypo_plot$sampledate, rev(hypo_plot$sampledate)), 
        c(hypo_plot$pocl_settle_cum/lake_area, rev(hypo_plot$resp_cum/lake_area)), col = "darkgreen", density = 20, angle = 45)


## DO line
lines(hypo_plot$sampledate,(hypo_plot$do_sat_compare*100),type = 'l', col = 'gray50') #satruation value

##POC_alloch line 
lines(hypo_plot$sampledate,(hypo_plot$pocr_settle_cum/lake_area),type = 'l', col = 'darkred', lwd = 2)

text(x=as.Date(hypo_plot$sampledate[20]), y=95, plot_title, col='black', cex=3) 

}

par(mfrow=c(1,1), mai = c(0.3, 1.1, 0.1, 0.05), omi=c(0.4,0.1,0.1,0.1))#,cex = 1.5)
hypo_do_oc_plot(me_hypo_vars_year,me_hypo_fluxes_year,me_epi_fluxes_year,me_driving_year,me_driving,'ME',0,1)

par(mfrow=c(1,1), mai = c(0.1, 1.1, 0.05, 0.05), omi=c(0.4,0.1,0.1,0.1))#,cex = 1.5)
hypo_do_oc_plot(mo_hypo_vars_year,mo_hypo_fluxes_year,mo_epi_fluxes_year,mo_driving_year,mo_driving,'MO',0,0)

par(mfrow=c(1,1), mai = c(0.1, 1.1, 0.05, 0.05), omi=c(0.4,0.1,0.1,0.1))#,cex = 1.5)
hypo_do_oc_plot(tr_hypo_vars_year,tr_hypo_fluxes_year,tr_epi_fluxes_year,tr_driving_year,tr_driving,'TR',0,0)

par(mfrow=c(1,1), mai = c(0.1, 1.1, 0.05, 0.05), omi=c(0.4,0.1,0.1,0.1))#,cex = 1.5)
hypo_do_oc_plot(al_hypo_vars_year,al_hypo_fluxes_year,al_epi_fluxes_year,al_driving_year,al_driving,'AL',1,1)

par(mfrow=c(1,1), mai = c(0.1, 1.1, 0.05, 0.05), omi=c(0.4,0.1,0.1,0.1))#,cex = 1.5)
hypo_do_oc_plot(bm_hypo_vars_year,bm_hypo_fluxes_year,bm_epi_fluxes_year,bm_driving_year,bm_driving,'BM',1,0)

par(mfrow=c(1,1), mai = c(0.1, 1.1, 0.05, 0.05), omi=c(0.4,0.1,0.1,0.1))#,cex = 1.5)
hypo_do_oc_plot(sp_hypo_vars_year,sp_hypo_fluxes_year,sp_epi_fluxes_year,sp_driving_year,sp_driving,'SP',1,0)


library(plotrix)

### whole lake (carbon budget plot) 
oc_budget_plot <- function(epi_fluxes_df,hypo_fluxes_df,driving_df,plot_title){

  lake_area <- driving_df$area_td[1]
  
  budget_build <- driving_df %>% select(sampledate)
  budget_build$year <- format(as.Date(budget_build$sampledate, format="%Y-%m-%d"),"%Y")
  
  budget_build$poc_alloch_raw <- epi_fluxes_df$poc_l_in + epi_fluxes_df$poc_r_in
  budget_build$doc_alloch_raw <- epi_fluxes_df$doc_l_in + epi_fluxes_df$doc_r_in
  budget_build$oc_alloch_raw <- budget_build$poc_alloch_raw + budget_build$doc_alloch_raw
  budget_build$poc_out_raw <- epi_fluxes_df$poc_l_out + epi_fluxes_df$poc_r_out
  budget_build$doc_out_raw <- epi_fluxes_df$doc_l_out + epi_fluxes_df$doc_r_out
  budget_build$oc_export_raw <- budget_build$poc_out_raw + budget_build$doc_out_raw
  budget_build$resp_wc_raw <- epi_fluxes_df$resp_total + hypo_fluxes_df$resp_total
  budget_build$resp_sed_raw <- epi_fluxes_df$resp_sed + hypo_fluxes_df$resp_sed
  budget_build$poc_sediment_raw <- hypo_fluxes_df$poc_total_sediment_flux
  budget_build$oc_burial_raw <- budget_build$poc_sediment_raw - budget_build$resp_sed_raw
  budget_build$oc_auto_raw <- epi_fluxes_df$npp_value + hypo_fluxes_df$npp_value
  
  
  oc_budget_sum <- budget_build %>%
    group_by(year) %>%
    mutate(oc_alloch_sum = sum(na.omit(oc_alloch_raw)/lake_area)) %>%
    mutate(oc_export_sum = sum(na.omit((oc_export_raw)/lake_area))) %>%
    mutate(resp_wc_sum = sum(na.omit((resp_wc_raw)/lake_area))) %>%
    mutate(resp_sed_sum = sum(na.omit((resp_sed_raw)/lake_area))) %>%
    mutate(oc_burial_sum = sum(na.omit((oc_burial_raw)/lake_area))) %>%
    mutate(oc_auto_sum = sum(na.omit((oc_auto_raw)/lake_area))) %>% 
    ungroup() %>%
    mutate(oc_alloch = mean(oc_alloch_sum)) %>%
    mutate(oc_export = mean(oc_export_sum)) %>%
    mutate(resp_wc = mean(resp_wc_sum)) %>%
    mutate(resp_sed = mean(resp_sed_sum)) %>%
    mutate(oc_burial = mean(oc_burial_sum)) %>%
    mutate(oc_auto = mean(oc_auto_sum)) %>%
    distinct(year, .keep_all = TRUE)
  


  oc_alloch_sem <- std.error(oc_budget_sum$oc_alloch_sum)
  oc_export_sem <- std.error(oc_budget_sum$oc_export_sum)
  resp_wc_sem <- std.error(oc_budget_sum$resp_wc_sum)
  resp_sed_sem <- std.error(oc_budget_sum$resp_sed_sum)
  oc_burial_sem <- std.error(oc_budget_sum$oc_burial_sum)
  oc_auto_sem <- std.error(oc_budget_sum$oc_auto_sum)

  oc_budget_info <- oc_budget_sum %>%
    distinct(year, .keep_all = TRUE) %>%
    select(oc_alloch,oc_export,resp_wc,resp_sed,oc_burial,oc_auto)

  oc_budget_info$lake <- plot_title
  oc_budget_info <- oc_budget_info[1,]

  
  oc_source_df <- oc_budget_info %>% select(oc_alloch,oc_auto) %>% mutate(oc_cat = 'source') %>% distinct()
  oc_fate_df <- oc_budget_info %>% select(oc_export,resp_wc,resp_sed,oc_burial) %>% mutate(oc_cat = 'fate')


  oc_source_matrix <- gather(oc_source_df,"oc_flux","flux_value", -oc_cat)
  oc_fate_matrix <- gather(oc_fate_df,"oc_flux","flux_value", -oc_cat)

  oc_matrix <- rbind(oc_source_matrix,oc_fate_matrix)

  oc_total = oc_matrix %>% group_by(oc_cat) %>% drop_na() %>% summarise(total_oc = sum(flux_value)) %>% ungroup()

  annual_oc_props = left_join(oc_matrix, oc_total, by = c('oc_cat')) %>%
    mutate(flux_val_prop = (flux_value / total_oc)*100) %>%
    mutate(oc_cat = factor(oc_cat, levels = c("source", "fate"), ordered = TRUE))
  
  annual_oc_props$lake <- plot_title


  annual_oc_props$se <- c(oc_alloch_sem,oc_export_sem,resp_wc_sem,resp_sed_sem,oc_burial_sem,oc_auto_sem)

  
  alloch_add = annual_oc_props %>%
    filter(oc_flux == "oc_auto") %>%
    select(lake, flux_add = flux_value) %>%
    mutate(oc_flux = "oc_alloch")

  rsed_add = annual_oc_props %>%
    filter(oc_flux == "resp_wc") %>%
    select(lake, flux_add = flux_value) %>%
    mutate(oc_flux = "resp_sed")

  export_add = annual_oc_props %>%
    filter(oc_flux %in% c('resp_wc',"resp_sed")) %>%
    group_by(lake) %>%
    summarise(flux_add = sum(flux_value)) %>%
    mutate(oc_flux = "oc_export") %>%
    select(lake, oc_flux, flux_add)

  burial_add = annual_oc_props %>%
    filter(oc_flux %in% c('resp_wc',"resp_sed","oc_export")) %>%
    group_by(lake) %>%
    summarise(flux_add = sum(flux_value)) %>%
    mutate(oc_flux = "oc_burial") %>%
    select(lake, oc_flux, flux_add)

  df_new = annual_oc_props %>%
    left_join(bind_rows(alloch_add, rsed_add,export_add,burial_add)) %>%
    mutate(flux_add = ifelse(is.na(flux_add), 0, flux_add)) %>%
    mutate(upper = flux_value + flux_add + se, lower = flux_value + flux_add - se)
  
  
  return(df_new)

}

p1 <- oc_budget_plot(me_epi_fluxes,me_hypo_fluxes,me_driving,'ME')
p2 <- oc_budget_plot(mo_epi_fluxes,mo_hypo_fluxes,mo_driving,'MO')
p3 <- oc_budget_plot(tr_epi_fluxes,tr_hypo_fluxes,tr_driving,'TR')
p4 <- oc_budget_plot(al_epi_fluxes,al_hypo_fluxes,al_driving,'AL')
p5 <- oc_budget_plot(bm_epi_fluxes,bm_hypo_fluxes,bm_driving,'BM')
p6 <- oc_budget_plot(sp_epi_fluxes,sp_hypo_fluxes,sp_driving,'SP')

all_lake_oc <- rbind(p1,p2,p3,p4,p5,p6)
all_lake_oc <- all_lake_oc %>% mutate(lake = factor(lake, levels = c("ME","MO",'TR','AL','BM','SP'), ordered = TRUE))

#just the sources
all_lake_oc_source <- all_lake_oc
all_lake_oc_source$oc_flux <- ifelse(all_lake_oc$oc_cat == 'fate',NA,all_lake_oc_source$oc_flux)
all_lake_oc_source$flux_value <- ifelse(all_lake_oc$oc_cat == 'fate',NA,all_lake_oc_source$flux_value)
all_lake_oc_source$upper <- ifelse(all_lake_oc$oc_cat == 'fate',NA,all_lake_oc_source$upper)
all_lake_oc_source$lower <- ifelse(all_lake_oc$oc_cat == 'fate',NA,all_lake_oc_source$lower)

ggplot(all_lake_oc_source, aes(fill=oc_flux, y=flux_value, x=oc_cat)) +
  geom_bar(position='stack', stat='identity') +
  scale_fill_manual(values = c("azure3","azure4","chartreuse4","darkorchid1","burlywood2","lightblue2")) +
  geom_text(aes(label = paste0(round(flux_val_prop,1),"%")),position = position_stack(vjust = 0.5), size = 3)+
  ylab('Flux (g/m2/year)') +
  xlab('OC Flux Category') +
  ggtitle('Annual OC Budget')+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.3, position = position_dodge(0.9)) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18)) +
  facet_wrap(~lake, scales = "free_y")



ggplot(all_lake_oc, aes(fill=oc_flux, y=flux_value, x=oc_cat)) +
  geom_bar(position='stack', stat='identity') +
  scale_fill_manual(values = c("azure3","azure4","chartreuse4","darkorchid1","burlywood2","lightblue2")) +
  geom_text(aes(label = paste0(round(flux_val_prop,1),"%")),position = position_stack(vjust = 0.5), size = 3)+
  ylab('Flux (g/m2/year)') +
  xlab('OC Flux Category') +
  #ggtitle('Annual OC Budget')+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.3, position = position_dodge(0.9)) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18)) +
  facet_wrap(~lake, scales = "free_y")



##make oc budget table from same information 
oc_all_table <- all_lake_oc %>% select(lake, oc_flux, flux_value)

oc_table_spread <- spread(oc_all_table, key=oc_flux, value=flux_value)
write.csv(oc_table_spread,'~/Documents/masters_work/model/metab_model/figures/oc_budget_table.csv',row.names = FALSE)


hypo_oc_budget_plot <- function(epi_fluxes_df,hypo_fluxes_df,driving_df,plot_title,lake_abbrev){
  
  lake_area <- driving_df$area_td
  
  hypo_driving <- driving_df %>% select(sampledate, area_td,thermocline_depth)
  hypo_driving$year <- format(as.Date(hypo_driving$sampledate, format="%Y-%m-%d"),"%Y")
  
  hypo_df <- hypo_driving %>% 
    left_join(hypo_fluxes_df,by = c('sampledate')) %>% 
    drop_na(thermocline_depth) %>% 
    group_by(year) %>% 
    mutate(sed_resp_sum = sum(na.omit(resp_sed/lake_area))) %>% 
    mutate(poc_resp_sum = sum(na.omit((resp_poc_l + resp_poc_r)/lake_area))) %>% 
    mutate(doc_resp_sum = sum(na.omit((resp_doc_l + resp_doc_r)/lake_area))) %>%
    ungroup() %>% 
    mutate(resp_sed = mean(sed_resp_sum)) %>% 
    mutate(resp_poc_wc = mean(poc_resp_sum)) %>% 
    mutate(resp_doc_wc = mean(doc_resp_sum))
  
  sed_sem <- std.error(hypo_df$sed_resp_sum)
  poc_sem <- std.error(hypo_df$poc_resp_sum)
  doc_sem <- std.error(hypo_df$doc_resp_sum)
  
  hypo_df2 <- hypo_df %>% 
    distinct(year, .keep_all = TRUE) %>% 
    select(resp_sed,resp_poc_wc,resp_doc_wc)
  
  hypo_df2$lake <- lake_abbrev
  hypo_df2 <- hypo_df2[1,]

  hypo_matrix_year <- gather(hypo_df2,"oc_fate","flux_value", -lake)
  
  hypo_matrix_year$se <- c(sed_sem,poc_sem,doc_sem)
  
  poc_wc_add = hypo_matrix_year %>%
    filter(oc_fate == "resp_sed") %>%
    select(lake, flux_add = flux_value) %>%
    mutate(oc_fate = "resp_poc_wc")
  
  doc_wc_add = hypo_matrix_year %>%
    filter(oc_fate %in% c("resp_sed", "resp_poc_wc")) %>%
    group_by(lake) %>%
    summarise(flux_add = sum(flux_value)) %>%
    mutate(oc_fate = "resp_doc_wc") %>%
    select(lake, oc_fate, flux_add)
  
  df_new = hypo_matrix_year %>%
    left_join(bind_rows(poc_wc_add, doc_wc_add)) %>% 
    mutate(flux_add = ifelse(is.na(flux_add), 0, flux_add)) %>%
    mutate(upper = flux_value + flux_add + se, lower = flux_value + flux_add - se)

  return(df_new)
  }

me_annual_oc <- hypo_oc_budget_plot(me_epi_fluxes,me_hypo_fluxes,me_driving,'Mendota Hypo','ME')
mo_annual_oc <- hypo_oc_budget_plot(mo_epi_fluxes,mo_hypo_fluxes,mo_driving,'Monona Hypo','MO')
tr_annual_oc <- hypo_oc_budget_plot(tr_epi_fluxes,tr_hypo_fluxes,tr_driving,'Trout Hypo','TR')
al_annual_oc <- hypo_oc_budget_plot(al_epi_fluxes,al_hypo_fluxes,al_driving,'Allequash Hypo','AL')
bm_annual_oc <- hypo_oc_budget_plot(bm_epi_fluxes,bm_hypo_fluxes,bm_driving,'Big Muskie Hypo','BM')
sp_annual_oc <- hypo_oc_budget_plot(sp_epi_fluxes,sp_hypo_fluxes,sp_driving,'Sparkling Hypo','SP')

lakes_annual_oc <- rbind(me_annual_oc,mo_annual_oc,tr_annual_oc,al_annual_oc,bm_annual_oc,sp_annual_oc)

lakes_total = lakes_annual_oc %>% group_by(lake) %>% drop_na() %>% summarise(total = sum(flux_value)) %>% ungroup()
lakes_annual_oc = left_join(lakes_annual_oc, lakes_total) %>% mutate(flux_val_prop = (flux_value / total)*100)

lakes_annual_oc$lake <- factor(lakes_annual_oc$lake, levels = c("ME", "MO", "TR", "AL", "BM", "SP"))

##stacked bar plot (https://www.statology.org/stacked-barplot-in-r/)
ggplot(lakes_annual_oc, aes(fill=oc_fate, y=flux_value, x=lake)) +
  geom_bar(position='stack', stat='identity') +
  geom_text(aes(label = paste0(round(flux_val_prop,1),"%")),position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("lightcoral","lightblue2","burlywood2")) +
  ylab('Respiration (g/m2/year)') + 
  xlab('Lake') +
  #ggtitle('OC Proportion of Hypolimnetic Respiration (g/m2/year)')+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.2) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))



#MAKE TEMPORAL PLOT FROM FIG 8 (PLOT RSED OVER TIME)

oc_temporal_plot <- function(hypo_fluxes, driving_data, lake_title){

oc_budget_ts <- hypo_fluxes

#first split up date to be just years
oc_budget_ts$year <- format(as.Date(oc_budget_ts$sampledate, format="%Y-%m-%d"),"%Y")

#group by year
oc_budget_grouped <- oc_budget_ts %>% 
  group_by(year) %>% 
  mutate(hypo_poc_resp_total = sum(resp_poc_l + resp_poc_r, na.rm = TRUE)) %>% 
  mutate(hypo_doc_resp_total = sum(resp_doc_l + resp_doc_r, na.rm = TRUE)) %>%   
  mutate(hypo_resp_sed_total = sum(resp_sed, na.rm = TRUE)) %>% 
  mutate(hypo_resp_total = sum(resp_total + resp_sed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(year, .keep_all = TRUE) %>% 
  select(year, hypo_poc_resp_total, hypo_doc_resp_total, hypo_resp_sed_total, hypo_resp_total)

oc_budget_grouped$hypo_poc_resp_total <- oc_budget_grouped$hypo_poc_resp_total/(driving_data$area_td[1])
oc_budget_grouped$hypo_doc_resp_total <- oc_budget_grouped$hypo_doc_resp_total/(driving_data$area_td[1])
oc_budget_grouped$hypo_resp_sed_total <- oc_budget_grouped$hypo_resp_sed_total/(driving_data$area_td[1])

oc_budget_grouped$hypo_resp_total <- oc_budget_grouped$hypo_resp_total/(driving_data$area_td[1])

oc_budget_grouped$hypo_poc_resp_percent <- (oc_budget_grouped$hypo_poc_resp_total/oc_budget_grouped$hypo_resp_total)*100
oc_budget_grouped$hypo_doc_resp_percent <- (oc_budget_grouped$hypo_doc_resp_total/oc_budget_grouped$hypo_resp_total)*100
oc_budget_grouped$hypo_resp_sed_percent <- (oc_budget_grouped$hypo_resp_sed_total/oc_budget_grouped$hypo_resp_total)*100



if (lake_title %in% c('SP','BM')){
plot(oc_budget_grouped$year,oc_budget_grouped$hypo_poc_resp_total,type = 'l',col = 'chartreuse4', ylim = c(0,40), ylab = NA, xlab = 'Date', cex.axis = 1.5, cex.lab = 1.5,lwd = 2)
points(oc_budget_grouped$year,oc_budget_grouped$hypo_poc_resp_total,col = 'chartreuse4',cex = 1.5)
abline(lm(oc_budget_grouped$hypo_poc_resp_total ~ as.numeric(as.character(oc_budget_grouped$year))), lty = 2, lwd = 0.5)

lines(oc_budget_grouped$year,oc_budget_grouped$hypo_doc_resp_total,type = 'l',col = 'lightcoral',lwd = 2)
points(oc_budget_grouped$year,oc_budget_grouped$hypo_doc_resp_total,col = 'lightcoral',cex = 1.5)
abline(lm(oc_budget_grouped$hypo_doc_resp_total ~ as.numeric(as.character(oc_budget_grouped$year))), lty = 2, lwd = 0.5)

lines(oc_budget_grouped$year,oc_budget_grouped$hypo_resp_sed_total,type = 'l',col = 'darkorange3',lwd = 2)
points(oc_budget_grouped$year,oc_budget_grouped$hypo_resp_sed_total,col = 'darkorange3',cex = 1.5)
abline(lm(oc_budget_grouped$hypo_resp_sed_total ~ as.numeric(as.character(oc_budget_grouped$year))),lty = 2, lwd = 0.5)
}

if (lake_title %in% c('ME','MO','TR','AL')){
  plot(oc_budget_grouped$year,oc_budget_grouped$hypo_poc_resp_total,type = 'l',col = 'chartreuse4', ylim = c(0,40), ylab = NA, xlab = NA, xaxt = 'n', cex.axis = 1.5, lwd = 2)
  points(oc_budget_grouped$year,oc_budget_grouped$hypo_poc_resp_total,col = 'chartreuse4',cex = 1.5)
  abline(lm(oc_budget_grouped$hypo_poc_resp_total ~ as.numeric(as.character(oc_budget_grouped$year))), lty = 2, lwd = 0.5)
  
  lines(oc_budget_grouped$year,oc_budget_grouped$hypo_doc_resp_total,type = 'l',col = 'lightcoral', lwd = 2)
  points(oc_budget_grouped$year,oc_budget_grouped$hypo_doc_resp_total,col = 'lightcoral',cex = 1.5)
  abline(lm(oc_budget_grouped$hypo_doc_resp_total ~ as.numeric(as.character(oc_budget_grouped$year))), lty = 2, lwd = 0.5)
  
  lines(oc_budget_grouped$year,oc_budget_grouped$hypo_resp_sed_total,type = 'l',col = 'darkorange3', lwd = 2)
  points(oc_budget_grouped$year,oc_budget_grouped$hypo_resp_sed_total,col = 'darkorange3', cex = 1.5)
  abline(lm(oc_budget_grouped$hypo_resp_sed_total ~ as.numeric(as.character(oc_budget_grouped$year))),lty = 2, lwd = 0.5)
}

text(x=(oc_budget_grouped$year[2]), y=35, lake_title, col='black', cex=1.5) 

if (lake_title == 'TR'){

legend("topright", c("RPOC","RDOC","RSED"),
       col=c("chartreuse4","lightcoral","darkorange3"), lwd=3, pt.cex = 1, cex = 0.75)
}
}

par(mfrow=c(3,2), mai = c(0.25, 0.3, 0.1, 0.05))

oc_temporal_plot(me_hypo_fluxes,me_driving,'ME')
oc_temporal_plot(mo_hypo_fluxes,mo_driving,'MO')
oc_temporal_plot(tr_hypo_fluxes,me_driving,'TR')
oc_temporal_plot(al_hypo_fluxes,al_driving,'AL')
oc_temporal_plot(bm_hypo_fluxes,bm_driving,'BM')
oc_temporal_plot(sp_hypo_fluxes,sp_driving,'SP')

dev.off()

Rsed_do_test <- function(hypo_vars,hypo_fluxes,driving_df){
resp_calc <- hypo_vars %>% left_join(hypo_fluxes,by = c('sampledate')) %>% 
  mutate(do_value_conc = do_value/driving_df$volume_hypo)

do_limit_df <- resp_calc[1:which(resp_calc$do_value_conc < 1)[1],]

rsed_mean <- mean(resp_calc[1:which(resp_calc$do_value_conc < 1)[1],'resp_sed']$resp_sed,na.rm = TRUE)

sed_test <- resp_calc
sed_test[nrow(do_limit_df):nrow(resp_calc),'resp_sed'] <- rsed_mean

print('assume Rsed with anerobic resp')
print(sum(resp_calc$resp_sed, na.rm = TRUE) - sum(do_limit_df$resp_total + do_limit_df$resp_sed, na.rm = TRUE))
print(paste((round(sum(resp_calc$resp_sed, na.rm = TRUE)/sum(do_limit_df$resp_total + do_limit_df$resp_sed, na.rm = TRUE),3))*100,'%',sep = ''))

print('assume Rsed with aerobic resp') #WE SET RSED AFTER ANOXIA EQUAL TO AVERAGE OF RSED BEFROE ANOXIA
print(sum(sed_test$resp_sed, na.rm = TRUE) - sum(do_limit_df$resp_total + do_limit_df$resp_sed, na.rm = TRUE))
print(paste((round(sum(sed_test$resp_sed, na.rm = TRUE)/sum(do_limit_df$resp_total + do_limit_df$resp_sed, na.rm = TRUE),3))*100,'%',sep = ''))
}


Rsed_do_test(me_hypo_vars_year,me_hypo_fluxes_year,me_driving_year)
Rsed_do_test(mo_hypo_vars_year,mo_hypo_fluxes_year,mo_driving_year)



##MAKE TEMPORAL PLOT OF FIG 5 -- AUTO VS HETERO TROPHY STATUS

trophy_temporal_plot <- function(epi_fluxes, hypo_fluxes, driving_data, lake_title, y_title){
  
  ts_breakdown_epi <- epi_fluxes
  ts_breakdown_hypo <- hypo_fluxes
  
  #first split up date to be just years
  ts_breakdown_epi$year <- format(as.Date(ts_breakdown_epi$sampledate, format="%Y-%m-%d"),"%Y")
  ts_breakdown_hypo$year <- format(as.Date(ts_breakdown_hypo$sampledate, format="%Y-%m-%d"),"%Y")
  
  #group by year
  oc_grouped_epi <- ts_breakdown_epi %>% 
    group_by(year) %>% 
    mutate(resp_total_epi = sum(resp_sed + resp_poc_l + resp_poc_r + resp_doc_l + resp_doc_r, na.rm = TRUE)) %>% 
    mutate(autoch_total_epi = sum(npp_value, na.rm = TRUE)) %>%   
    ungroup() %>% 
    distinct(year, .keep_all = TRUE) %>% 
    select(year, resp_total_epi, autoch_total_epi)
  
  #group by year
  oc_grouped_hypo <- ts_breakdown_hypo %>% 
    group_by(year) %>% 
    mutate(resp_total_hypo = sum(resp_sed + resp_poc_l + resp_poc_r + resp_doc_l + resp_doc_r, na.rm = TRUE)) %>% 
    mutate(autoch_total_hypo = sum(npp_value, na.rm = TRUE)) %>%   
    ungroup() %>% 
    distinct(year, .keep_all = TRUE) %>% 
    select(year, resp_total_hypo, autoch_total_hypo)
  
  
  oc_group <- oc_grouped_epi %>% left_join(oc_grouped_hypo, by = c('year')) %>% 
    mutate(resp_total = (resp_total_epi + resp_total_hypo)/(driving_data$area_td[1])) %>% 
    mutate(autoch_total = (autoch_total_epi + autoch_total_hypo)/(driving_data$area_td[1])) %>% 
    mutate(troph_value = (autoch_total - resp_total)) 
  
  
  if (y_title == 1){
    plot(oc_group$year,oc_group$autoch_total,type = 'l',col = '#009E73', ylim = c(min(oc_group$resp_total),max(oc_group$autoch_total)),
         ylab = 'Flux',xlab = NA,xaxt = "n", lwd = 2)
    lines(oc_group$year,(oc_group$resp_total),type = 'l',col = '#D55E00', lwd = 2)
    points(oc_group$year,oc_group$autoch_total, col = '#009E73')
    points(oc_group$year,oc_group$resp_total, col = '#D55E00')
    abline(0,0, lty=2,lwd=1)
    text(x=(oc_group$year[3]), y=max(oc_group$autoch_total) - 10, lake_title, col='black', cex=2)

    if (lake_title == 'ME'){
      legend("topright", c("NPP","R"),
             col=c("#009E73","#D55E00"), lwd=3, pt.cex = 1, cex = 1)
    }

    plot(oc_group$year,oc_group$troph_value,type = 'l',ylab = 'NEP', lwd = 2, xlab  = 'NA')
    points(oc_group$year,oc_group$troph_value)
    abline(0,0, lty=2,lwd=1)


  }else if (y_title == 0){
    plot(oc_group$year,oc_group$autoch_total,type = 'l',col = '#009E73', ylim = c(min(oc_group$resp_total),max(oc_group$autoch_total)),
         xlab = NA,xaxt = "n", ylab = NA, lwd = 2)
    lines(oc_group$year,(oc_group$resp_total),type = 'l',col = '#D55E00', lwd = 2)
    points(oc_group$year,oc_group$autoch_total, col = '#009E73')
    points(oc_group$year,oc_group$resp_total, col = '#D55E00')
    abline(0,0, lty=2,lwd=1)
    text(x=(oc_group$year[3]), y=max(oc_group$autoch_total) - 10, lake_title, col='black', cex=2)
  
    plot(oc_group$year,oc_group$troph_value,type = 'l',ylab = NA, xlab = NA, lwd = 2)
    points(oc_group$year,oc_group$troph_value)
    abline(0,0, lty=2,lwd=1)
  
      }else{
    print ('label error')
  }  
  
  
}

par(mfrow=c(2,1), mai = c(0.1, 1.2, 0.05, 0.05), omi=c(0.5,0.1,0.2,0.1))#, cex = 1.5)
trophy_temporal_plot(me_epi_fluxes,me_hypo_fluxes,me_driving,'ME',1)

par(mfrow=c(2,1), mai = c(0.1, 1.2, 0.05, 0.05), omi=c(0.5,0.1,0.2,0.1))
trophy_temporal_plot(mo_epi_fluxes,mo_hypo_fluxes,mo_driving,'MO',0)

par(mfrow=c(2,1), mai = c(0.1, 1.2, 0.05, 0.05), omi=c(0.5,0.1,0.2,0.1))
trophy_temporal_plot(tr_epi_fluxes,tr_hypo_fluxes,tr_driving,'TR',0)

par(mfrow=c(2,1), mai = c(0.1, 1.2, 0.05, 0.05), omi=c(0.5,0.1,0.2,0.1))
trophy_temporal_plot(al_epi_fluxes,al_hypo_fluxes,al_driving,'AL',1)

par(mfrow=c(2,1), mai = c(0.1, 1.2, 0.05, 0.05), omi=c(0.5,0.1,0.2,0.1))
trophy_temporal_plot(bm_epi_fluxes,bm_hypo_fluxes,bm_driving,'BM',0)

par(mfrow=c(2,1), mai = c(0.1, 1.2, 0.05, 0.05), omi=c(0.5,0.1,0.2,0.1))
trophy_temporal_plot(sp_epi_fluxes,sp_hypo_fluxes,sp_driving,'SP',0)




##plot hydrology over time

par(mfrow=c(6,1), mai = c(0.05, 0.1, 0.03, 0.05), omi=c(0.3,0.3,0.05,0.05)) #cex = 1.5)#mai = c(0.8, 0.8, 0.3, 0.2)) #mar= c(4,4,0,1)) 

plot(me_driving$sampledate,me_driving$inflow_vol/10000,type = 'l', ylab = NA, xlab = NA, xaxt = "n")
text(x=(me_driving$sampledate[100]), y=(max(me_driving$inflow_vol)/10000) -100, 'ME', col='black', cex=1.5) 

plot(me_driving$sampledate,me_driving$outflow_vol/10000,type = 'l', ylab = NA, xlab = NA, xaxt = "n")
text(x=(me_driving$sampledate[100]), y=(max(me_driving$inflow_vol,na.rm = TRUE)/10000) -100, 'MO', col='black', cex=1.5) 

plot(tr_driving$sampledate,tr_driving$inflow_vol/10000,type = 'l', ylab = NA, xlab = NA, xaxt = "n")
text(x=(tr_driving$sampledate[100]), y=(max(tr_driving$inflow_vol)/10000) -50, 'TR', col='black', cex=1.5) 

plot(al_driving$sampledate,al_driving$inflow_vol/10000,type = 'l', ylab = NA, xlab = NA)#, xaxt = "n")
text(x=(al_driving$sampledate[100]), y=(max(al_driving$inflow_vol)/10000) -10, 'AL', col='black', cex=1.5) 

plot(bm_driving$sampledate,bm_driving$inflow_vol/10000,type = 'l', ylab = NA, xlab = NA, xaxt = "n")
text(x=(bm_driving$sampledate[100]), y=(max(bm_driving$inflow_vol)/10000) -10, 'BM', col='black', cex=1.5) 

plot(sp_driving$sampledate,sp_driving$inflow_vol/10000,type = 'l', ylab = NA, xlab = 'Date')
text(x=(sp_driving$sampledate[100]), y=(max(sp_driving$inflow_vol)/10000) -3, 'SP', col='black', cex=1.5) 

dev.off()




##Plot TP over time
par(mfrow=c(3,1), mai = c(0.1, 0.1, 0.03, 0.05), omi=c(0.3,0.3,0.05,0.05)) #cex = 1.5)#mai = c(0.8, 0.8, 0.3, 0.2)) #mar= c(4,4,0,1)) (c(bottom,right,top,left ))

plot(me_tp$sampledate,me_tp$tp_epi_approx,type = 'l',xaxt = 'n')
tp_m <- mean(me_tp$tp_epi_approx, na.rm = TRUE)
abline(h=tp_m, lty = 2, lwd = 0.8)
text(x=(me_tp$sampledate[100]), y=max(me_tp$tp_epi_approx) -20, 'ME', col='black', cex=1.5) 

plot(mo_tp$sampledate,mo_tp$tp_epi_approx,type = 'l',xaxt = 'n')
tp_m <- mean(mo_tp$tp_epi_approx, na.rm = TRUE)
abline(h=tp_m, lty = 2, lwd = 0.8)
text(x=(mo_tp$sampledate[100]), y=max(mo_tp$tp_epi_approx) -20, 'MO', col='black', cex=1.5) 

plot(tr_tp$sampledate,tr_tp$tp_epi_approx,type = 'l',xaxt = 'n')
tp_m <- mean(tr_tp$tp_epi_approx, na.rm = TRUE)
abline(h=tp_m, lty = 2, lwd = 0.8)
text(x=(tr_tp$sampledate[100]), y=max(tr_tp$tp_epi_approx) -2, 'TR', col='black', cex=1.5) 

plot(al_tp$sampledate,al_tp$tp_epi_approx,type = 'l')#,xaxt = 'n')
tp_m <- mean(al_tp$tp_epi_approx, na.rm = TRUE)
abline(h=tp_m, lty = 2, lwd = 0.8)
text(x=(al_tp$sampledate[100]), y=max(al_tp$tp_epi_approx) -5, 'AL', col='black', cex=1.5) 

plot(bm_tp$sampledate,bm_tp$tp_epi_approx,type = 'l',xaxt = 'n', ylim = c(0,30))
tp_m <- mean(bm_tp$tp_epi_approx, na.rm = TRUE)
abline(h=tp_m, lty = 2, lwd = 0.8)
text(x=(bm_tp$sampledate[100]), y=max(bm_tp$tp_epi_approx) -27, 'BM', col='black', cex=1.5) 

plot(sp_tp$sampledate,sp_tp$tp_epi_approx,type = 'l', ylab = NA, xlab= NA, ylim = c(0,20))
tp_m <- mean(sp_tp$tp_epi_approx, na.rm = TRUE)
abline(h=tp_m, lty = 2, lwd = 0.8)
text(x=(sp_tp$sampledate[100]), y=max(sp_tp$tp_epi_approx) -23, 'SP', col='black', cex=1.5) 


##perform MK analysis
library(trend)

nep_mk_analysis <- function(epi_fluxes, hypo_fluxes, driving_data, lake_name){
  
  ts_breakdown_epi <- epi_fluxes
  ts_breakdown_hypo <- hypo_fluxes
  driver_breakdown <- driving_data
  
  #first split up date to be just years
  ts_breakdown_epi$year <- format(as.Date(ts_breakdown_epi$sampledate, format="%Y-%m-%d"),"%Y")
  ts_breakdown_hypo$year <- format(as.Date(ts_breakdown_hypo$sampledate, format="%Y-%m-%d"),"%Y")
  driver_breakdown$year <- format(as.Date(driving_data$sampledate, format="%Y-%m-%d"),"%Y")
  
  #group by year
  oc_grouped_epi <- ts_breakdown_epi %>% 
    group_by(year) %>% 
    mutate(resp_total_epi = sum(resp_sed + resp_poc_l + resp_poc_r + resp_doc_l + resp_doc_r, na.rm = TRUE)) %>% 
    mutate(autoch_total_epi = sum(npp_value, na.rm = TRUE)) %>%   
    ungroup() %>% 
    distinct(year, .keep_all = TRUE) %>% 
    select(year, resp_total_epi, autoch_total_epi)
  
  #group by year
  oc_grouped_hypo <- ts_breakdown_hypo %>% 
    group_by(year) %>% 
    mutate(resp_total_hypo = sum(resp_sed + resp_poc_l + resp_poc_r + resp_doc_l + resp_doc_r, na.rm = TRUE)) %>% 
    mutate(autoch_total_hypo = sum(npp_value, na.rm = TRUE)) %>%   
    ungroup() %>% 
    distinct(year, .keep_all = TRUE) %>% 
    select(year, resp_total_hypo, autoch_total_hypo)
  
  
  tp_grouped_epi <- driver_breakdown %>% 
    drop_na(thermocline_depth) %>% 
    group_by(year) %>% 
    mutate(tp_avg = mean(tp_epi_approx)) %>% 
    ungroup() %>% 
    distinct(year, .keep_all=TRUE) %>% 
    select(year, tp_avg)

  
  oc_tp_join <- oc_grouped_epi %>% left_join(oc_grouped_hypo, by = c('year'))
  
  oc_group <- oc_grouped_epi %>% 
    left_join(oc_grouped_hypo, by = c('year')) %>% 
    left_join(tp_grouped_epi, by = c('year')) %>% 
    mutate(resp_total = (resp_total_epi + resp_total_hypo)/(driving_data$area_td[1])) %>% 
    mutate(autoch_total = (autoch_total_epi + autoch_total_hypo)/(driving_data$area_td[1])) %>% 
    mutate(nep_value = (autoch_total - resp_total)) 

  mk_nep <- mk.test(oc_group$autoch_total)
  mk_tp <- mk.test(oc_group$tp_avg)
  
  mk_df <- data.frame('lake' = lake_name, 'npp_z' = as.numeric(mk_nep[[3]]),'npp_pvalue' = mk_nep[[2]], 
                      'avg_epi_tp_z' = as.numeric(mk_tp[[3]]), 'avg_tp_epi_pvalue' = mk_tp[[2]])
  return(mk_df)
}


me_mk <- nep_mk_analysis(me_epi_fluxes,me_hypo_fluxes,me_driving,'ME')
mo_mk <- nep_mk_analysis(mo_epi_fluxes,mo_hypo_fluxes,mo_driving,'MO')
tr_mk <- nep_mk_analysis(tr_epi_fluxes,tr_hypo_fluxes,tr_driving,'TR')
al_mk <- nep_mk_analysis(al_epi_fluxes,al_hypo_fluxes,al_driving,'AL')
bm_mk <- nep_mk_analysis(bm_epi_fluxes,bm_hypo_fluxes,bm_driving,'BM')
sp_mk <- nep_mk_analysis(sp_epi_fluxes,sp_hypo_fluxes,sp_driving,'SP')

lake_mk_df <- rbind(me_mk,mo_mk,tr_mk,al_mk,bm_mk,sp_mk)

write.csv(lake_mk_df, '~/Documents/masters_work/model/metab_model/figures/mk_stats_table.csv', row.names = FALSE)
