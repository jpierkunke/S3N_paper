# Ohio River Basin case study

library(sf) # for reading and possibly writing shapefiles
library(plyr) # for rbind.fill
library(knitr) # for making tables
library(beepr) # for notifying when a task is done
library(igraph) # for making the adjacency matrix
library(tictoc) # for benchmarking (timing to evaluate efficiency/runtime)
library(Matrix) # for efficient sparse adjacency matrix operations
library(mapview) # for visualizing spatial data
library(tidyverse) # for efficient data manipulation
library(shapefiles) # for read.dbf
library(magrittr) # for the eager pipe
library(cowplot) # for side-by-side maps with shared legend
library(latex2exp) # for plot label formatting
library(egg) # for ggarrange
library(S3N) # S3N estimation and prediction

source("S3N_functions.R")

# set file paths
data_path = "input_data/"
covs_path = paste0(data_path, "envir_covs/") # environmental covariate data
streams_path = paste0(data_path, "flowlines/")  # flowline layer for Region 5
pred_path = paste0(data_path, "predpoints/") # prediction point layer for Region 5
comid_huc12_path = paste0(data_path, "match_COMID_to_HUC12/") # data matching COMIDs to HUC12s
fish_path = paste0(data_path, "fish/") # fish survey data

# default location for where to save preprocessing output from this script
preproc_output = "pwdists/input/"
# where to save data to use as input in computing pairwise distances
pwdist_input_dir = "pwdists/input/"
# where to find the preds-obs pairwise distance results from the cluster
pwdist_predsobs_dir = "pwdists/output_predsobs/"
# where to find the obs-obs pairwise distance results from the cluster
pwdist_obsobs_dir = "pwdists/output_obsobs/"
est_pred_results_dir = "Region5_results/"

# envir covars preprocessing ----------------------------------------------

# only need to do this once unless the covariates change; uncomment if needed

# # combine environmental covariates and write to csv file
# # note default input: comid_huc12_filename = "HUC12_PU_COMIDs_CONUS.csv"
# combine_and_write_national_covariate_data(covs_path, comid_huc12_path)

# streams data preprocessing ----------------------------------------------

# read in streams flowlines for a given region,
# join the combined envir covariate data,
# reassign duplicate COMIDs as negative COMIDs,
# impute and rename covariates,
# write imputed renamed covariates to file
tic("streams data preprocessing")
streams = read_sf(streams_path, "Flowline_MS05_NSI") %!>% # 169463 obs of 16 vars
  # must join envir covar before reassigning dup COMIDs or else the dup COMIDs won't get HUC codes or envir covars
  join_envir_covar_data(covs_path) %!>%
  reassign_dup_COMIDs() %!>%
  impute_and_rename_covars()
toc() # streams data preprocessing: 13.156 sec elapsed

# build the stream network -----------------------------------------------

tic("configure stream network")
streams_res = configure_stream_network(streams)
streams = streams_res$streams
stream_graphs = streams_res$sg
# now streams has 169463 obs of 36 vars
# outflow:
#     0      1 
#    63 169400
# inflow:
#     0     1     2     3 
# 64095 41337 64030     1
# This network has 63 separate components.

## explore the complex confluence -----------------------------------------
complex_conf = names(which(table(streams$dnstream_node) == 3))
# "1151540.2996 2059592.6663"
streams$complex = (streams$dnstream_node == complex_conf)
# mapview(filter(streams, HUC8 == "05040002"), zcol = c("complex", "COMID"))

## remove smallest branch of complex confluence ---------------------------
# - COMID 167484513 and all reaches upstream of it
complex_conf = 167484513
upnd1 = streams$upstream_node[streams$COMID == complex_conf]
dnnd1 = streams$COMID[streams$dnstream_node == upnd1] # 15411283
upnd2 = streams$upstream_node[streams$COMID == dnnd1]
dnnd2 = streams$COMID[streams$dnstream_node == upnd2] # 15409971 15410029
upnd3 = streams$upstream_node[streams$COMID %in% dnnd2]
dnnd3 = streams$COMID[streams$dnstream_node %in% upnd3] # 15409975 15409973
upnd4 = streams$upstream_node[streams$COMID %in% dnnd3]
dnnd4 = streams$COMID[streams$dnstream_node %in% upnd4] # 15409981 15411355
upnd5 = streams$upstream_node[streams$COMID %in% dnnd4]
dnnd5 = streams$COMID[streams$dnstream_node %in% upnd5] # 15411353 15411309
upnd6 = streams$upstream_node[streams$COMID %in% dnnd5]
dnnd6 = streams$COMID[streams$dnstream_node %in% upnd6] # empty

COMIDs_to_drop = c(complex_conf, 
                   dnnd1, 
                   dnnd2, 
                   dnnd3, 
                   dnnd4, 
                   dnnd5, 
                   dnnd6)

# remove these 10 COMIDs and also any COMIDs outside of the main component
streams2 = streams %>%
  filter(!(COMID %in% COMIDs_to_drop), componentID == 1)
# now streams has 169094 obs (369 fewer COMIDs)
100*nrow(streams2)/nrow(streams) # we are still keeping 99.78% of the network
streams = streams2; rm(streams2)

## build the stream network again -----------------------------------------

streams_res = configure_stream_network(streams)
streams = streams_res$streams
stream_graphs = streams_res$sg
rm(streams_res)
toc() # configure stream network: 124.363 sec elapsed
# still 169094 obs of 36 vars
# outflow:
#     0      1 
#     1 169093 
# inflow:
#     0      1      2 
# 63905  41285  63904 
# This network has 1 component.

# Compute stream reach upstream distance and AFV --------------------------

tic("Stream updist and AFV")
streams = compute_stream_updist_vars(streams, stream_graphs)
toc() # Stream updist and AFV: 78.175 sec elapsed
# ~1280 iterations

# save(streams, file = paste0(data_path, "streams_for_Region5_results.rda"))

# Add observation points to LSN, compute site updist and AFV --------------

# load(paste0(data_path, "streams_for_Region5_results.rda"))

tic("Add obs to LSN")
# load preprocessed fish data
# read_fish_data(fish_path, with_cutoff_year = TRUE) # preprocessing took 281.165 sec
load(paste0(fish_path, "combined_fish_data_starting_1990.RData"))
# prep_to_compute_pwdist_region5 accomplishes several tasks:
# - reads in and preprocess prediction points layer, add updist and AFV
# - joins geometry, updist and AFV from pred points layer to fish dataset and
#   selects only unique points to create the obs points layer
prep_to_compute_pwdist_region5(streams, fish, pred_path,
                               out_dir = pwdist_input_dir)
toc() # Add obs to LSN: 21.916 sec elapsed

# Compute obs-obs pairwise distances and neighbor variables ---------------

# run this part of the code on a research cluster

# obs-obs distances are used in both estimation and prediction
# neighbor variables are just for estimation

# tic("Obs-obs distances")
# system('cd pwdists; ./scripts/all_obsobs.sh')
# toc() # Obs-obs distances: sec elapsed
# 
# # Compute preds-obs pairwise distances and neighbor variables for prediction --
# 
# tic("Preds-obs distances")
# # determine batch number and size based on number of (unobserved) prediction points
# batch_info = get_preds_batch_info( nrow(preds) - nrow(obs) )
# m=10 # use 10 neighbors
# args = sprintf("%i %i %i", batch_info$n, batch_info$size, m)
# system(paste('cd pwdists; ./scripts/run_pwdist_locally.sh', args))
# system(paste('cd pwdists; ./scripts/combine_pred_neighbors.sh', args))
# toc() # Preds-obs distances: sec elapsed

# Prepare for estimation and prediction -----------------------------------

# do this once for Region 5, not once per species

# add covariates to preds and obs and save to file
load(paste0(pwdist_input_dir, "preds_obs_pwdist_input_data_Region5.RData"))
# names(preds); names(obs) # they're the same
# # [1] "ptID"      "COMID"     "networkID" "binaryID"  "up_dist"   "log_AFV"   "geometry" 
# names(streams) # 44 variables

# add covariates to preds
preds = left_join(
  preds,
  streams %>%
    st_drop_geometry() %>%
    data.frame() %>%
    # need LENGTHKM later for scaling up from point predictions to regional estimate
    select(ptID, LENGTHKM, Development, Agriculture, Elevation, Water_Area, 
           Ann_Runoff, Baseflow, Ann_Temp, Hydro_Alter, Fldplain_Dis),
  by="ptID"
)

# add covariates to obs
obs = left_join(
  obs,
  streams %>%
    st_drop_geometry() %>%
    data.frame() %>%
    select(ptID, Development, Agriculture, Elevation, Water_Area, 
           Ann_Runoff, Baseflow, Ann_Temp, Hydro_Alter, Fldplain_Dis),
  by="ptID"
)

# add to fish
load(paste0(fish_path, "combined_fish_data_starting_1990.RData"))
fish_region5 = fish %>%
  filter(COMID %in% preds$COMID) %>%
  left_join(streams %>% st_drop_geometry() %>% select(COMID, LENGTHKM)) %>%
  compute_fish_reach_count_from_density()

# table(fish_region5$Source)
# FishScales GiamOlden16 
# 164645        7533 

save(preds, obs, fish_region5, 
     file = paste0(pwdist_input_dir, "preds_obs_fish_for_estimation.RData"))

count_each_species_in_fish(
  fish_region5,
  out_dir = est_pred_results_dir, 
  filename = "nobs_by_species", 
  write_to_csv = TRUE,
  write_to_rda = TRUE,
  return_obj = FALSE)




# Estimation and prediction for each species ------------------------------------

# for previous version of code, see Region5_est_pred_pipeline.R located in
# ~/Documents/UW/Research/Julian-Olden-SAFS/BRISC_stream/extras/

pwdist_obsobs_dir = "Region5_cluster_results/"
pwdist_predsobs_dir = "Region5_cluster_results/"

# needed for estimation and prediction: preds_obs_fish_for_estimation.RData loads preds, obs, fish_region5
# - if you also want streams, load this first then overwrite preds and obs with the next line
load(paste0(pwdist_input_dir, "preds_obs_pwdist_input_data_Region5.RData"))
load(paste0(pwdist_input_dir, "preds_obs_fish_for_estimation_Region5.RData"))

# needed for estimation: loads obs_neighbors
load(paste0(pwdist_obsobs_dir, "obs_neighbors.rda"))

# needed for prediction: loads obsobs_dist, obsobs_wt
load(paste0(pwdist_obsobs_dir, "obsobs_dist_wt.rda"))
# moved the next two lines from BRISC_prediction_stream to here
obsobs_dist = obsobs_dist + t(obsobs_dist)
obsobs_wt = obsobs_wt + t(obsobs_wt) - diag(nrow(obs)) # this is the slow step (4 sec)
# needed for prediction: loads pred_neighbors
load(paste0(pwdist_predsobs_dir, "pred_neighbors.rda"))

## for all species ----------------------------

load(paste0(est_pred_results_dir, "nobs_by_species.rda"))
# load(paste0(est_pred_results_dir, "Region5_est_pred_results.RData"))

# results_by_species = list()
bs = list()
nspecies = length(nobs_by_species$Common_Name)

mem.maxVSize(20000)
tic("Estimation and prediction for all species")
for(s_ind in 1:nspecies){ #1:nspecies){ #for(s_ind in 1:nspecies){ # for(s_ind in 1:5){
  common_name = nobs_by_species$Common_Name[s_ind]
  cat("\n----------------------------------------------", fill = TRUE)
  cat(paste0("Species ", s_ind, " of ", nspecies, ": ", common_name), fill = TRUE)
  cat("----------------------------------------------", fill = TRUE)
  
  # results = tryCatch({
  #   get_regional_estimate_for_species(
  #     common_name, 
  #     fish_region5, obs, preds,
  #     obs_neighbors, pred_neighbors,
  #     obsobs_dist, obsobs_wt,
  #     nugget_status = 1, verbose = FALSE)
  # }, error = function(err) {
  #   # error handler picks up where error was generated
  #   print(err)
  #   return(NA)
  # })
  
  results = tryCatch({
    BRISC_bootstrap(results_by_species[[common_name]]$estimation, n_boot = 20)
  }, error = function(err) {
    # error handler picks up where error was generated
    print(err)
    return(NA)
  })
  
  bs[[common_name]] = results
  
}
toc()
# save(results_by_species,
#      file = paste0(est_pred_results_dir, "Region5_est_pred_results.RData"))
save(bs,
     file = paste0(est_pred_results_dir, "Region5_bs.RData"))
beep(3)


## set all weights to 1 ----------------------------

load(paste0(est_pred_results_dir, "nobs_by_species.rda"))

results_by_species = list()
nspecies = length(nobs_by_species$Common_Name)

# set weights to 1:
obsobs_wt = matrix(1, nrow = n, ncol = n)
obs_neighbors$nnWt = rep(1, length(obs_neighbors$nnWt))
obs_neighbors$Dwt = rep(1, length(obs_neighbors$Dwt))
pred_neighbors$nnWght = rep(1, length(pred_neighbors$nnWght))

mem.maxVSize(20000)
tic("Estimation and prediction for all species")
for(s_ind in 1:nspecies){ #for(s_ind in 1:nspecies){ # for(s_ind in 1:5){
  common_name = nobs_by_species$Common_Name[s_ind]
  cat("\n----------------------------------------------", fill = TRUE)
  cat(paste0("Species ", s_ind, " of ", nspecies, ": ", common_name), fill = TRUE)
  cat("----------------------------------------------", fill = TRUE)
  
  results = tryCatch({
    get_regional_estimate_for_species(
      common_name, 
      fish_region5, obs, preds,
      obs_neighbors, pred_neighbors,
      obsobs_dist, obsobs_wt,
      nugget_status = 1, verbose = FALSE)
  }, error = function(err) {
    # error handler picks up where error was generated
    print(err)
    return(NA)
  })
  
  results_by_species[[common_name]] = results
  
}
toc()
save(results_by_species,
     file = paste0(est_pred_results_dir, "Region5_est_pred_results.RData"))
beep(3)


# Diagnostics ----------------------

load("pwdists/input/preds_obs_pwdist_input_data_Region5.RData")
load("pwdists/input/preds_obs_fish_for_estimation_Region5.RData")
load("Region5_results/Region5_est_pred_results.RData")
load("Region5_results/nobs_by_species.rda")

# update Nov 14 2025: remove 24 species that are identified only at the genus level
results_by_species = results_by_species[!str_detect(tolower(names(results_by_species)), "genus")]
results_by_species = results_by_species[!str_detect(tolower(names(results_by_species)), "subspecies")]

q = nrow(preds)
n = n_distinct(fish_region5$COMID)
# update Nov 14 2025: remove 24 species that are identified only at the genus level
nobs_by_species = nobs_by_species[!str_detect(tolower(nobs_by_species$Common_Name), "genus"),]
nobs_by_species = nobs_by_species[!str_detect(tolower(nobs_by_species$Common_Name), "subspecies"),]
n_species = nrow(nobs_by_species)

# col 1 = COMID, cols 2 through end are the 309 different species observed in Region 5
density_by_site_obs = matrix(nrow = n, ncol = n_species+1)
density_by_site_obs[,1] = preds$COMID[1:n]

density_by_site_pred = matrix(nrow = n, ncol = n_species+1)
density_by_site_pred[,1] = preds$COMID[1:n]

total_counts = data.frame(
  # species name
  species = nobs_by_species$Common_Name,
  # number of COMIDs on which this species was observed
  nCOMIDs = nobs_by_species$nCOMIDs,
  # proportion of COMIDs on which any fish was observed on which
  # *THIS* species was observed
  p_COMIDs_with_obs = nobs_by_species$nCOMIDs/n,
  # total observed count
  obs_total_count = nobs_by_species$TotalCount,
  # total predicted count on COMIDs where *any fish* were observed
  pred_total_count_wherefishobs = NA,
  # total predicted count on COMIDs where *this species* was observed
  pred_total_count_wherethisspecobs = NA,
  # total predicted Region 5 population size
  # (sum of 10*DensityPer100m*LENGTHKM over COMIDs with nonnegative predicted densities)
  regional_estimate = NA,
  # proportion of predicted densities that are <= -1
  # total_counts$p_neg[s] = sum(DensityPer100m <= -1)/q
  p_neg = NA,
  # proportion of predicted densities *on COMIDs with observations of any
  # fish species* that are <= -1
  # sum(DensityPer100m[1:n] <= -1)/n
  p_neg_obs = NA,
  # proportion of predicted densities *on COMIDs with observations of any
  # fish species* for which (1) predicted density <= -1 *AND* (2) this species
  # was not observed
  # sum(DensityPer100m[1:n][obs_density==0] < -1)/n
  p_neg_obs0 = NA,
  # median of the negative predicted densities
  # median(DensityPer100m[1:n][DensityPer100m[1:n] < 0])
  median_neg = NA,
  # median of the positive predicted densities
  # median(DensityPer100m[1:n][DensityPer100m[1:n] > 0])
  median_pos = NA,
  # correlation between predicted and observed density on COMIDs in which
  # any fish species was observed
  # cor(DensityPer100m[1:n], obs_density)
  cor_obs_pred = NA,
  # correlation between predicted and observed density on COMIDs in which
  # *THIS* fish species was observed
  # cor(DensityPer100m[1:n][obs_density > 0], obs_density[obs_density > 0])
  cor_obs_pred_nz = NA,
  # estimation runtime
  estimation_time = NA,
  estimation_units = NA,
  # prediction runtime
  prediction_time = NA,
  prediction_units = NA
)

params = data.frame(
  beta_1 = NA,
  beta_2 = NA,
  beta_3 = NA,
  beta_4 = NA,
  beta_5 = NA,
  beta_6 = NA,
  beta_7 = NA,
  beta_8 = NA,
  beta_9 = NA,
  beta_10 = NA,
  theta_1 = NA,
  theta_2 = NA,
  theta_3 = NA
)

tic("diagnostics loop"); for(s in 1:nrow(total_counts)){ #for(s in 1:10){
  cat(paste0(s, " "))
  species = total_counts$species[s]
  
  if(!is.na(results_by_species[[s]][1])){
    
    DensityPer100m = results_by_species[[s]]$preds_supp$DensityPer100m_pred
    CountReachTotal = results_by_species[[s]]$preds_supp$CountReachTotal_pred
    obs_density = results_by_species[[s]]$preds_supp$obs_density
    
    # predicted total count
    total_counts$pred_total_count_wherefishobs[s] = sum(CountReachTotal[1:n][CountReachTotal[1:n] >=0])
    total_counts$pred_total_count_wherethisspecobs[s] = sum(CountReachTotal[1:n][CountReachTotal[1:n] >=0 & obs_density[1:n] > 0])
    total_counts$regional_estimate[s] = sum(CountReachTotal[CountReachTotal >=0])
    total_counts$p_neg[s] = sum(DensityPer100m <= -1)/q
    total_counts$p_neg_obs[s] = sum(DensityPer100m[1:n] <= -1)/n
    total_counts$p_neg_obs0[s] = sum(DensityPer100m[1:n][obs_density[1:n]==0] < -1)/n
    total_counts$median_neg[s] = median(DensityPer100m[1:n][DensityPer100m[1:n] < 0])
    total_counts$median_pos[s] = median(DensityPer100m[1:n][DensityPer100m[1:n] > 0])
    total_counts$cor_obs_pred[s] = cor(DensityPer100m[1:n], obs_density[1:n])
    total_counts$cor_obs_pred_nz[s] = cor(DensityPer100m[1:n][obs_density[1:n] > 0],
                                          obs_density[1:n][obs_density[1:n] > 0])
    # ensure all estimation and predictions times have the same units
    units(results_by_species[[s]]$estimation_time) = "secs"
    units(results_by_species[[s]]$prediction_time) = "secs"
    total_counts$estimation_time[s] = results_by_species[[s]]$estimation_time
    total_counts$prediction_time[s] = results_by_species[[s]]$prediction_time
    total_counts$prediction_units[s] = "secs"
    total_counts$estimation_units[s] = "secs"
    params[s,] = c(results_by_species[[s]]$estimation$Beta, results_by_species[[s]]$estimation$Theta)
    
    density_by_site_pred[, s+1] = DensityPer100m[1:n]
    density_by_site_obs[, s+1] = obs_density[1:n]
  }
}; cat("\n"); toc()
# diagnostics loop: 55.643 sec elapsed

diagnostics = cbind(total_counts, params) %>%
  dplyr::rename(
    Intercept = beta_1,
    Elevation = beta_2,
    Water_Area = beta_3,
    Ann_Runoff = beta_4,
    Baseflow = beta_5,
    Ann_Temp = beta_6,
    Development = beta_7,
    Agriculture = beta_8,
    Hydro_Alter = beta_9,
    Fldplain_Dis = beta_10,
    SigmaSq = theta_1,
    TauSq = theta_2,
    Phi = theta_3
  )

cor_density_by_site = rep(NA, n)
for(i in 1:n){
  cor_density_by_site[i] = cor(density_by_site_obs[i,2:(n_species+1)],
                               density_by_site_pred[i,2:(n_species+1)])
}

save(diagnostics, cor_density_by_site,
     file = "Region5_diagnostics.RData")


