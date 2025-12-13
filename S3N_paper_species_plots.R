# Ohio River Basin case study: generate plots and numbers for the paper

library(latex2exp)
library(tidyverse)
library(sf)
library(ggbreak) 
library(patchwork)
library(palettes)
library(paletteer)
library(scales)

load("pwdists/input/preds_obs_pwdist_input_data_Region5.RData")

# these results used 20 bootstrap reps
est_pred_results_dir = "Region5_results/"
load(paste0(est_pred_results_dir, "Region5_bs.RData"))
load(paste0(est_pred_results_dir, "nobs_by_species.rda"))

nspecies = length(nobs_by_species$Common_Name)
bs_results = data.frame(
  param = character(),
  lower = numeric(),
  upper = numeric(),
  signif = logical(),
  species = character(),
  s_ind = numeric()
)
bs_times = rep(NA, nspecies)

for(s_ind in 1:nspecies){
  this_species = bs[[s_ind]]$confidence.interval %>% 
    as.data.frame() %>%
    mutate(bound = c("lower", "upper")) %>% 
    pivot_longer(
      cols = beta_1:phi,
      names_to = "param",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = bound,
      values_from = value
    ) %>%
    mutate(
      signif = ifelse(
        (lower > 0 & upper > 0) | (lower < 0 & upper < 0),
        TRUE, FALSE),
      species = names(bs)[s_ind],
      s_ind = s_ind
    )
  
  bs_results = rbind(bs_results, this_species)
  
  bs_times[s_ind] = bs[[s_ind]]$boot.time[1]
}
summary(bs_times)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 39.34   51.51   58.86   84.72   75.72  247.13

load(paste0(est_pred_results_dir, "Region5_est_pred_results.RData"))

plot_colors = c("#FDC718FF", "#023743FF")

# this_species = "Southern Redbelly Dace"
# this_species = "Mottled Sculpin"
# this_species = "Slough Darter"
this_species = "Green Sunfish"
# this_species = "Brook Trout" # also need to edit species name below for map
this_data = filter(bs_results, species == this_species)
this_data$estimate = c(as.numeric(results_by_species[[this_species]]$estimation$Beta), 
                       as.numeric(results_by_species[[this_species]]$estimation$Theta))
this_data$param[1:10] = c(colnames(results_by_species[[this_species]]$estimation$X))
this_data$param[13] = "lambda"
this_data$estimate[13] = 1/this_data$estimate[13]
this_data$lower[13] = 1/this_data$lower[13]
this_data$upper[13] = 1/this_data$upper[13]
this_data # check estimates

# if(this_species == "Brook Trout"){
ind = 13; this_data$estimate[ind] = NA; this_data$lower[ind] = NA; this_data$upper[ind] = NA
# }
this_data = this_data[c(1:11, 13, 12),]

this_data$param = factor(this_data$param, levels = this_data$param[1:13])
this_data = this_data |>
  mutate(
    param = fct_recode(param,
                       "Tail-up variance" = "sigma.sq",
                       "Water area" = "Water_Area",
                       "Annual runoff" = "Ann_Runoff",
                       "Annual temperature" = "Ann_Temp",
                       "Hydrological alteration" = "Hydro_Alter",
                       "Floodplain integrity" = "Fldplain_Dis",
                       "Tail-up length scale" = "lambda",
                       "Nugget variance" = "tau.sq"
    )
  )

# plot all parameters, option 1/2: all params, with or without axis breaks
ggplot(this_data, 
       aes(x = estimate, y = param, color = signif, pch = signif)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper)) +
  ## scale_x_continuous(trans = pseudolog10_trans) +
  # scale_x_break(c(-0.05, -0.03), scales = 5) + # uncomment for axis breaks
  # scale_x_break(c(0.03, 0.25), scales = 1) + # uncomment for axis breaks
  scale_colour_manual(values = plot_colors) +
  labs(
    title = this_species,
    x = "Estimate",
    y = "Parameter"
  ) +
  theme_bw() +
  theme(legend.position = "none")
ggsave(filename = paste0("S3N_paper_species_plots/", this_species, "_params_option1.png"), 
       width = 6, height = 3, units = "in")


# # plot all parameters, option 3: just small parameter estimates
# ggplot(this_data[2:8,], 
#        aes(x = estimate, y = param, color = signif, pch = signif)) +
#   geom_point() +
#   geom_errorbar(aes(xmin = lower, xmax = upper)) +
#   scale_colour_manual(values = plot_colors) +
#   geom_vline(xintercept = 0, size = 0.2) +
#   labs(
#     title = this_species,
#     x = "Estimate",
#     y = "Parameter"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")

# plot all parameters, option 4: put all estimates on comparable scales
scale_factor = ceiling(abs(log(abs(this_data$estimate), base = 10)))
for(i in 1:length(scale_factor)){
  if(!is.na(sum(this_data[i,c("lower", "upper", "estimate")]))){
    this_data[i,c("lower", "upper", "estimate")] = this_data[i,c("lower", "upper", "estimate")]*10^scale_factor[i]
  }
}

ggplot(this_data, 
       aes(x = estimate, y = param, color = signif, pch = signif)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper)) +
  scale_x_break(c(-200, -20), scales = 5) + # uncomment for axis breaks
  scale_x_break(c(20, 200), scales = 1) + # uncomment for axis breaks
  scale_colour_manual(values = plot_colors) +
  geom_vline(xintercept = 0, size = 0.2) +
  labs(
    title = this_species,
    x = "Estimate",
    y = "Parameter"
  ) +
  theme_bw() +
  theme(legend.position = "none")
ggsave(filename = paste0("S3N_paper_species_plots/", this_species, "_params_option4.png"), width = 6, height = 3, units = "in")


plot_pred_density_map = function(streams, preds_supp, common_name, out_dir, 
                                 qtls = NULL, save_to_file = TRUE){
  usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) %>%
    st_transform(crs = st_crs(streams))
  
  # discrete
  streams = streams %>%
    left_join(select(preds_supp, COMID, DensityPer100m_pred), by="COMID") %>%
    mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred <= 1e-8, 1e-8, DensityPer100m_pred)) %>%
    mutate(log_density_pred = log10(DensityPer100m_pred))
  
  if(is.null(qtls)){
    message("No quantiles provided; computing now...")
    qtls = quantile(streams$log_density_pred, probs = seq(0,1,0.1))
    qtls = unique(round(qtls[qtls > -8], 2))
    print(qtls)
  } else {
    message("Quantiles provided:")
    print(qtls)
  }
  
  # plot of Subnetwork 6 with Subnetwork 5 highlighted in colors
  fish_map = ggplot() +
    geom_sf(data = usa,
            color = "#2b2b2b",
            fill = "white") +
    geom_sf(data = st_simplify(streams,
                               preserveTopology = FALSE,
                               dTolerance = 1000),
            aes(color = log_density_pred), linewidth = 0.3) +
    scale_color_steps(
      breaks = qtls,
      high = "#132B43",
      low = "#BFEFFF" #"#56B1F7"#,
    ) +
    coord_sf(xlim = c(-90, -75),
             ylim = c(35, 45),
             default_crs = sf::st_crs(4326)) +
    labs(
      title = common_name,
      color = TeX("$log_{10}$(Fish density per 100m)")) +
    ggthemes::theme_map() +
    theme(
      legend.position = "right"
    ) + 
    guides(colour = guide_coloursteps(show.limits = TRUE))
  
  if(save_to_file){
    ggsave(fish_map, filename = paste0(out_dir, "Region5_pred_density_map_", str_replace_all(common_name, " ", "_"), ".png"),
           width = 4, height = 4, units = "in")
  }
  
  return(fish_map)
}

# maps
plot_pred_density_map(
  streams,
  results_by_species$`Slough Darter`$preds_supp, 
  "Slough Darter", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map(
  streams,
  results_by_species$`Green Sunfish`$preds_supp, 
  "Green Sunfish", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map(
  streams,
  results_by_species$`Brook Trout`$preds_supp, 
  "Brook Trout", 
  out_dir = "S3N_paper_species_plots/"
)

# changes: plot densities instead of log densities, and set densities <1 fish/100 m to 0
plot_pred_density_map_updated = function(streams, preds_supp, common_name, out_dir, 
                                 qtls = NULL, save_to_file = TRUE){
  usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) %>%
    st_transform(crs = st_crs(streams))
  
  # discrete
  streams2 = streams %>%
    left_join(select(preds_supp, COMID, DensityPer100m_pred), by="COMID") %>%
    mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred <= 1e-8, 1e-8, DensityPer100m_pred)) %>%
    mutate(log_density_pred = log10(DensityPer100m_pred))
  print(summary(streams2$DensityPer100m_pred))
  print(mean(streams2$DensityPer100m_pred >= 1))
  
  if(is.null(qtls)){
    message("No quantiles provided; computing now...")
    qtls = quantile(streams2$log_density_pred, probs = seq(0,1,0.1))
    qtls = unique(round(qtls[qtls > -8], 2))
    # if(qtls[1]!= 0){qtls = c(0, qtls)}
    print(qtls)
  } else {
    message("Quantiles provided:")
    print(qtls)
  }
  
  # print("Breaks:")
  # print(qtls)
  # print("Labels:")
  # print(round(10^qtls, 4))
  
  # plot of Subnetwork 6 with Subnetwork 5 highlighted in colors
  fish_map = ggplot() +
    geom_sf(data = usa,
            color = "#2b2b2b",
            fill = "white") +
    geom_sf(data = st_simplify(streams2,
                               preserveTopology = FALSE,
                               dTolerance = 1000),
            aes(color = log_density_pred), linewidth = 0.3) +
    scale_color_steps(
      breaks = qtls,
      labels = function(b) round(10^b, 4),
      # labels = c(0, round(10^qtls, 4)),
      high = "#132B43",
      low = "#BFEFFF" #"#56B1F7"#,
    ) +
    coord_sf(xlim = c(-90, -75),
             ylim = c(35, 45),
             default_crs = sf::st_crs(4326)) +
    labs(
      title = common_name,
      color = TeX("Number of fish per 100m")) +
    ggthemes::theme_map() +
    theme(
      legend.position = "right"
    ) + 
    guides(colour = guide_coloursteps(show.limits = TRUE))
  
  if(save_to_file){
    ggsave(fish_map, filename = paste0(out_dir, "updated_Region5_pred_density_map_", str_replace_all(common_name, " ", "_"), ".png"),
           width = 4, height = 4, units = "in")
  }
  
  return(fish_map)
}

# maps
plot_pred_density_map_updated(
  streams,
  results_by_species$`Slough Darter`$preds_supp, 
  "Slough Darter", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated(
  streams,
  results_by_species$`Green Sunfish`$preds_supp, 
  "Green Sunfish", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated(
  streams,
  results_by_species$`Brook Trout`$preds_supp, 
  "Brook Trout", 
  out_dir = "S3N_paper_species_plots/"
)

# changes: plot densities instead of log densities, and set densities <1 fish/100 m to 0
plot_pred_density_map_updated2 = function(streams, preds_supp, common_name, out_dir, 
                                         qtls = NULL, save_to_file = TRUE){
  usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) %>%
    st_transform(crs = st_crs(streams))
  
  # discrete
  streams = streams %>%
    left_join(select(preds_supp, COMID, DensityPer100m_pred), by="COMID") %>%
    mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred <= 1, 1e-8, DensityPer100m_pred)) %>%
    mutate(log_density_pred = log10(DensityPer100m_pred))
  streams$standardized_log_density = (streams$log_density_pred - mean(streams$log_density_pred))/sd(streams$log_density_pred)
    # mutate(is_logdensity_gt0 = as.factor(log_density_pred > 0))
  print(summary(streams$DensityPer100m_pred))
  print(mean(streams$DensityPer100m_pred >= 1))
  print(sum(streams$DensityPer100m_pred >= 1))
  
  if(is.null(qtls)){
    message("No quantiles provided; computing now...")
    # qtls = quantile(streams$log_density_pred, probs = seq(0,1,0.1))
    # qtls = unique(round(qtls[qtls > -8], 2))
    # print(qtls)
    
    qtls = quantile(streams$log_density_pred[streams$log_density_pred >=0], probs = seq(0,1,0.25))
    qtls = unique(round(qtls[qtls > -8], 2))
    print(qtls)
    # print(table(qtls))
  } else {
    message("Quantiles provided:")
    print(qtls)
  }
  
  # plot of Subnetwork 6 with Subnetwork 5 highlighted in colors
  fish_map = ggplot() +
    geom_sf(data = usa,
            color = "#2b2b2b",
            fill = "white") +
    geom_sf(data = st_simplify(streams,
                               preserveTopology = FALSE,
                               dTolerance = 1000),
            aes(color = log_density_pred), linewidth = 0.3) +
            # aes(color = standardized_log_density), linewidth = 0.3) +
    scale_color_steps(
      breaks = qtls,
      labels = function(b) round(10^b, 1),
      # breaks = (qtls-mean(qtls))/sd(qtls),
      # labels = function(b) round(10^(sd(qtls)*b + mean(qtls)), 4),
      # labels = function(b) ifelse(
      #   10^(sd(qtls)*b + mean(qtls)) - 0.1 <= min(qtls),
      #   0,
      #   round(10^(sd(qtls)*b + mean(qtls)), 4)),
      high = "#132B43",
      low = "#BFEFFF" #"#56B1F7"#,
    ) +
    coord_sf(xlim = c(-90, -75),
             ylim = c(35, 45),
             default_crs = sf::st_crs(4326)) +
    labs(
      title = common_name,
      color = TeX("Number of fish per 100m")) +
    ggthemes::theme_map() +
    theme(
      legend.position = "right"
    ) + 
    guides(colour = guide_coloursteps(show.limits = TRUE))
  
  if(save_to_file){
    ggsave(fish_map, filename = paste0(out_dir, "updated2_Region5_pred_density_map_", str_replace_all(common_name, " ", "_"), ".png"),
           width = 4, height = 4, units = "in")
  }
  
  return(fish_map)
}


# maps
plot_pred_density_map_updated2(
  streams,
  results_by_species$`Slough Darter`$preds_supp, 
  "Slough Darter", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated2(
  streams,
  results_by_species$`Green Sunfish`$preds_supp, 
  "Green Sunfish", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated2(
  streams,
  results_by_species$`Brook Trout`$preds_supp, 
  "Brook Trout", 
  out_dir = "S3N_paper_species_plots/"
)

prop_ge1 = rep(NA, nrow(nobs_by_species)); max_density = rep(NA, nrow(nobs_by_species)); i=0
for(this_species in nobs_by_species$Common_Name){
  i=i+1; prop_ge1[i] = mean(results_by_species[[this_species]]$preds_supp$DensityPer100m_pred>=1)
  max_density[i] = max(results_by_species[[this_species]]$preds_supp$DensityPer100m_pred)
}
summary(prop_ge1)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000000 0.0000946 0.0006742 0.0777351 0.0070375 0.9425763 
log(max(max_density))
# 8.088125

nobs_by_species$prop_ge1 = prop_ge1
View(arrange(nobs_by_species, prop_ge1))



# changes: instead of quantiles, make the breakpoints for color when natural log of density = 0, 1, ..., 8 (up to max for that species)
max(results_by_species$`Slough Darter`$preds_supp$DensityPer100m_pred)
# [1] 32.45099
max(results_by_species$`Green Sunfish`$preds_supp$DensityPer100m_pred)
# [1] 230.6399
max(results_by_species$`Brook Trout`$preds_supp$DensityPer100m_pred)
# [1] 67.06079
log(max(results_by_species$`Slough Darter`$preds_supp$DensityPer100m_pred))
# [1] 3.479731
log(max(results_by_species$`Green Sunfish`$preds_supp$DensityPer100m_pred))
# [1] 5.440858
log(max(results_by_species$`Brook Trout`$preds_supp$DensityPer100m_pred))
# [1] 4.205599
plot_pred_density_map_updated3 = function(streams, preds_supp, common_name, out_dir, 
                                          breaks = NULL, save_to_file = TRUE){
  usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) %>%
    st_transform(crs = st_crs(streams))
  
  # discrete
  streams = streams %>%
    left_join(select(preds_supp, COMID, DensityPer100m_pred), by="COMID") %>%
    mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred <= 1, 1e-8, DensityPer100m_pred)) %>%
    mutate(log_density_pred = log(DensityPer100m_pred))
  
  if(is.null(breaks)){
    message("No breaks provided; computing now...")
    max_log_dens = max(streams$log_density_pred[streams$log_density_pred >=0])
    breaks = c(0:floor(max_log_dens), max_log_dens)
    print(breaks)
  } else {
    message("Breaks provided:")
    print(breaks)
  }
  
  # plot of Subnetwork 6 with Subnetwork 5 highlighted in colors
  fish_map = ggplot() +
    geom_sf(data = usa,
            color = "#2b2b2b",
            fill = "white") +
    geom_sf(data = st_simplify(streams,
                               preserveTopology = FALSE,
                               dTolerance = 1000),
            aes(color = log_density_pred), linewidth = 0.3) +
    # aes(color = standardized_log_density), linewidth = 0.3) +
    scale_color_steps(
      breaks = breaks,
      labels = function(b) round(exp(b), 1),
      # breaks = (qtls-mean(qtls))/sd(qtls),
      # labels = function(b) round(10^(sd(qtls)*b + mean(qtls)), 4),
      # labels = function(b) ifelse(
      #   10^(sd(qtls)*b + mean(qtls)) - 0.1 <= min(qtls),
      #   0,
      #   round(10^(sd(qtls)*b + mean(qtls)), 4)),
      high = "#132B43",
      low = "#BFEFFF" #"#56B1F7"#,
    ) +
    coord_sf(xlim = c(-90, -75),
             ylim = c(35, 45),
             default_crs = sf::st_crs(4326)) +
    labs(
      title = common_name,
      color = TeX("Number of fish per 100m")) +
    ggthemes::theme_map() +
    theme(
      legend.position = "right"
    ) + 
    guides(colour = guide_coloursteps(show.limits = TRUE))
  
  if(save_to_file){
    ggsave(fish_map, filename = paste0(out_dir, "updated3_Region5_pred_density_map_", str_replace_all(common_name, " ", "_"), ".png"),
           width = 4, height = 4, units = "in")
  }
  
  return(fish_map)
}


# maps
plot_pred_density_map_updated3(
  streams,
  results_by_species$`Slough Darter`$preds_supp, 
  "Slough Darter", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated3(
  streams,
  results_by_species$`Green Sunfish`$preds_supp, 
  "Green Sunfish", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated3(
  streams,
  results_by_species$`Brook Trout`$preds_supp, 
  "Brook Trout", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated3(
  streams,
  results_by_species$`Southern Redbelly Dace`$preds_supp, 
  "Southern Redbelly Dace", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated3(
  streams,
  results_by_species$`Mottled Sculpin`$preds_supp, 
  "Mottled Sculpin", 
  out_dir = "S3N_paper_species_plots/"
)


library(paletteer)
library(cartography)


# change: let's make the values factors so the colors can be more distinct
# in updated 2-3, most of the colors are all dark blue and it's hard to tell the difference between densities of 1 and 50
plot_pred_density_map_updated4 = function(streams, preds_supp, common_name, out_dir, 
                                          save_to_file = TRUE){
  usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) %>%
    st_transform(crs = st_crs(streams))

  streams = streams %>%
    left_join(select(preds_supp, COMID, DensityPer100m_pred), by="COMID") %>%
    mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred <= 1, 1e-8, DensityPer100m_pred)) %>%
    mutate(log_density_pred = log(DensityPer100m_pred))
  
  print(sum(streams$DensityPer100m_pred > 1))
  print(sum(streams$log_density_pred > 0))
  
  max_log_dens = max(streams$log_density_pred[streams$log_density_pred >= 0])
  breaks = c(0:floor(max_log_dens), max_log_dens)
  print("Breaks:")
  print(breaks)
  
  streams = streams %>%
    mutate(
      density_factor = factor(
        case_when(
          (log_density_pred < 0) ~ 0,
          (log_density_pred >= 0 & log_density_pred < 1) ~ 1,
          (log_density_pred >= 1 & log_density_pred < 2) ~ 2,
          (log_density_pred >= 2 & log_density_pred < 3) ~ 3,
          (log_density_pred >= 3 & log_density_pred < 4) ~ 4,
          (log_density_pred >= 4 & log_density_pred < 5) ~ 5,
          (log_density_pred >= 5 & log_density_pred < 6) ~ 6,
          (log_density_pred >= 6 & log_density_pred < 7) ~ 7,
          (log_density_pred >= 7 & log_density_pred < 8) ~ 8,
          (log_density_pred >= 8) ~ 8
        ),
        levels = 0:8 # levels = 8:0
      )
    )
  
  print(table(streams$density_factor))
  
  # return(streams)
  
  # plot of Subnetwork 6 with Subnetwork 5 highlighted in colors
  fish_map = ggplot() +
    geom_sf(data = usa,
            color = "#2b2b2b",
            fill = "white") +
    geom_sf(data = st_simplify(streams,
                               preserveTopology = FALSE,
                               dTolerance = 1000),
            aes(color = density_factor), linewidth = 0.3) +
            # aes(color = density_factor, linewidth = 0.1*as.integer(density_factor)+0.3)) +
    coord_sf(xlim = c(-90, -75),
             ylim = c(35, 45),
             default_crs = sf::st_crs(4326)) +
    # scale_color_paletteer_d("MoMAColors::Ernst") + 
    # scale_color_paletteer_d("colorBlindness::LightBlue2DarkBlue10Steps") +
    scale_color_paletteer_d(
      "cartography::blue.pal",
      # "fishualize::Aluterus_scriptus",
      direction = 1,
      dynamic = TRUE) +
    # paletteer_dynamic("cartography::blue.pal", n=n_distinct(streams$density_factor), direction = 1) +
    labs(
      title = common_name,
      color = TeX("Number of fish per 100m")) +
    ggthemes::theme_map() +
    theme(
      legend.position = "right"
    )

  if(save_to_file){
    ggsave(fish_map, filename = paste0(out_dir, "updated4_Region5_pred_density_map_", str_replace_all(common_name, " ", "_"), ".png"),
           width = 4, height = 4, units = "in")
  }

  return(fish_map)
}


# maps
plot_pred_density_map_updated4(
  streams,
  results_by_species$`Slough Darter`$preds_supp, 
  "Slough Darter", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated4(
  streams,
  results_by_species$`Green Sunfish`$preds_supp, 
  "Green Sunfish", 
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated4(
  streams,
  results_by_species$`Brook Trout`$preds_supp, 
  "Brook Trout", 
  out_dir = "S3N_paper_species_plots/"
)


# for now (Nov 2), I'll go with updated3


nhd <- read_csv("~/Documents/UW/Research/Julian-Olden-SAFS/Data_Region5/NHDPlusV2/nhd.csv") |>
  select(COMID, StreamOrde)

# before I set the 217 NAs to 1:
# table(streams$StreamOrde)
# # 1     2     3     4     5     6     7     8     9 
# # 91660 36181 19184 10866  6437  2609  1006   809   125 
# sum(is.na(streams$StreamOrde))
# # [1] 217

streams_orig = streams

streams = streams |>
  left_join(nhd, by="COMID") |>
  mutate(StreamOrde = ifelse(is.na(StreamOrde), 1, StreamOrde)) |>
  mutate(StreamOrde = factor(StreamOrde, levels = 1:9))

# now:
# table(streams$StreamOrde)
# # 1     2     3     4     5     6     7     8     9 
# # 91877 36181 19184 10866  6437  2609  1006   809   125 
# sum(is.na(streams$StreamOrde))
# # [1] 0

# plot_pred_density_map_updated5 = function(streams, preds_supp, common_name, out_dir, 
#                                           breaks = NULL, save_to_file = TRUE){
#   usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) %>%
#     st_transform(crs = st_crs(streams))
#   
#   # discrete
#   streams = streams %>%
#     left_join(select(preds_supp, COMID, DensityPer100m_pred), by="COMID") %>%
#     mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred <= 1, 1e-8, DensityPer100m_pred)) %>%
#     mutate(log_density_pred = log(DensityPer100m_pred))
#   
#   print(summary(streams$DensityPer100m_pred))
#   
#   # if(is.null(breaks)){
#   #   message("No breaks provided; computing now...")
#   #   max_log_dens = max(streams$log_density_pred[streams$log_density_pred >=0])
#   #   breaks = c(0:floor(max_log_dens), max_log_dens)
#   #   print(breaks)
#   # } else {
#   #   message("Breaks provided:")
#   #   print(breaks)
#   # }
#   
#   # plot of Subnetwork 6 with Subnetwork 5 highlighted in colors
#   fish_map = ggplot() +
#     geom_sf(data = usa,
#             color = "#2b2b2b",
#             fill = "white") +
#     geom_sf(data = st_simplify(streams,
#                                preserveTopology = FALSE,
#                                dTolerance = 1000),
#             aes(color = DensityPer100m_pred, linewidth = StreamOrde)) +
#             # aes(color = log_density_pred, linewidth = StreamOrde)) +
#     scale_discrete_manual(
#       "linewidth", 
#       values = seq(0.1, 1, length.out = 9), 
#       guide = "none") +
#     # scale_fill_palette_c(
#     #   met_palettes$Benedictus,
#     #   rescaler = ~ rescale_mid(.x, mid = 1)
#     # ) +
#     # scale_color_continuous(
#     #   palette = met_palettes$Benedictus,
#     #   breaks = breaks,
#     #   labels = function(b) round(exp(b), 1)
#     # ) +
#     # scale_color_steps2(
#     #   breaks = breaks,
#     #   labels = function(b) round(exp(b), 1),
#     #   high = "#3E938BFF",
#     #   mid = "#FDC718FF",
#     #   low = "#1942CDFF"
#     # ) +
#     # scale_color_steps(
#     #   # breaks = breaks,
#     #   labels = function(b) round(exp(b), 1),
#     #   # n.breaks = 10,
#     #   # breaks = c(0, 4, 8, 12, 20),
#     #   # breaks = seq(-15, 5, 1),
#     #   # labels = function(b) round(b, 1),
#     #   high = "#FDC718FF",
#     #   low = "#1942CDFF"
#     # ) +
#     scale_colour_gradientn(
#       # https://nschiett.github.io/fishualize/index.html
#       colors = paletteer_d("fishualize::Hypsypops_rubicundus"), # v4 and v7 and v8
#       values = c(0, 0.01, 0.03, 0.05, 0.07, 1), # fish/100m # v4 and v7 and v8
#       # breaks = c(0, 0.01, 0.03, 0.05, 0.07, 1)*276, # v7 and v8
#       breaks = c(1, 5, 10, 15, 200),
#       labels = c("0-2.76", "2.76-8.28", "8.28-13.80", "13.80-19.32", "19.32-276"),
#       # # labels = c(0, 0.01, 0.03, 0.05, 0.07, 1),
#       guide = "legend" # v7
#       # values = c(0, 0.0036, 0.036, 0.36, 1), # fish/100m
#       # values = c(0, 0.01, 0.03, 0.05, 0.07, 1), # fish/100m
#       # values = c(-Inf, -4.6, -3.5, -3, -2.7, 0), # log(fish/100m)
#       # labels = function(b) round(exp(b), 1)
#     ) +
#     coord_sf(xlim = c(-90, -75),
#              ylim = c(35, 45),
#              default_crs = sf::st_crs(4326)) +
#     labs(
#       title = common_name,
#       color = TeX("Number of fish per 100m")) +
#     ggthemes::theme_map() +
#     theme(
#       legend.position = "right"
#     ) #+ 
#     # guides( # v4 and v8
#     #   # colour = guide_coloursteps(show.limits = TRUE),
#     #   linewidth = "none")
#   
#   if(save_to_file){
#     ggsave(fish_map, filename = paste0(out_dir, "updated5_Region5_pred_density_map_", str_replace_all(common_name, " ", "_"), ".png"),
#            width = 4, height = 4, units = "in")
#   }
#   
#   return(fish_map)
# }

# https://ggplot2.tidyverse.org/reference/scale_gradient.html
# https://stackoverflow.com/questions/44855061/logarithmic-color-scale-in-ggplot2-squishes-certain-legend-numbers

plot_pred_density_map_updated5 = function(streams, preds_supp, common_name, out_dir, 
                                          breaks = NULL, save_to_file = TRUE){
  usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) %>%
    st_transform(crs = st_crs(streams))
  
  # discrete
  streams = streams %>%
    left_join(select(preds_supp, COMID, DensityPer100m_pred), by="COMID") %>%
    mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred <= 1, 1e-8, DensityPer100m_pred)) %>%
    mutate(log_density_pred = log(DensityPer100m_pred))
  
  print(summary(streams$DensityPer100m_pred))
  maxval = max(streams$DensityPer100m_pred)
  
  # plot of Subnetwork 6 with Subnetwork 5 highlighted in colors
  fish_map = ggplot() +
    geom_sf(data = usa,
            color = "#2b2b2b",
            fill = "white") +
    geom_sf(data = st_simplify(streams,
                               preserveTopology = FALSE,
                               dTolerance = 1000),
            aes(color = DensityPer100m_pred, linewidth = StreamOrde)) +
    scale_discrete_manual(
      "linewidth", 
      values = seq(0.1, 1, length.out = 9), 
      guide = "none") +
    # # for continuous scale in legend
    # scale_colour_gradientn(
    #   # https://nschiett.github.io/fishualize/index.html
    #   colors = paletteer_d("fishualize::Hypsypops_rubicundus"),
    #   values = c(0, 1/maxval, 10/maxval, 15/maxval, 20/maxval, 1) #, # fish/100m
    # ) +
    scale_colour_gradientn(
      # https://nschiett.github.io/fishualize/index.html
      colors = paletteer_d("fishualize::Hypsypops_rubicundus"),
      values = c(0, 1/maxval, 10/maxval, 15/maxval, 20/maxval, 1), # fish/100m
      breaks = c(0.5, 5.5, 12.5, 17.5, 150),
      labels = c("0-1", "1-10", "10-15", "15-20", "20-859"), #max = 277 for mottled, 231 for green, 859 for southern
      # values = c(0, 0.01, 0.03, 0.05, 0.07, 1), # fish/100m
      # breaks = c(1, 5, 10, 15, 200),
      # labels = c("0-2.76", "2.76-8.28", "8.28-13.80", "13.80-19.32", "19.32-276"),
      guide = "legend"
    ) +
    coord_sf(xlim = c(-90, -75),
             ylim = c(35, 45),
             default_crs = sf::st_crs(4326)) +
    labs(
      title = common_name,
      color = TeX("Number of fish per 100m")) +
    ggthemes::theme_map() +
    theme(
      legend.position = "right"
    )
  
  if(save_to_file){
    ggsave(fish_map, 
           filename = paste0(out_dir, "updated5_Region5_pred_density_map_", 
                             str_replace_all(common_name, " ", "_"), ".png"),
           width = 4, height = 4, units = "in")
  }
  
  return(fish_map)
}

plot_pred_density_map_updated5(
  streams,
  results_by_species$`Mottled Sculpin`$preds_supp,
  "Mottled Sculpin",
  out_dir = "S3N_paper_species_plots/"
)
plot_pred_density_map_updated5(
  streams,
  results_by_species$`Green Sunfish`$preds_supp,
  "Green Sunfish",
  out_dir = "S3N_paper_species_plots/"
)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.00006   2.25090   4.33456   5.20771   6.79316 230.63992 
plot_pred_density_map_updated5(
  streams,
  results_by_species$`Southern Redbelly Dace`$preds_supp,
  "Southern Redbelly Dace",
  out_dir = "S3N_paper_species_plots/"
)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0001   2.1817   5.5298   6.3677   8.6591 858.4455 

