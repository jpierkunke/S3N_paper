# Functions for S3N models and analysis
# PhD dissertation, Chapters 3-4
# Jess Kunke, 2025 April

# Environmental covariate preprocessing -----------------------------------

# function to read in and combine national covariate data
# - updated to use just the variables we want
# - imputing is done after joining with streams because it requires geometries
#   in order to identify the nearest COMID for each COMID with a missing value
# - note: running this function requires having the individual input datasets
combine_and_write_national_covariate_data = function(covs_path, comid_huc12_path,
                                                     comid_huc12_filename = "HUC12_PU_COMIDs_CONUS.csv"){
  
  message("Mapping COMIDs to HUC12 and HUC8 codes...")
  comid_to_huc12 = read_csv(paste0(comid_huc12_path, comid_huc12_filename), 
                            show_col_types = FALSE,
                            name_repair = "unique_quiet") %>%
    # contains FID_COMID_, FID_UHUC12, COMID, 
    # REACHCODE, HUC12, TOHUC, Length, GLOBALID, HUC8
    select(COMID, HUC8, HUC12)
  
  # Development: Sum of percentages of low, medium and high development
  # Agriculture: Sum of percentages of pasture/hay and crops
  message("Reading land use data...")
  land_use = read_csv(paste0(covs_path,"Landuse/NLCD16_TOT_CONUS.TXT"), 
                      show_col_types = FALSE,
                      name_repair = "unique_quiet") %>%
    mutate_all(na_if, -9999) %>% #across(.everything), na_if(-9999)) %>%
    mutate(Development = TOT_NLCD16_22+TOT_NLCD16_23+TOT_NLCD16_24,
           Agriculture = TOT_NLCD16_81+TOT_NLCD16_82) %>%
    select(COMID, Development, Agriculture)
  
  # Mean elevation
  message("Reading mean elevation data...")
  elev_mean = read_csv(paste0(covs_path,"Topographic/BASIN_CHAR_CAT_CONUS.TXT"), 
                       show_col_types = FALSE,
                       name_repair = "unique_quiet") %>%
    # file contains COMID, CAT_BASIN_AREA, CAT_BASIN_SLOPE, CAT_ELEV_MEAN, 
    # CAT_ELEV_MIN, CAT_ELEV_MAX, CAT_STREAM_SLOPE, CAT_STREAM_LENGTH
    # we use mean elevation
    select(COMID, CAT_ELEV_MEAN)
  
  # Total basin area
  message("Reading total basin area data...")
  basin_area = read_csv(paste0(covs_path,"Topographic/BASIN_CHAR_TOT_CONUS.TXT"), 
                        show_col_types = FALSE,
                        name_repair = "unique_quiet") %>%
    mutate_all(na_if, -9999) %>%
    select(COMID, TOT_BASIN_AREA)
  
  # Total annual runoff
  message("Reading total annual runoff data...")
  runoff = read_csv(paste0(covs_path,"Hydrology/RUN7100_CONUS.TXT"), 
                    show_col_types = FALSE,
                    name_repair = "unique_quiet") %>%
    mutate_all(na_if, -9999) %>%
    select(COMID, TOT_RUN7100)
  
  # Baseflow index
  message("Reading baseflow index data...")
  BFI = read_csv(paste0(covs_path,"Hydrology/BFI_CONUS.TXT"), show_col_types = FALSE,
                 name_repair = "unique_quiet") %>%
    mutate_all(na_if, -9999) %>%
    select(COMID, TOT_BFI)
  
  # Mean annual temperature
  message("Reading mean annual temperature data...")
  temp_mean = read_csv(paste0(covs_path,"Temperature/TMEAN7100_ANN_CONUS.TXT"), 
                       show_col_types = FALSE,
                       name_repair = "unique_quiet") %>%
    select(COMID, TOT_TAV7100_ANN) %>%
    mutate_all(na_if, -9999) %>%
    rename(MeanAnnualTemp = TOT_TAV7100_ANN)
  
  # Hydrologic Alteration Index (HAI) from US model
  message("Reading hydrologic alteration index data (US)...")
  HAI_US = read_csv(paste0(covs_path,
                           "Hydrologic_alteration_McManamay2022/predicted_alteration_US_model.csv"), 
                    show_col_types = FALSE,
                    name_repair = "unique_quiet") %>%
    select(COMID, pnHA_rank) %>%
    rename(HAI_US = pnHA_rank)
  
  # Floodplain integrity
  message("Reading floodplain integrity data...")
  FPI = read_csv(paste0(covs_path,"Morrison2023_data/_IFI.stats.csv"), 
                 show_col_types = FALSE,
                 name_repair = "unique_quiet") %>%
    select(HUC12, IFI_geomean) %>%
    mutate(HUC12 = as.character(HUC12))
  
  # Now join them all together
  message("Joining environmental covariate data...")
  envir_covars = data.frame(
    COMID = unique(c(comid_to_huc12$COMID, land_use$COMID, elev_mean$COMID, 
                     basin_area$COMID, runoff$COMID, BFI$COMID, 
                     temp_mean$COMID, HAI_US$COMID)) #, HAI_regional$COMID))
  ) %>%
    left_join(comid_to_huc12, by="COMID") %>%
    left_join(land_use, by="COMID") %>%
    left_join(elev_mean, by="COMID") %>%
    left_join(basin_area, by="COMID") %>%
    # left_join(stream_density, by="COMID") %>%
    left_join(runoff, by="COMID") %>%
    left_join(BFI, by="COMID") %>%
    left_join(temp_mean, by="COMID") %>%
    left_join(HAI_US, by="COMID") %>%
    left_join(FPI, by="HUC12")
  
  message("Writing environmental covariate data to file...")
  write_csv(envir_covars, file = paste0(covs_path, "envir_covars_national.csv"))
  message("Done.")
}

# Streams data preprocessing ----------------------------------------------

read_national_covariate_data = function(covs_path, 
                                        envir_covars_fn = "envir_covars_national.csv"){
  envir_covars = read_csv(paste0(covs_path, envir_covars_fn),
                          show_col_types = FALSE,
                          name_repair = "unique_quiet")
  return(envir_covars)
}

join_envir_covar_data = function(data_or_sf, 
                                 covs_path,
                                 envir_covars_fn = "envir_covars_national.csv"){
  message("Reading and joining environmental covariates...")
  envir_covars = read_national_covariate_data(covs_path, envir_covars_fn)
  
  data_or_sf = data_or_sf %>%
    left_join(envir_covars, by="COMID")
  
  return(data_or_sf)
}

# reassign the COMIDs of reaches with DUP_COMID = 1 to be unique negative values
# - note: these could possibly (not likely) inadvertently match a negative COMID
#   in the environmental covariate data; not an issue for Region 5 but possible
#   for other regions
reassign_dup_COMIDs = function(streams){
  message("Reassigning duplicate COMIDs as negative COMIDs...")
  streams$COMID[streams$DUP_COMID == 1] = -1*(1:sum(streams$DUP_COMID == 1))
  return(streams)
}

# impute HAI_US and IFI_geomean with value from nearest COMID
# - we can only impute the variables once we join with streams so that we have 
#   geometries to use in st_nearest_feature
impute_and_rename_covars = function(streams, save_to_file = FALSE, out_dir = NULL){
  
  message("Imputing HAI_US and IFI_geomean from nearest COMID...")
  # impute HAI_US
  value_missing = dplyr::filter(streams, is.na(HAI_US))
  value_not_missing = dplyr::filter(streams, !is.na(HAI_US))
  # for each COMID with missing HAI_US,
  # this returns the index *among COMIDs with nonmissing HAI_US* of the nearest COMID
  nearest = st_nearest_feature(value_missing, value_not_missing)
  # use this information to replace missing values with the value from the nearest "nonmissing" COMID
  streams$HAI_US[is.na(streams$HAI_US)] = value_not_missing$HAI_US[nearest]
  
  # impute IFI_geomean
  value_missing = dplyr::filter(streams, is.na(IFI_geomean))
  value_not_missing = dplyr::filter(streams, !is.na(IFI_geomean))
  nearest = st_nearest_feature(value_missing, value_not_missing)
  streams$IFI_geomean[is.na(streams$IFI_geomean)] = value_not_missing$IFI_geomean[nearest]
  
  message("Renaming environmental covariates...")
  streams = streams %>%
    rename(Elevation = CAT_ELEV_MEAN,
           Water_Area = TOT_BASIN_AREA,
           Ann_Runoff = TOT_RUN7100,
           Baseflow = TOT_BFI,
           Ann_Temp = MeanAnnualTemp,
           Hydro_Alter = HAI_US,
           Fldplain_Dis = IFI_geomean)
  
  if(save_to_file){
    message("Writing imputed and renamed covariates to file...")
    
    region5_covars = select(st_drop_geometry(streams), COMID, Development:Fldplain_Dis)
    
    save(region5_covars, 
         file = paste0(out_dir, "region5_imputed_covars_by_COMID.RData"))
  }
  
  message("Done.")
  return(streams)
}

# Stream network preprocessing: streams -----------------------------------

#' Get XY coordinates for a single stream reach
#'
#' This function can be used manually to extract the coordinates for individual
#' stream reaches. Its main purpose is to be applied by add_upstream_downstream_nodes
#' to extract coordinates for the entire simple feature list column of a streams sf object.
#'
#' @param streamsgeom The simple feature geometry (sfg) for a single reach in the streams sf object.
#'
#' @return A list with four named elements:
#' the upstream X and Y coordinates (`up_X` and `up_Y`) and
#' the downstream X and Y coordinates (`dn_X` and `dn_Y`).
#' @export
#'
#' @examples
get_node_coords = function(streamsgeom){
  row = data.frame(sf::st_coordinates(streamsgeom))
  up_X = row$X[1]
  up_Y = row$Y[1]
  dn_X = tail(row$X, n=1)
  dn_Y = tail(row$Y, n=1)
  return(list(up_X=up_X, up_Y=up_Y, dn_X=dn_X, dn_Y=dn_Y))
}


#' Add upstream and downstream nodes to streams dataframe as columns
#'
#' @param streams The streams sf object
#'
#' @return The same streams sf object with six additional columns
#' @export
#'
#' @examples
add_upstream_dnstream_nodes = function(streams){
  message("Extracting upstream and downstream node coordinates for each stream reach...")
  coordcols = purrr::map_dfr(streams$geometry, get_node_coords) %>%
    dplyr::mutate(
      upstream_node = paste(as.character(up_X),
                            as.character(up_Y)),
      dnstream_node = paste(as.character(dn_X),
                            as.character(dn_Y))
    )
  
  streams$up_X = coordcols$up_X
  streams$up_Y = coordcols$up_Y
  streams$dn_X = coordcols$dn_X
  streams$dn_Y = coordcols$dn_Y
  streams$upstream_node = coordcols$upstream_node
  streams$dnstream_node = coordcols$dnstream_node
  
  return(streams)
}


#' Make node and line graphs of a stream network
#'
#' @param streams The stream network of interest, as an sf object, with two
#' columns indicating the unique IDs of the upstream and downstream nodes of
#' each reach. These columns are assumed to have names ending in "node" and to
#' be the only columns whose names end in "node".
#'
#' @return a bunch of graphs and adjacency matrices
#' @export
#'
#' @examples
get_stream_graphs = function(streams){
  ptIDs = streams %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tidyselect::ends_with("node")) %>%
    as.matrix()
  
  # here, graph nodes are endpoints of reaches
  node_graph = igraph::graph_from_edgelist(ptIDs)
  adj_mat_node = igraph::as_adj(node_graph, sparse=TRUE)
  
  # here, graph nodes are stream reaches themselves; this is the edge/dual
  # graph to node_graph
  line_graph = igraph::make_line_graph(node_graph)
  adj_mat_edge = igraph::as_adj(line_graph, sparse=TRUE)
  
  # make list of just the nonzero indices in the edge adjacency matrix
  # for use in computing upstream distances later
  # https://slowkow.com/notes/sparse-matrix/
  adj_mat_edge_nonzero = data.frame(
    row_index = adj_mat_edge@i+1, # row ID of each nonzero entry (need to add 1 b/c zero indexed)
    col_index = rep(1:adj_mat_edge@Dim[2], diff(adj_mat_edge@p)) # col ID of each nonzero entry
    # adj_mat_edge@x # values of the nonzero entries; don't need because we know they're 1s
  )
  
  return(list(node_graph = node_graph,
              adj_mat_node = adj_mat_node,
              line_graph = line_graph,
              adj_mat_edge = adj_mat_edge,
              adj_mat_edge_nonzero = adj_mat_edge_nonzero))
}


#' Add stream network information to the streams and stream graph objects
#'
#' @param streams The streams sf object
#' @param sg The streams graph object created from `get_stream_graphs`, or a
#' list with at least two elements (called `adj_mat_node` and `adj_mat_edge`)
#' that correspond to the node and edge/line adjacency matrices
#'
#' @return A list with both the streams and sg object updated in the following
#' ways:
#'
#' @export
#'
#' @examples
add_stream_source_outlet_component = function(streams, sg){
  source_nodes = names(which(Matrix::colSums(sg$adj_mat_node)==0))
  sink_nodes = names(which(Matrix::rowSums(sg$adj_mat_node)==0))
  message(paste("Number of source nodes:",length(source_nodes)))
  message(paste("Number of sink nodes:",length(sink_nodes)))
  
  source_reaches = which(Matrix::colSums(sg$adj_mat_edge)==0)
  sink_reaches = which(Matrix::rowSums(sg$adj_mat_edge)==0)
  message(paste("Number of source reaches:",length(source_reaches)))
  message(paste("Number of sink reaches:",length(sink_reaches)))
  
  # some checks:
  message("\nChecking outflow:")
  message("Row sums of the streams adjacency matrix should be")
  message("0 for stream outlets (network sinks),")
  message("1 for simple continuations from one reach to another,")
  message(">1 if the flow forks into 2 or more flows (forks must be removed)")
  # for outflow, we can look at edges instead of nodes because
  #   we don't care about forks at sources
  print(table(Matrix::rowSums(sg$adj_mat_edge)))
  
  message("\nChecking inflow:")
  message("Column sums of the streams adjacency matrix should be")
  message("0 for stream sources,")
  message("1 for simple continuations from one reach to another,")
  message("2 for confluences,")
  message(">2 for complex confluences (these must be removed)")
  # for inflow, we also can look at edges instead of nodes because we don't
  # care about complex confluences at sink nodes
  print(table(Matrix::colSums(sg$adj_mat_edge)))
  
  # add columns to streams indicating whether the downstream node of each reach
  # is a sink or a source (often neither)
  streams = streams %>%
    dplyr::mutate(is_sink = (dnstream_node %in% sink_nodes)) %>%
    dplyr::mutate(is_src = (upstream_node %in% source_nodes))
  
  message("\nIdentifying network components from node and line graphs")
  conn_comp = igraph::components(sg$line_graph)
  conn_comp_nodes = igraph::components(sg$node_graph)
  
  if(conn_comp_nodes$no == 1){
    message(paste("\nThis network has", conn_comp_nodes$no, "component."))
  } else {
    message(paste("\nThis network has", conn_comp_nodes$no, "components."))
  }
  
  sg$conn_comp = conn_comp
  sg$conn_comp_nodes = conn_comp_nodes
  
  # add network component ID to streams
  streams$componentID = conn_comp$membership
  
  return(list(streams = streams, sg = sg))
}


#' Configure the stream network
#'
#' This function combines `add_upstream_dnstream_nodes`, `get_stream_graphs`, and  extracts the upstream and downstream nodes from the stream reach
#' geometry column, creates unique IDs for them, adds the coordinates and unique
#' IDs to the stream sf object as new columns, computes the node and edge/line
#' adjacency matrices for the stream network, identifies the network components,
#' adds the network component information to the stream graph object, and adds
#' columns to the streams sf object to indicate whether each reach is a stream
#' outlet, whether each reach is a stream source, and the unique network ID of
#' the network component containing that reach.
#'
#' @param streams A streams sf object.
#'
#' @return A list consisting of the streams and sg objects updated in the following
#' ways:
#' @export
#'
#' @examples
configure_stream_network = function(streams){
  streams = add_upstream_dnstream_nodes(streams)
  
  stream_graphs = get_stream_graphs(streams)
  
  streams_res = add_stream_source_outlet_component(streams, stream_graphs)
  
  return(streams_res)
}

#' Compute reach binary IDs, upstream distance, and weights
#'
#' @param streams A streams sf object
#' @param sg A streams graph object
#'
#' @return The streams object with
#' @export
#'
#' @examples
compute_stream_updist_vars = function(streams, sg){
  # add rowID for use in computing upstream distances
  streams = dplyr::mutate(streams, rowID = 1:nrow(streams), .before = "COMID")
  # change 0's to a small value for calculating log_segPI and log_AFV later
  streams$TotDASqKM = ifelse(streams$TotDASqKM == 0, 0.0001, streams$TotDASqKM)
  
  upstream_dists = matrix(nrow=nrow(streams), ncol=5)
  current_list = data.frame(
    rowID = which(streams$is_sink),
    # network ID for a given river network has to be unique for each stream outlet (sink),
    # so we use the COMID of the stream outlet (sink)
    networkID = streams$COMID[streams$is_sink],
    # upstream distances of sink reaches are 0
    binaryID = as.character(1),
    up_dist = 0,
    # segment proportional influences and additive function values are 1 for sinks
    log_segPI = 0,
    log_AFV = 0)
  # matrices must all have the same type,
  # and binaryIDs get too long to store as numbers or integers
  # therefore, store the binary IDs as character type in a separate matrix
  binaryIDs = matrix(nrow=nrow(streams), ncol=1)
  
  i=1
  # as long as current list has at least 1 stream reach,
  while(nrow(current_list) > 0){
    if(i%%20 == 0){cat(paste0("\r Iteration ", i))}
    
    # save binaryIDs to the character matrix
    binaryIDs[current_list$rowID,] = current_list$binaryID
    # update upstream_dists
    upstream_dists[current_list$rowID, ] = as.matrix(dplyr::select(current_list, -binaryID))
    
    # new current_list will have a row for each parent reach of the reaches in old current_list
    current_list <- current_list %>%
      # match each old reach to the (up to 2) "parent" (new) reaches directly upstream of it that flow into it
      dplyr::inner_join(sg$adj_mat_edge_nonzero,
                        by=c("rowID" = "col_index")) %>%
      dplyr::rename(child_rowID = rowID,
                    rowID = row_index,
                    child_binID = binaryID) %>%
      dplyr::relocate(rowID, .before = child_rowID)
    
    if(nrow(current_list) == 0){break}
    
    nparents = (current_list %>%
                  dplyr::group_by(child_rowID) %>%
                  dplyr::summarize(n = dplyr::n()))$n
    
    current_list <- current_list %>%
      dplyr::arrange(child_rowID) %>%
      dplyr::mutate(binaryID = unlist(purrr::map(nparents, function(x) 1:x))) %>%
      dplyr::mutate(binaryID = ifelse(binaryID==1,
                                      paste0(as.character(child_binID), 0),
                                      paste0(as.character(child_binID), 1))) %>%
      dplyr::arrange(rowID) %>%
      # add COMID and TotDASqKM to current_list so we can compute segment proportional influence (segPI)
      dplyr::left_join(dplyr::select(sf::st_drop_geometry(streams),
                                     rowID, COMID, TotDASqKM),
                       by="rowID") %>%
      # join LENGTHKM for each child_rowID (length of the old reach downstream of the new one)
      dplyr::left_join(dplyr::select(sf::st_drop_geometry(streams),
                                     rowID, LENGTHKM),
                       by=c("child_rowID" = "rowID")) %>%
      dplyr::rename(child_length = LENGTHKM) %>%
      # add the length of the child reach to the child (old) up_dist to get the parent up_dist
      dplyr::mutate(up_dist = child_length + up_dist)
    
    # compute segment proportional influence and AFV:
    current_list = current_list %>%
      # first, compute the sums of TotDASqKM across all parents of each child
      dplyr::left_join((current_list %>%
                          dplyr::group_by(child_rowID) %>%
                          dplyr::summarise(sum = sum(TotDASqKM))),
                       by = "child_rowID") %>%
      # compute segment PI
      dplyr::mutate(log_segPI = log(TotDASqKM/sum)) %>%
      # update AFV
      dplyr::mutate(log_AFV = log_segPI + log_AFV) %>%
      dplyr::select(rowID, networkID, binaryID,
                    up_dist, log_segPI, log_AFV)
    
    i=i+1
  }
  message("")
  
  # final reformatting of upstream_dists before merging with streams
  upstream_dists = as.data.frame(upstream_dists) %>%
    dplyr::mutate(binaryID = as.vector(binaryIDs)) %>%
    dplyr::rename(
      rowID = V1,
      networkID = V2,
      up_dist = V3,
      log_segPI = V4,
      log_AFV = V5
    )
  
  if(sum(names(upstream_dists)[2:6] %in% names(streams)) > 0){
    streams = select(streams, -names(upstream_dists)[2:6][names(upstream_dists)[2:6] %in% names(streams)])
  }
  
  streams = streams %>% dplyr::left_join(upstream_dists, by="rowID")
  
  return(streams)
}


# Stream network preprocessing: points ------------------------------------

read_preds_data = function(streams, pred_path, pred_filename){
  preds1 = read_sf(pred_path, pred_filename) %>%
    reassign_dup_COMIDs() %>%
    dplyr::filter(COMID %in% streams$COMID) %>%
    select(COMID)
  
  preds = preds1 %>%
    left_join(
      select(st_drop_geometry(streams), -rowID),
      by = "COMID"
      )
  
  if(!identical(preds1, preds[,1:ncol(preds1)])){
    cat("Warning in read_preds_data(): double-check the preds-streams join.", fill = TRUE)
  }
  
  rm(preds1)
  
  return(preds)
}

# needed if preds DOES NOT have a DUP_COMID column
join_fish_to_preds = function(fish, preds){
  cat(paste0("Joining fish data to preds... "), fill=TRUE)
  # join the preds data to use preds XY coords instead of the X,Y in fishscales
  fish = left_join(fish, preds, by="COMID") %>%
    # make an sf class object
    st_sf()
  return(fish)
}

# needed if preds HAS a DUP_COMID column
join_fish_to_preds_withdups = function(fish, preds){
  cat(paste0("Joining fish data to preds... "), fill=TRUE)
  # join the preds data to use preds XY coords instead of the X,Y in fishscales
  fish = left_join(fish, filter(preds, DUP_COMID == 0), by="COMID") %>%
    # make an sf class object
    st_sf()
  return(fish)
}

# using fish density and stream reach length,
# estimate the total number of fish on that stream reach
# - only needed for comparing observed and predicted counts
compute_fish_reach_count_from_density = function(fish){
  fish = fish %>%
    mutate(CountReachTotal = 10*DensityPer100m*LENGTHKM) %>%
    relocate(CountReachTotal, .after = DensityPer100m)
  return(fish)
}


#' Compute point upstream distances
#'
#' @param points A points sf object
#' @param streams A streams sf object
#'
#' @return The points sf object with an additional column called `up_dist` with
#' each point's upstream distance in kilometers
#' @export
#'
#' @examples
calculate_point_upstream_distance = function(points, streams){
  streams = mutate(streams, rowID = 1:nrow(streams))
  
  # match streams geometry to the point data frame (preds and/or obs) by COMID
  match_COMIDs = left_join(select(st_drop_geometry(points), COMID),
                           select(st_drop_geometry(streams), rowID, COMID), by="COMID")
  
  # subset the streams and points geometries to those that match
  # l = st_sf(streams$geometry[match_COMIDs$rowID])
  # p = st_sf(points$geometry)
  l = streams$geometry[match_COMIDs$rowID]
  p = points$geometry
  
  # note: sf::st_snap is based on PostGIS st_snap, which snaps to the nearest vertex,
  # not the nearest point: https://github.com/r-spatial/sf/issues/792
  # units: km
  up_dist_point = (as.numeric(st_length(l)) - st_line_project(l, p))/1000 # convert to km
  
  # # to validate:
  # # look at a observation point p that is farther from the downstream node than 
  # # from the upstream node
  # i=5; mapview(st_sf(l[i,])) + 
  #   mapview(st_sf(p[i,])) + 
  #   # downstream node
  #   mapview(st_sf(st_line_sample(st_sf(l[i,]), sample = 1)))
  # 
  # # this value (the fraction of the length of the reach from the downstream 
  # # node that the observation point is located) should be > 0.5
  # 1.0 - st_line_project(l, p, normalized = TRUE)[i]
  
  # finally, add this value to the upstream distance of the reach the point is on
  # to obtain the total upstream distance of that point from the stream outlet for
  # this connected component within this region
  points = points %>%
    # no longer need this since we compute binaryIDs on streams before we join with preds and then with fish,
    # so they're already part of obs
    # left_join(select(st_drop_geometry(streams),
    # COMID, networkID:binaryID), by="COMID") %>%
    # the upstream distance of the point = the upstream distance of the reach it is on
    # plus the extra distance from the reach downstream node to the point
    # units: km (both up_dist and up_dist_point are in km)
    mutate(up_dist = up_dist + up_dist_point)
  
  return(points)
}

check_preds_fish_updists_match = function(preds, fish){
  compare_up_dist = preds %>% 
    st_drop_geometry() %>% 
    dplyr::filter(COMID %in% fish$COMID) %>% 
    select(COMID, up_dist) %>% 
    left_join(fish %>%
                st_drop_geometry() %>%
                select(COMID, up_dist) %>%
                distinct(),
              by = "COMID")
  
  # all the upstream distances match before the point upstream distances are added
  diff_sum = sum(compare_up_dist$up_dist.x != compare_up_dist$up_dist.y)
  
  if(diff_sum == 0){
    cat("Upstream distances match between prediction and observation points.", fill = TRUE)
  }else{
    stop(paste("Upstream distances do not match at", diff_sum, "points"))
  }
}

filter_fish_to_preds_COMIDs = function(fish, preds){
  cat(paste0("Filtering fish data to only COMIDs that are also in preds... ", nrow(fish)), fill=TRUE)
  fish = filter(fish, COMID %in% preds$COMID)
  cat(paste0("Fish obs on the same COMIDs as preds: ", nrow(fish)), fill=TRUE)
  cat(paste0("Number of unique COMIDs in this subset of the fish data: ", n_distinct(fish$COMID)), fill=TRUE)
  return(fish)
}

prep_to_compute_pwdist_region5 = function(streams, fish, pred_path, out_dir,
                                          pred_filename = "PredictionPoints_MS05_NSI"){
  
  # inherit envir covariates, binary IDs, upstream distances, and AFV
  # from streams to preds
  cat("Reading in regional prediction points data...", fill=TRUE)
  tic("Read in regional prediction points data")
  preds = read_preds_data(streams, pred_path, pred_filename = pred_filename)
    
  toc()
  
  # make sure the preds and obs represent only one connected component
  cat("Checking that prediction points layer has only one network component...", fill=TRUE)
  if (n_distinct(preds$networkID) != 1) {
    stop("Observation, prediction, and stream data must come from a single connected river network component.")
  }else{
    cat("Yes, prediction points layer has only one network component.", fill = TRUE)
  }
  
  # compute point upstream distances
  cat("Updating upstream distances to add the distance from downstream node to point...", fill=TRUE)
  tic("Compute prediction site/point upstream distances")
  preds = calculate_point_upstream_distance(preds, streams)
  toc() # prediction site/point upstream distances: 5.08 sec elapsed
  
  cat("Creating obs point layer from obs data...", fill = TRUE)
  obs = fish %>%
    filter_fish_to_preds_COMIDs(preds) %!>%
    join_fish_to_preds(preds) %!>%
    select(COMID, networkID, binaryID, up_dist, log_AFV) %>%
    unique()
  obs$ptID = 1:nrow(obs)
  obs = relocate(obs, ptID)
  
  cat("Reordering preds so that first n rows are the fish COMIDs...", fill = TRUE)
  
  preds = preds %>%
    left_join(select(st_drop_geometry(obs), COMID, ptID)) %>%
    arrange(ptID) %>%
    select(COMID, networkID, binaryID, up_dist, log_AFV, ptID)
  preds$ptID[(nrow(obs)+1):nrow(preds)] = (nrow(obs)+1):nrow(preds)
  preds = relocate(preds, ptID)
  
  streams = streams %>%
    left_join(select(st_drop_geometry(preds), COMID, ptID), by="COMID") %>%
    relocate(ptID) %>%
    arrange(ptID)
  
  cat("Writing obs, preds, streams to file preds_obs_pwdist_input_data.RData for computing pairwise distances...", fill=TRUE)
  tic("Writing obs, preds, streams to file")
  save(streams, preds, obs, file = paste0(out_dir, "preds_obs_pwdist_input_data.RData"))
  toc()
  
  cat("Done.", fill=TRUE)
}

prep_to_compute_pwdist = function(streams, obs, out_dir, 
                                  pred_filename = "PredictionPoints_MS05_NSI"){
  
  # inherit envir covariates, binary IDs, upstream distances, and weights
  # from streams to preds
  cat("Reading in regional prediction points data...", fill=TRUE)
  tic("Read in regional prediction points data")
  preds = read_preds_data(streams, pred_path, pred_filename = pred_filename)
  toc()
  
  # make sure the preds and obs represent only one connected component
  cat("Checking that prediction points layer has only one network component...", fill=TRUE)
  if (n_distinct(preds$networkID) != 1) {
    stop("Observation, prediction, and stream data must come from a single connected river network component.")
  }else{
    cat("Yes", fill = TRUE)
  }
  
  # compute point upstream distances
  cat("Updating upstream distances to add the distance from downstream node to point...", fill=TRUE)
  tic("Compute prediction site/point upstream distances")
  preds = calculate_point_upstream_distance(preds, streams)
  toc() # prediction site/point upstream distances: 5.08 sec elapsed
  
  cat("Reordering preds so that first n rows are the fish COMIDs...", fill = TRUE)
  
  obs = obs %>%
    st_drop_geometry() %>%
    join_fish_to_preds(preds) %>%
    select(COMID, networkID, binaryID, up_dist, log_AFV) %>%
    unique()
  obs$ptID = 1:nrow(obs)
  obs = relocate(obs, ptID)
  
  preds = preds %>%
    left_join(select(st_drop_geometry(obs), COMID, ptID)) %>%
    arrange(ptID) %>%
    select(COMID, networkID, binaryID, up_dist, log_AFV, ptID)
  preds$ptID[(nrow(obs)+1):nrow(preds)] = (nrow(obs)+1):nrow(preds)
  preds = relocate(preds, ptID)
  
  streams = streams %>%
    left_join(select(st_drop_geometry(preds), COMID, ptID), by="COMID") %>%
    relocate(ptID) %>%
    arrange(ptID)
  
  cat("Writing obs, preds, streams to file preds_obs_pwdist_input_data.RData for computing pairwise distances...", fill=TRUE)
  tic("Writing obs, preds, streams to file")
  save(streams, preds, obs, file = paste0(out_dir, "preds_obs_pwdist_input_data.RData"))
  toc()
  
  cat("Done.", fill=TRUE)
}


# Computing obs-obs and preds-obs pairwise distances ----------------------

# function to find nearest common junction (NCJ) of two binary IDs
# thanks to flodel:
# https://stackoverflow.com/questions/26285010/r-find-largest-common-substring-starting-at-the-beginning
nearest_common_junction <- function(binID1, binID2) {
  # the length of the shorter word
  n <- min(nchar(binID1), nchar(binID2))
  # the length of the nearest common junction ID
  m = sum(as.logical(cumprod(charToRaw(binID1)[1:n] == charToRaw(binID2)[1:n])))
  return(rawToChar(charToRaw(binID1)[1:m]))
}

# computes pairwise distances in km between each prediction location and each 
# observation location (for later identifying nearest obs locations to each 
# prediction location)
# suggestion: use just a subset of preds and run in parallel
# same function works for finding pwdists and nearest neighbors between obs points
# - in this case, preds represents the subset of obs locations for which we are 
#   computing pairwise distances and neighbors
# - Apr 9 2025: I updated this to avoid computing redundant pairs
compute_pwdists_pred_obs = function(preds, obs, streams, m, out_dir, 
                                    obs_only = FALSE, out_file_suffix = NULL) {
  
  if(!obs_only & is.null(out_file_suffix)){
    stop("When obs_only = FALSE, must provide out_file_suffix in case of multiple batches.")
  }
  
  # make sure the preds and obs represent only one connected component
  if (n_distinct(preds$networkID) != 1 | n_distinct(obs$networkID) != 1) {
    if(obs_only){
      stop("Observation locations must come from a single connected river network component.")
    } else {
      stop("Prediction and observation locations must come from a single connected river network component.")
    }
  }
  
  preds_info = preds %>%
    st_drop_geometry() %>%
    select(ptID, COMID, binaryID, up_dist, log_AFV) %>%
    # we only need the unique point locations
    unique()
  
  obs_info = obs %>%
    st_drop_geometry() %>%
    select(ptID, COMID, binaryID, up_dist, log_AFV) %>%
    # we only need the unique point locations
    unique()
  
  # initialize a matrix of fish_pairs
  # one row for each combination of a prediction location and an observation location
  # number of rows = number of preds x number of obs
  # fish_pairs = matrix(nrow=nrow(preds)*nrow(obs), ncol=2)
  # fish_pairs = expand_grid(ind_pred = 1:nrow(preds), ind_obs = 1:nrow(obs))
  fish_pairs = expand_grid(ind_pred = preds_info$ptID, ind_obs = obs_info$ptID)
  # fish_pairs$pred_binID = rep(preds$binaryID, each = nrow(obs))
  # fish_pairs$obs_binID = rep(obs$binaryID, times = nrow(preds))
  fish_pairs$COMID_pred = rep(preds$COMID, each = nrow(obs))
  fish_pairs$COMID_obs = rep(obs$COMID, times = nrow(preds))
  fish_pairs = filter(fish_pairs, ind_pred > ind_obs)
  # if p = # pred points in this batch and n = # obs points in this batch,
  # at this point there should be n*(n-1)/2 obs-obs pairs and n*(p-n) pred-obs pairs
  # so number of rows should equal n*(n-1)/2 + n*(p-n)
  
  # join binIDs and up_dist
  cat("Joining binary IDs and upstream distances...", fill = TRUE)
  tic("Joining binary IDs and upstream distances")
  fish_pairs = as.data.frame(fish_pairs) %>%
    # add the binary IDs and upstream distances for each COMID in each pair
    left_join(preds_info, by = join_by(COMID_pred == COMID)) %>%
    rename(binID_pred = binaryID, updist_pred = up_dist, logAFV_pred = log_AFV) %>%
    # filter(networkID %in% c(454080,  465220)) %>%
    left_join(obs_info, by = join_by(COMID_obs == COMID)) %>%
    rename(binID_obs = binaryID, updist_obs = up_dist, logAFV_obs = log_AFV)
  toc()
  
  # compute nearest common junctions
  cat("Computing nearest common junctions...", fill = TRUE)
  tic("Computing nearest common junctions")
  fish_pairs$binID_ncj = mapply(nearest_common_junction, 
                                fish_pairs$binID_pred, fish_pairs$binID_obs)
  toc()
  
  # compute pairwise distances
  cat("Computing pairwise distances...", fill = TRUE)
  tic("Computing pairwise distances")
  fish_pairs = fish_pairs %>%
    # determine flow connected (FC) or flow unconnected (used in computing distance)
    # FC if the NCJ of points 1 and 2 is one of those two points themselves
    mutate(FC = ((binID_ncj == binID_pred) | (binID_ncj == binID_obs))) %>%
    # get the upstream distance of the NCJ
    # we need to use streams instead of fish obs because the COMID of the NCJ of
    # two observation points may not be represented in the obs data frame
    left_join(select(st_drop_geometry(streams), binaryID, up_dist, LENGTHKM),
              by = join_by(binID_ncj == binaryID)) %>%
    rename(updist_ncj = up_dist, ncj_length = LENGTHKM) %>%
    # compute each distance
    mutate(pair_dist = ifelse(FC,
                              abs(updist_pred - updist_obs),
                              updist_pred + updist_obs - 2*(updist_ncj+ncj_length))) %>%
    # the upstream AFV is the smaller of the two AFVs (need this to compute weights)
    mutate(logAFVup = pmin(logAFV_pred, logAFV_obs), logAFVdn = pmax(logAFV_pred, logAFV_obs)) %>%
    select(ind_pred, ind_obs, pair_dist, FC, logAFVup, logAFVdn)
  toc()
  
  # compute spatial weights
  # weight pi_{ij} = sqrt(AFVj/AFVi) if i,j FC and j upstream,
  #                  sqrt(AFVi/AFVj) if i,j FC and i upstream,
  #                  0 otherwise
  cat("Computing spatial weights...", fill = TRUE)
  fish_pairs$weight = 0
  fish_pairs$weight[fish_pairs$FC] = sqrt(exp(fish_pairs$logAFVup[fish_pairs$FC] - fish_pairs$logAFVdn[fish_pairs$FC]))
  fish_pairs = select(fish_pairs, ind_pred, ind_obs, pair_dist, weight, FC)
  
  # compute neighbor vars nnIndx, d, and weights
  cat("Computing neighbor variables nnIndx, nnDist, and nnWght", fill = TRUE)
  
  # identify nearest neighbors, prioritizing FC points for tail-up covariance
  fish_mat = fish_pairs %>%
    # within the group of rows for a given pred_point,
    # - put the FC points first, followed by the not-FC points
    # - then within FC and not-FC for each pred_point,
    #   sort in order of increasing pairwise distance
    # this ensures that FC points are picked first, and
    # not-FC points are chosen only if # FC points < m
    arrange(ind_pred, desc(FC), pair_dist) %>%
    group_by(ind_pred) %>%
    slice_head(n = m)
  
  # there are m neighbors for every pred point
  # (whereas there are only i-1 many neighbors for obs points i <= m)
  if(obs_only){ # obs-obs pairs
    
    fish_mat$obs_rank = c(
      get_nnIndx(1, m) + 1,
      rep(1:m, times = (n_distinct(fish_mat$ind_pred)-m))
    )
    
  } else { # preds-obs pairs
    
    fish_mat$obs_rank = c(
      rep(1:m, times = n_distinct(fish_mat$ind_pred))
    )
    
  }
  
  nnIndx = fish_mat %>%
    select(-pair_dist, -FC, -weight) %>%
    pivot_wider(
      names_from = obs_rank,
      values_from = ind_obs
    ) %>% 
    ungroup() %>%
    select(-ind_pred) %>%
    as.matrix() %>%
    labelled::remove_attributes("dimnames") %>%
    t() %>%
    matrix(, nrow=1)
  # remove NAs and change to 0-indexing
  nnIndx = nnIndx[!is.na(nnIndx)] - 1
  
  nnDist = fish_mat %>%
    select(-ind_obs, -FC, -weight) %>%
    pivot_wider(
      names_from = obs_rank,
      values_from = pair_dist
    ) %>% 
    ungroup() %>%
    select(-ind_pred) %>%
    as.matrix() %>%
    labelled::remove_attributes("dimnames") %>%
    t() %>%
    matrix(, nrow=1)
  nnDist = nnDist[!is.na(nnDist)]
  
  nnWght = fish_mat %>%
    select(-ind_obs, -FC, -pair_dist) %>%
    pivot_wider(
      names_from = obs_rank,
      values_from = weight
    ) %>% 
    ungroup() %>%
    select(-ind_pred) %>%
    as.matrix() %>%
    labelled::remove_attributes("dimnames") %>%
    t() %>%
    matrix(, nrow=1)
  nnWght = nnWght[!is.na(nnWght)]
  
  if(obs_only){ # for obs-obs distances
    
    n = nrow(obs)
    
    obsobs_dist = as.matrix(sparseMatrix(i = c(fish_pairs$ind_pred, n),
                                         j = c(fish_pairs$ind_obs, n),
                                         x = c(fish_pairs$pair_dist, 0)))
    
    obsobs_wt = as.matrix(sparseMatrix(i = c(fish_pairs$ind_pred, n),
                                       j = c(fish_pairs$ind_obs, n),
                                       x = c(fish_pairs$weight, 0))) + diag(n)
    
    rm(fish_pairs, fish_mat)
    
    cat("Saving distances and spatial weights to file for use in prediction...", fill = TRUE)
    # save to file these lookup matrices for obsobs dists and weights
    save(obsobs_dist, obsobs_wt,
         file = paste0(out_dir, "obsobs_dist_wt.rda"))
    
    neighbors = list(nnIndx = nnIndx, nnDist = nnDist, nnWght = nnWght)
    
    obs_neighbors = add_D_neighbor_vars(neighbors, obsobs_dist, obsobs_wt)
    
    # save to file these neighbor variables
    cat("Saving neighbor variables to file...", fill = TRUE)
    save(obs_neighbors, file = paste0(out_dir, "obs_neighbors.rda"))
    
    obsobs_dist = obsobs_dist + t(obsobs_dist)
    obsobs_wt = obsobs_wt + t(obsobs_wt) - diag(n)
    
    nnIndxObs = matrix(as.integer(0), nrow = n, ncol = m)
    nnDistObs = matrix(0.0, nrow = n, ncol = m)
    nnWghtObs = matrix(0.0, nrow = n, ncol = m)
    
    for(i in 1:n){
      FC_points = which(obsobs_wt[i,] > 0)
      FC_points_sorted = FC_points[order(obsobs_dist[i, FC_points])]
      # prioritize the FC points
      nnIndxObs[i,] = FC_points_sorted[1:m]
      
      # if fewer FC points than m (desired number of nearest neighbors),
      # then add neighbors to replace the remaining NAs
      if(length(FC_points) < m){
        FU_points = which(obsobs_wt[i,] == 0)
        FU_points_sorted = FU_points[order(obsobs_dist[i, FU_points])]
        nnIndxObs[i, (length(FC_points) + 1):m] = FU_points_sorted[1:(m-length(FC_points))]
      }
      
      nnDistObs[i,] = obsobs_dist[i,][nnIndxObs[i,]]
      nnWghtObs[i,] = obsobs_wt[i,][nnIndxObs[i,]]
    }
    
    nnIndxObs = matrix(t(nnIndxObs), nrow=1)
    nnDistObs = matrix(t(nnDistObs), nrow=1)
    nnWghtObs = matrix(t(nnWghtObs), nrow=1)
    
    rm(obsobs_dist, obsobs_wt)
    
    cat("Saving m nearest neighbors for each obs point to file for use in prediction...", fill = TRUE)
    # save to file these lookup matrices for obsobs dists and weights
    save(nnIndxObs, nnDistObs, nnWghtObs,
         file = paste0(out_dir, "obsobs_nns_for_prediction.rda"))
    cat("Done.", fill = TRUE)
    
  } else { # a batch of pred-obs distances
    
    pred_neighbors = list(
      nnIndx = nnIndx,
      nnDist = nnDist,
      nnWght = nnWght
    )
    
    # save to file these neighbor variables
    cat("Saving neighbor variables to file...", fill = TRUE)
    save(pred_neighbors, file = paste0(out_dir, "pred_neighbors_", out_file_suffix))
    cat("Done.", fill = TRUE)
  }
}

# read and combine all the pred-obs nn data
combine_preds_nn_results = function(data_dir, out_dir, m, batch_size){
  
  cat("Reading in files...", fill = TRUE)
  fnames = list.files(data_dir,
                      pattern = paste0(m, "nn_batchsize_", batch_size, ".rda"))
  fn_nn = fnames[str_starts(fnames, "pred_neighbors_")]
  
  nbatches = length(fn_nn)
  
  if(nbatches == 0){
    stop("No files named pred_neighbor_* exist in data_dir")
  }
  
  if(nbatches == 1){
    
    cat("Only one batch", fill = TRUE)
    file.rename(paste0(data_dir, fn_nn[1]), paste0(data_dir, "pred_neighbors.rda"))
    
  } else{
    
    cat(paste("Processing batch", 1, "of", nbatches), fill = TRUE)
    load(paste0(data_dir, fn_nn[1])) # loads object called pred_neighbors
    nnIndx = pred_neighbors$nnIndx
    nnDist = pred_neighbors$nnDist
    nnWght = pred_neighbors$nnWght
    
    for (i in 2:nbatches) {
      cat(paste("Processing batch", i, "of", nbatches), fill = TRUE)
      load(paste0(data_dir, fn_nn[i])) # loads object called pred_neighbors
      
      nnIndx = c(nnIndx, pred_neighbors$nnIndx)
      nnDist = c(nnDist, pred_neighbors$nnDist)
      nnWght = c(nnWght, pred_neighbors$nnWght)
    }
    
    pred_neighbors = list(
      nnIndx = nnIndx,
      nnDist = nnDist,
      nnWght = nnWght
    )
    
    cat("Saving combined data to file...", fill = TRUE)
    save(pred_neighbors, file = paste0(out_dir, "pred_neighbors.rda"))
    cat("Done.", fill = TRUE)
  
  }
}

add_obs_nns_for_prediction = function(obs_dir, out_dir, nnIndxObs, nnDistObs, nnWghtObs){
  load(paste0(out_dir, "pred_neighbors.rda"))
  load(paste0(obs_dir, "obsobs_nns_for_prediction.rda"))
  
  # nnIndxObs is 1-indexed while pred_neighbors$nnIndx is 0-indexed
  nnIndx = c(nnIndxObs-1, pred_neighbors$nnIndx)
  nnDist = c(nnDistObs, pred_neighbors$nnDist)
  nnWght = c(nnWghtObs, pred_neighbors$nnWght)
  
  pred_neighbors = list(
    nnIndx = nnIndx,
    nnDist = nnDist,
    nnWght = nnWght
  )
  
  cat("Saving combined data to file...", fill = TRUE)
  save(pred_neighbors, file = paste0(out_dir, "pred_neighbors.rda"))
  cat("Done.", fill = TRUE)
}

# assumes neighbors already contains nnIndx, nnDist, nnWght
# computes the other six variables to add to the neighbors object
add_D_neighbor_vars = function(neighbors, obsobs_dist, obsobs_wt){
  n = nrow(obsobs_dist)
  
  # make nnIndxLU ----------------------------------------------------------- #
  nnIndxLU_numnn = c(0:m, rep(m, n-m-1))
  nnIndxLU_ind1stnn = c(0, cumsum(nnIndxLU_numnn[1:n-1]))
  
  nnIndxLU = as.integer(c(nnIndxLU_ind1stnn, nnIndxLU_numnn))
  
  # make CIndx -------------------------------------------------------------- #
  CIndx_numDelem = nnIndxLU_numnn^2
  CIndx_ind1stD = c(0, cumsum(CIndx_numDelem[1:n-1]))
  
  CIndx = as.integer(c(CIndx_ind1stD, CIndx_numDelem))
  
  # one row for each 0-indexed obs point i = 0, ..., n-1
  # nn = how many neighbors it has (0, 1, 2, ..., m, m, ..., m)
  # e.g. the row i=3 nn=2 would mean the fourth (3+1th) point has two neighbors
  D_lookup = data.frame(
    i = 0:(n-1),
    nn = nnIndxLU[(n+1):(2*n)]
  )
  
  # nD = number of nonzero elements of D
  # nD = 0 for the first two points i=0,1; 
  # for any other point i > 1, it's the number of pairs of neighbors of that point
  nn_sub = D_lookup$nn[3:nrow(D_lookup)]
  D_lookup$nD = c(0, 0, nn_sub*(nn_sub-1)/2)
  
  # here CIndx is the index of the first element of D corresponding to point i
  D_lookup$CIndx = CIndx[1:n]
  
  # compute l and k as in BRISC
  # e.g. l = 0 picks the first neighbor of a given point i
  # e.g. k = 2 picks the third neighbor of a given point i
  # the length of l and k should equal sum_i(number of pairs of neighbors for point i)
  # l[j], k[j] are a pair of neighbors of a given point, so we need their 
  # pairwise distance and their spatial weight
  l = unlist(map(nn_sub, function(x) get_l(x)))
  k = unlist(map(nn_sub, function(x) get_k(x)))
  
  # create a dataframe D_nonzero with a row for each nonzero element of D
  D_nonzero = data.frame(i = get_nDi(D_lookup$i, D_lookup$nD)) %>%
    left_join(D_lookup, by="i") %>%
    mutate(
      l = l,
      k = k
    ) %>%
    # compute the index (0-indexed) of the element of D that will store the
    # distance between the lth and kth neighbors of point i
    mutate(D_ind = CIndx + l*nn + k) %>%
    select(D_ind, i, l, k) %>%
    # compute the indices of the lth and kth neighbors of point i
    # e.g. the lth neighbor is the nn_lth point in the list of obs points
    # l and nn_l are the same for the first m+1 points, but not after that
    #  - this is because after the first m+1 points, the neighbors of point i  
    #    are some subset of the points 0 through i-1, while for the first m+1 
    #    points the neighbors of point i are exactly the points 0 through i-1, 
    #    and my code lists the neighbors for the first m+1 points in order by 
    #    index (after that they are sorted by increasing distance from point i)
    mutate(nn_l = neighbors$nnIndx[nnIndxLU[i+1]+l+1],
           nn_k = neighbors$nnIndx[nnIndxLU[i+1]+k+1]) %>%
    # create nn_l1 and nn_k1 which swap nn_l and nn_k if necessary so that
    # nn_l1 is always smaller than nn_k1 (l is always smaller than k, but
    # nn_l is not always smaller than nn_k, and we want to know which one
    # is smaller in order to look up whether this distance has already been
    # computed and stored in d so we don't have to recompute it)
    mutate(nn_l1 = pmin(nn_l, nn_k), nn_k1 = pmax(nn_l, nn_k))
  
  ij_pairs = D_nonzero %>%
    select(nn_l1, nn_k1) %>%
    unique() # 72482 obs
  
  ij_pairs$D = unlist(map(1:nrow(ij_pairs),
                          function(x) obsobs_dist[ij_pairs$nn_k1[x] + 1, 
                                                  ij_pairs$nn_l1[x] + 1]))
  ij_pairs$weight = unlist(map(1:nrow(ij_pairs),
                               function(x) obsobs_wt[ij_pairs$nn_k1[x] + 1, 
                                                     ij_pairs$nn_l1[x] + 1]))
  
  D_nonzero = D_nonzero %>%
    left_join(ij_pairs, by=c("nn_l1", "nn_k1")) %>%
    select(D_ind, D, weight) %>%
    # need a -1 here so that the resulting number of elements equals
    # CIndx[n] + CIndx[2*n]
    right_join(data.frame(D_ind = 0:(CIndx[n] + CIndx[2*n]-1)), by="D_ind") %>%
    arrange(D_ind) 
  
  # set weights to 1 along the diagonals of the mxm or smaller matrices represented by D
  diag_inds = unlist(
    map(2:nrow(D_lookup), 
        function(x) get_diag_elem_of_D(
          D_lookup$nn[x], 
          D_lookup$CIndx[x])
    )
  )
  D_nonzero$weight[diag_inds + 1] = 1
  
  # set remaining NAs to zeroes
  D_nonzero$D[is.na(D_nonzero$D)] = 0
  D_nonzero$weight[is.na(D_nonzero$weight)] = 0
  
  neighbors = list( 
    d = neighbors$nnDist, 
    nnWt = neighbors$nnWght,
    nnIndx = neighbors$nnIndx,
    nnIndxLU = nnIndxLU, 
    D = D_nonzero$D,
    Dwt = D_nonzero$weight,
    CIndx = CIndx,
    Length.D = length(D_nonzero$D)
  )
  
  return(neighbors)
}

# check all the dimensions are as expected:
check_obs_nn_vars = function(neighbors, n, m){
  if(length(neighbors$nnIndx) != m*(1+m)/2 + m*(n-m-1)){
    stop(paste("length(neighbors$nnIndx) =", length(neighbors$nnIndx), 
               "but should equal", m*(1+m)/2 + m*(n-m-1)))
  }
  if(length(neighbors$d) != m*(1+m)/2 + m*(n-m-1)){
    stop(paste("length(neighbors$d) =", length(neighbors$d), 
               "but should equal", m*(1+m)/2 + m*(n-m-1)))
  }
  if(length(neighbors$D) != neighbors$CIndx[n] + neighbors$CIndx[2*n]){
    stop(paste("length(neighbors$D) =", length(neighbors$D), 
               "but should equal", neighbors$CIndx[n] + neighbors$CIndx[2*n]))
  }
  if(length(neighbors$nnIndxLU) != 2*n){
    stop(paste("length(neighbors$nnIndxLU) =", length(neighbors$nnIndxLU), 
               "but should equal", 2*n))
  }
  if(length(neighbors$CIndx) != 2*n){
    stop(paste("length(neighbors$CIndx) =", length(neighbors$CIndx), 
               "but should equal", 2*n))
  }
}

# start_ind and end_ind are assumed to be 0-indexed;
# output is 0-indexed
get_nnIndx = function(start_ind, end_ind){
  return(unlist(map(start_ind:end_ind, function(x) 0:(x-1))))
}

# start_ind and end_ind are assumed to be 0-indexed;
# output is 0-indexed
get_pointi = function(start_ind, end_ind){
  return(unlist(map(start_ind:end_ind, function(x) rep(x, x))))
}

get_diag_elem_of_D = function(nn, CIndx){
  if(nn <= 0){ stop("nn must be positive")}
  j = 0:(nn-1)
  return(CIndx + j*(nn+1))
}

get_nDi = function(inds, nD){
  return(unlist(map(inds, function(x) rep(x, nD[x+1]))))
}

get_l = function(nn){
  return(unlist(map(0:(nn-2), function(x) rep(x, nn-1-x))))
}

get_k = function(nn){
  return(unlist(map(1:(nn-1), function(x) x:(nn-1))))
}




# benchmarking and validation ---------------------------------------------

# generates initial data (basic variables for streams, preds, obs)
# then calls benchmark_S3N, then benchmark_SSN
benchmark_and_validate = function(streams, pred_path, network,
                                  nreps_S3N = 10, nreps_SSN = 10,
                                  preproc_only = FALSE,
                                  bench_res_dir = "bench_results/", lsn.path = "lsn"){
  
  message(paste("Starting benchmarking/validation for network", network, "with nreps_S3N", nreps_S3N, "and nreps_SSN", nreps_SSN))
  
  if(preproc_only){
    message("Preprocessing only")
  } else{
    message("Preprocessing and estimation")
  }
  
  # keep only inputs needed for S3N and/or SSN
  streams = select(streams, COMID, LENGTHKM, TotDASqKM, Elevation)
  preds = generate_benchmark_preds(streams, pred_path)
  obs = generate_benchmark_obs(preds)
  
  save(streams, preds, obs, file = paste0(bench_res_dir, "network", network, "_initial_data.rda"))
  rm(streams, preds, obs)
  
  # run S3N code to simulate responses for estimation (S3N is faster for 
  # computing the pwdists necessary to simulate responses)
  if(!is.na(nreps_S3N)){
    benchmark_S3N(network, nreps = nreps_S3N, out_dir = bench_res_dir, 
                  preproc_only = preproc_only)
  }
  if(!is.na(nreps_SSN)){
    benchmark_SSN(network, nreps = nreps_SSN, out_dir = bench_res_dir, 
                  preproc_only = preproc_only, lsn.path = lsn.path)
  }
  message("benchmark_and_validate is done.")
}

# generate responses from an SSN with known spatial parameters
# assumes covariance = exponential tail-up + nugget
#       sigsq: spatial scale parameter
#      lambda: range for spatial stream correlations (phi = 1/lambda)
#       tausq: nugget scale parameter
#        beta: fixed-effect parameters
#   distances: (obsobs_dist) pairwise distances among obs points
#     weights: (obsobs_wt) spatial weights for pairs of obs points
simulate_SSN_data = function(distances, weights, X, 
                             sigsq = 5, lambda = 5, tausq = 0.1, 
                             beta = matrix(c(-44, 0.5), ncol=1)){
  n = nrow(X)
  C = weights*sigsq*exp(-distances/lambda)
  print(dim(C)); print(n)
  C = C + t(C) - sigsq*diag(n)
  Sigma = C + tausq*diag(n)
  y = X %*% beta + t(chol(Sigma)) %*% rnorm(n)
  return(as.numeric(y))
}

generate_benchmark_preds = function(streams, pred_path){
  # read in the prediction points layer
  preds = read_sf(pred_path, "PredictionPoints_MS05_NSI") %>%
    # reassign the duplicate COMIDs so they match up with the streams COMIDs
    reassign_dup_COMIDs() %>%
    # subset to the COMIDs present in the streams subnetwork
    dplyr::filter(COMID %in% streams$COMID) %>%
    # omit any duplicate COMIDs (we don't want to make predictions on these tiny gap-closing segments)
    dplyr::filter(COMID > 0) %>%
    select(COMID) %>%
    left_join(select(st_drop_geometry(streams), COMID, Elevation), by="COMID")
  return(preds)
}

generate_benchmark_obs = function(preds){
  # make obs locations at about half of the pred points
  nobs = min(ceiling(nrow(preds)/2), 10000)
  set.seed(100) # always pick the same points
  preds_sample = sample(preds$COMID, nobs)
  obs = data.frame(COMID = preds_sample)
  return(obs)
}

# identify how many digits needed to represent the number of reps in filenames
# (so that the right number of zeros can be used for padding)
# test: for nreps = 2, 10, 50, 100 this function should return 1, 2, 2, 3
identify_ndigits = function(nreps){
  return(ceiling(log(nreps+0.1, base = 10)))
}

benchmark_S3N = function(network, nreps, out_dir, preproc_only){
  ndigits = identify_ndigits(nreps)
  for(rep in 1:nreps){
    message(paste("Network", network, "S3N benchmark rep", rep, "of", nreps))
    print(paste("out_dir:", out_dir))
    S3N_preproc_and_estimation(network, 
                               str_pad(rep, ndigits, side = "left", pad = "0"),
                               out_dir,
                               preproc_only)
  }
}

benchmark_SSN = function(network, nreps, out_dir, preproc_only, lsn.path){
  ndigits = identify_ndigits(nreps)
  for(rep in 1:nreps){
    message(paste("Network", network, "SSN benchmark rep", rep, "of", nreps))
    print(paste("out_dir:", out_dir))
    SSN_preproc_and_estimation(network, 
                               str_pad(rep, ndigits, side = "left", pad = "0"),
                               out_dir, preproc_only, lsn.path)
  }
}

S3N_preproc_and_estimation = function(network, rep, out_dir, preproc_only = FALSE){
  
  runtimes = rep(NA, 5)
  
  print(paste("Loading", paste0(out_dir, "network", network, "_initial_data.rda")))
  load(paste0(out_dir, "network", network, "_initial_data.rda"))
  
  # configure the stream network
  tic("Build LSN"); start_time = Sys.time()
  streams_res = configure_stream_network(streams)
  streams = streams_res$streams
  stream_graphs = streams_res$sg
  toc(); stop_time = Sys.time()
  runtimes[1] = as.numeric(difftime(stop_time, start_time), units="secs")
  
  # add stream updist vars
  tic("Stream updist and AFV"); start_time = Sys.time()
  streams = compute_stream_updist_vars(streams, stream_graphs)
  toc(); stop_time = Sys.time()
  runtimes[2] = as.numeric(difftime(stop_time, start_time), units="secs")
  
  # add obs to LSN, compute updist and AFV
  tic("Add obs to LSN"); start_time = Sys.time()
  prep_to_compute_pwdist(streams, obs, out_dir = pwdist_input_dir)
  toc(); stop_time = Sys.time()
  runtimes[3] = as.numeric(difftime(stop_time, start_time), units="secs")
  
  # load the results so that updated obs, preds, streams are written to file
  load(paste0(pwdist_input_dir, "preds_obs_pwdist_input_data.RData"))
  
  if(!preproc_only){
    
    tic("Obs-obs distances"); start_time = Sys.time()
    system('cd pwdists; ./scripts/all_obsobs.sh')
    toc(); stop_time = Sys.time()
    runtimes[4] = as.numeric(difftime(stop_time, start_time), units="secs")
    
    load(paste0(pwdist_obsobs_dir, "obsobs_dist_wt.rda"))
    
    preds = left_join(
      preds,
      streams %>%
        st_drop_geometry() %>%
        data.frame() %>%
        select(ptID, Elevation),
      by="ptID"
    )
    
    obs$Elevation = preds$Elevation[1:nrow(obs)]
    
    X = obs %>%
      st_drop_geometry() %>%
      data.frame() %>%
      mutate(Intercept = 1) %>%
      select(Intercept, Elevation) %>%
      as.matrix()
    
    obs$DensityPer100m = simulate_SSN_data(obsobs_dist, obsobs_wt, X)
    
    load(paste0(pwdist_obsobs_dir, "obs_neighbors.rda"))
    
    tic("Estimation"); start_time = Sys.time()
    estimation = BRISC_estimation_stream(
      coords = as.matrix(1:nrow(obs)),
      y = as.matrix(obs$DensityPer100m),
      x = X,
      neighbor = obs_neighbors,
      cov.model = "exponential",
      nugget_status = 1,
      verbose = TRUE
    )
    toc(); stop_time = Sys.time()
    runtimes[5] = as.numeric(difftime(stop_time, start_time), units="secs")
    
    params = c(
      estimation$Beta, # beta (intercept, elevation)
      estimation$Theta # sigma.sq, tau.sq, phi
    )
    params[5] = 1/params[5] # convert phi to lambda (range parameter)
    names(params)[5] = "lambda"
    
    save(streams, obs, preds, params, runtimes,
         file = paste0(out_dir, "S3N_results_network", network, "_rep", rep, ".rda"))
  } else {
    save(streams, obs, preds, runtimes,
         file = paste0(out_dir, "S3N_results_network", network, "_rep", rep, ".rda"))
  }
}

get_preds_batch_info = function(npreds_only, nproc = 4){
  # if npreds_only <= 1000, compute pred-obs dists in one batch with batch size 1000
  if(npreds_only <= 1000){
    return(list(n = 1, size = 1000))
  } else if(npreds_only <= 5000*nproc){
    # if npreds_only between 1000 and 5000*nproc, run four batches with batch size ceiling(npreds_only/4)
    return(list(n = nproc, size = ceiling(npreds_only/4)))
  } else{
    # otherwise, run ceiling(npreds_only/5000) batches with batch size 5000
    return(list(n = ceiling(npreds_only/5000), size = 5000))
  }
}

SSN_preproc_and_estimation = function(network, rep, out_dir, preproc_only, lsn.path){
  
  print(paste("Loading", paste0(out_dir, "S3N_results_network", network, "_rep", rep, ".rda")))
  load(paste0(out_dir, "S3N_results_network", network, "_rep", rep, ".rda"))
  
  # need to do runtimes after loading because otherwise S3N runtimes overwrites the blank runtimes
  runtimes = rep(NA, 6)
  
  tic("Build LSN"); start_time = Sys.time()
  edges <- lines_to_lsn(
    streams = streams,
    lsn_path = lsn.path,
    check_topology = TRUE,
    snap_tolerance = 0.05,
    topo_tolerance = 20,
    overwrite = TRUE
  )
  toc(); stop_time = Sys.time()
  runtimes[1] = as.numeric(difftime(stop_time, start_time), units="secs")
  
  tic("Stream updist and AFV"); start_time = Sys.time()
  edges <- updist_edges(
    edges = edges,
    save_local = TRUE,
    lsn_path = lsn.path,
    calc_length = TRUE
  )
  edges$TotDASqKM[edges$TotDASqKM == 0] = 0.0001
  edges <- afv_edges(
    edges = edges,
    infl_col = "TotDASqKM",
    segpi_col = "areaPI",
    afv_col = "afvArea",
    lsn_path = lsn.path
  )
  toc(); stop_time = Sys.time()
  runtimes[2] = as.numeric(difftime(stop_time, start_time), units="secs")
  
  # add obs to LSN, compute updist and AFV
  tic("Add obs to LSN"); start_time = Sys.time()
  obs <- sites_to_lsn(
    sites = obs,
    edges = edges,
    lsn_path = lsn.path,
    file_name = "obs",
    snap_tolerance = 100,
    save_local = TRUE,
    overwrite = TRUE
  )
  site.list <- updist_sites(
    sites = list(
      obs = obs
    ),
    edges = edges,
    length_col = "Length",
    save_local = TRUE,
    lsn_path = lsn.path
  )
  site.list <- afv_sites(
    sites = site.list,
    edges = edges,
    afv_col = "afvArea",
    save_local = TRUE,
    lsn_path = lsn.path
  )
  toc(); stop_time = Sys.time()
  runtimes[3] = as.numeric(difftime(stop_time, start_time), units="secs")
  
  tic("Assemble SSN"); start_time = Sys.time()
  ssntoy <- ssn_assemble(
    edges = edges,
    lsn_path = lsn.path,
    obs_sites = site.list$obs,
    ssn_path = "validateS3N.ssn",
    import = TRUE,
    check = TRUE,
    afv_col = "afvArea",
    overwrite = TRUE
  )
  toc(); stop_time = Sys.time()
  runtimes[4] = as.numeric(difftime(stop_time, start_time), units="secs")
  
  if(!preproc_only){
    tic("Obs-obs distances"); start_time = Sys.time()
    ssn_create_distmat(ssntoy, overwrite = TRUE)
    toc(); stop_time = Sys.time()
    runtimes[5] = as.numeric(difftime(stop_time, start_time), units="secs")
    
    tic("Estimation"); start_time = Sys.time()
    ssn_mod <- ssn_lm(
      formula = DensityPer100m ~ Elevation,
      ssn.object = ssntoy,
      tailup_type = "exponential",
      taildown_type = "none",
      euclid_type = "none",
      additive = "afvArea"
    )
    toc(); stop_time = Sys.time()
    runtimes[6] = as.numeric(difftime(stop_time, start_time), units="secs")
    
    ssnres = coef(summary(ssn_mod))
    params = c(
      ssnres$fixed[1:2,1], # beta
      ssnres$params_object$tailup[1], # sigmasq
      ssnres$params_object$nugget[1], # tausq
      ssnres$params_object$tailup[2]/10^3 # lambda
    )
    names(params) = c("beta_1", "beta_2", "sigma.sq", "tau.sq", "lambda")
    
    print(runtimes)
    save(params, runtimes,
         file = paste0(out_dir, "SSN_results_network", network, "_rep", rep, ".rda"))
    
  } else{
    print(runtimes)
    save(runtimes,
         file = paste0(out_dir, "SSN_results_network", network, "_rep", rep, ".rda"))
  }
}

combine_runtimes_onemodel = function(bench_res_dir, network, nreps, model, preproc_only){
  
  # SSN has 4 tasks if preproc_only = TRUE, 6 otherwise:
  # "Build LSN" = lines_to_lsn
  # "Stream updist and AFV" = updist_edges and afv_edges
  # "Add obs to LSN" = sites_to_lsn and updist_sites and afv_sites
  # "Assemble SSN" = ssn_assemble
  # if not preproc_only,
  # "Obs-obs distances" = ssn_create_distmat
  # "Estimation" = ssn_lm
  if(model == "SSN"){
    tasks = c("Build LSN", "Stream updist and AFV", "Add obs to LSN", 
              "Assemble SSN",
              "Obs-obs distances", "Estimation")
  }
  
  # S3N has 3 tasks if preproc_only = TRUE, 5 otherwise:
  # "Build LSN" = configure_stream_network
  # "Stream updist and AFV" = compute_stream_updist_vars
  # "Add obs to LSN" = prep_to_compute_pwdist
  # if not preproc_only,
  # "Obs-obs distances" = system('cd pwdists; ./scripts/all_obsobs.sh')
  # "Estimation" = BRISC_estimation_stream
  if(model == "S3N"){
    tasks = c("Build LSN", "Stream updist and AFV", "Add obs to LSN", 
                "Obs-obs distances", "Estimation")
  }
  
  if(is.na(nreps)){
    
    stats = data.frame(
      avg = rep(NA, length(tasks)),
      med = rep(NA, length(tasks)),
      std = rep(NA, length(tasks))
    )
    runtimes_all = NA
    
  } else { # nreps is not NA
    
    ndigits = identify_ndigits(nreps)
    one = str_pad("1", ndigits, side = "left", pad = "0")
    load(paste0(bench_res_dir, model, "_results_network", network, "_rep", one, ".rda"))
    runtimes_all = runtimes
    
    if(nreps > 1){ # combine reps and compute summary statistics
      
      for(rep in 2:nreps){
        rep = str_pad(rep, ndigits, side = "left", pad = "0")
        load(paste0(bench_res_dir, model, "_results_network", network, "_rep", rep, ".rda"))
        runtimes_all = rbind(runtimes_all, runtimes)
      }
      stats = data.frame(
        avg = apply(runtimes_all, 2, mean),
        med = apply(runtimes_all, 2, median),
        std = apply(runtimes_all, 2, sd)
      )
      
    } else{ # nreps = 1
      
      stats = data.frame(
        avg = runtimes_all,
        med = runtimes_all,
        std = NA)
    }
  } # end conditionals to handle nreps = NA, 1, >1
  
  stats$task = tasks
  stats = relocate(stats, task)
    
  return(list(runtimes_all = runtimes_all, stats = stats))
}

combine_runtimes_bothmodels = function(bench_res_dir, network, 
                                       nreps_S3N, nreps_SSN, 
                                       preproc_only){
  runtimes_S3N = combine_runtimes_onemodel(bench_res_dir, network, nreps_S3N, 
                                           "S3N", preproc_only)
  runtimes_SSN = combine_runtimes_onemodel(bench_res_dir, network, nreps_SSN, 
                                           "SSN", preproc_only)
  
  estS3N = runtimes_S3N$stats %>% 
    select(-med) %>%
    rename(S3N_avg = avg, S3N_sd = std)
  
  estSSN = runtimes_SSN$stats %>%
    select(-med) %>%
    rename(SSN_avg = avg, SSN_sd = std)
  obs_only = full_join(estS3N, estSSN, by="task")
  obs_only = add_row(obs_only,
                     task = "Total",
                     S3N_avg = ifelse(
                       # if all SSN avg times are NA,
                       sum(is.na(obs_only$S3N_avg)) == length(obs_only$S3N_avg),
                       # return NA
                       NA,
                       # otherwise return the sum of the non-NA times
                       sum(obs_only$S3N_avg, na.rm = TRUE)
                     ),
                     S3N_sd = NA,
                     SSN_avg = ifelse(
                       # if all SSN avg times are NA,
                       sum(is.na(obs_only$SSN_avg)) == length(obs_only$SSN_avg),
                       # return NA
                       NA,
                       # otherwise return the sum of the non-NA times
                       sum(obs_only$SSN_avg, na.rm = TRUE)
                     ),
                     SSN_sd = NA)
    
    return(list(
      runtimes_S3N = runtimes_S3N, 
      runtimes_SSN = runtimes_SSN,
      obs_only = obs_only
    ))
}

get_summary_for_table = function(network, benchres){
  if(network < 4){
    nwres = data.frame(
      Network = network,
      Task = benchres$obs_only$task[1:3],
      S3N = benchres$obs_only$S3N_avg[c(1,2,4)],
      SSN = c(benchres$obs_only$SSN_avg[1:2],
              sum(benchres$obs_only$SSN_avg[3:4]))
    )
  } else {
    nwres = data.frame(
      Network = network,
      Task = benchres$obs_only$task[1:3],
      S3N = benchres$obs_only$S3N_avg[1:3],
      SSN = benchres$obs_only$SSN_avg[1:3]
    )
  }
  return(nwres)
}

combine_params_onemodel = function(bench_res_dir, network, nreps, model, one){
  ndigits = identify_ndigits(nreps)
  one = str_pad("1", ndigits, side = "left", pad = "0")
  load(paste0(bench_res_dir, model, "_results_network", network, "_rep", one, ".rda"))
  params_all = params
  for(rep in 2:nreps){
    rep = str_pad(rep, ndigits, side = "left", pad = "0")
    load(paste0(bench_res_dir, model, "_results_network", network, "_rep", rep, ".rda"))
    params_all = rbind(params_all, params)
  }
  return(params_all)
}

combine_params_bothmodels = function(bench_res_dir, network, nreps_S3N, nreps_SSN){
  plot_colors = fish(12, option = "Centropyge_loricula")[c(5,10)] # S3N, SSN
  
  S3N = combine_params_onemodel(bench_res_dir, network, nreps_S3N, "S3N")
  rownames(S3N) = NULL; print(S3N)
  
  SSN = combine_params_onemodel(bench_res_dir, network, nreps_SSN, "SSN")
  rownames(SSN) = NULL; print(SSN)
  
  Truth = c(-44, 0.5, 5, 0.1, 5)
  avg_S3N = apply(S3N, 2, mean)
  
  stats = as.data.frame(cbind(Truth, avg_S3N))
  stats$Parameter = rownames(stats)
  rownames(stats) = NULL
  stats = stats[,c("Parameter", "Truth", "avg_S3N")]
  
  stats$avg_SSN = apply(SSN, 2, mean)
  stats$med_S3N = apply(S3N, 2, median)
  stats$med_SSN = apply(SSN, 2, median)
  stats$bias_S3N = stats$avg_S3N - stats$Truth
  stats$bias_SSN = stats$avg_SSN - stats$Truth
  stats$sd_S3N = apply(S3N, 2, sd)
  stats$sd_SSN = apply(SSN, 2, sd)
  # stats = stats[, c("Parameter", "bias_S3N", "bias_SSN", "sd_S3N", "sd_SSN", 
  #                   "avg_S3N", "avg_SSN", "med_S3N", "med_SSN")]
  stats_print = select(stats, Parameter, bias_S3N, bias_SSN, sd_S3N, sd_SSN)
  print(kable(stats_print, format.args = list(big.mark = ",", scientific = FALSE)))
  
  plot_data = as.data.frame(rbind(S3N, SSN))
  plot_data$Model = c(
    rep("S3N", nreps_S3N),
    rep("SSN", nreps_SSN)
    )
  plot_data_long = pivot_longer(plot_data, 
                                cols = beta_1:lambda, 
                                names_to = "Parameter",
                                values_to = "Value") %>%
    # mutate(Truth = case_when(
    #   Parameter == "beta_1" ~ -44,
    #   Parameter == "beta_2" ~ 0.5,
    #   Parameter == "sigma.sq" ~ 5,
    #   Parameter == "tau.sq" ~ 0.1,
    #   Parameter == "lambda" ~ 5
    # )) %>%
    left_join(stats, by="Parameter") %>%
    mutate(Parameter2 = as.factor(case_when(
      # Parameter == "beta_1" ~ "Intercept",
      # Parameter == "beta_2" ~ "Elevation",
      Parameter == "beta_1" ~ "$\\beta_0$",
      Parameter == "beta_2" ~ "$\\beta_1$",
      Parameter == "sigma.sq" ~ "$\\sigma^2$",
      Parameter == "tau.sq" ~ "$\\tau^2$",
      Parameter == "lambda" ~ "$\\lambda$"
    )))
  levels(plot_data_long$Parameter2) <- c(TeX("$\\beta_0$"),
                                         TeX("$\\beta_1$"),
                                         TeX("$\\sigma^2$"),
                                         TeX("$\\tau^2$"),
                                         TeX("$\\lambda$"))
  
  hist_plot = ggplot(plot_data_long, aes(x = Value, fill = Model)) +
    facet_wrap(~ Parameter2, scales = "free", labeller=label_parsed) +
    geom_histogram_pattern(
      aes(pattern = Model), 
      alpha = 0.6,
      # pattern_fill = "black", # Color of the pattern itself
      pattern_density = 0.05
      ) +
    geom_vline(aes(xintercept = Truth, group = Model, 
                   color = "Truth", linetype = "Truth", linewidth = "Truth")) + #, color = "black") +
    geom_vline(aes(xintercept = avg_S3N, group = Model, 
                   color = "S3N", linetype = "S3N", linewidth = "S3N")) +
    geom_vline(aes(xintercept = avg_SSN, group = Model,
                   color = "SSN", linetype = "SSN", linewidth = "SSN")) +
    labs(title = paste("Network", network),
         y = "Count",
         x = "Parameter value") +
    theme_minimal(base_size = 8) +
    scale_fill_manual(values = plot_colors) +
    scale_pattern_manual(values = c("S3N" = "none", "SSN" = "circle")) +
    scale_linetype_manual(values = c("Truth" = "solid", 
                                     "S3N" = "dashed", 
                                     "SSN" = "dotted"), name = "Legend") +
    scale_linewidth_manual(values = c("Truth" = 0.5, 
                                      "S3N" = 0.4, 
                                      "SSN" = 0.4), name = "Legend") +
    scale_color_manual(values = c("Truth" = 'black', 
                                  "S3N" = plot_colors[1], 
                                  "SSN" = plot_colors[2]), name = "Legend") +
                                  # "S3N" = "deeppink3", 
                                  # "SSN" = "deepskyblue3"), name = "Legend") +
    theme(
      legend.title = element_blank(),
      # legend.title=element_text(size=8),
      legend.text = element_text(size = 6),
      legend.direction = "horizontal")
  
  hist_plot = reposition_legend(hist_plot, "center", panel = "panel-3-2")
  
  ggsave(hist_plot, filename = paste0(bench_res_dir, "validation_network", network, ".png"),
         width = 6, height = 3, units = "in")
  
  # return(hist_plot)
}


# utility functions ------------

# to exit from code without saying there was an error
# example: if a preds-obs pairwise distance batch turns out to be unnecessary, 
# print a message explaining why the job will stop prematurely and then stop 
# the job, without saying "Error"
stop_quietly <- function(){
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


# functions for Region 5 -------------

get_regional_estimate_for_species = function(common_name, 
                                             fish_region5, obs, preds,
                                             obs_neighbors, pred_neighbors,
                                             obsobs_dist, obsobs_wt,
                                             nugget_status = 1, verbose = FALSE){
  
  n = nrow(obs)
  
  if(!identical(as.integer(preds$COMID[1:n]), as.integer(obs$COMID))){
    stop("obs COMIDs do not match the first n preds COMIDs")
  }
  
  this_species = fish_region5 %>%
    filter(Common_Name == common_name) %>%
    select(COMID, DensityPer100m)
  
  if(nrow(this_species) != n_distinct(this_species$COMID)){
    stop("COMID is not a unique identifier for this_species")
  }
  
  obs_this_species = obs %>%
    left_join(this_species, by = "COMID") %>%
    # set to 0 if NA because this species was not observed there
    mutate(DensityPer100m = replace_na(DensityPer100m, 0))
  
  cat("Estimating model parameters...", fill=TRUE)
  # other arguments that could be included:
  # sigma.sq = 1, tau.sq = 0.1, phi = 1, nu = 1.5, 
  # search.type = "tree", stabilization = NULL,
  # pred.stabilization = 1e-8, verbose = TRUE, 
  # eps = 2e-05, nugget_status = 1, n_omp = 1
  tic("Estimation"); start_time = Sys.time()
  estimation = BRISC_estimation_stream(
    coords = as.matrix(1:nrow(obs_this_species)),
    y = as.matrix(obs_this_species$DensityPer100m),
    x = get_X(obs_this_species),
    neighbor = obs_neighbors,
    cov.model = "exponential",
    nugget_status = nugget_status,
    verbose = verbose
  )
  stop_time = Sys.time(); toc()
  estimation_time = as.numeric(difftime(stop_time, start_time), units="secs")
  
  # beta_params = estimation$Beta # beta (intercept, elevation)
  # theta_params = estimation$Theta # sigma.sq, tau.sq, phi
  # theta_params["lambda"] = 1/theta_params["phi"] # convert phi to lambda (range parameter)
  
  # at minimum, save estimated parameter values, n_neighbors, common_name, runtime
  
  cat("Predicting fish density across the region...", fill=TRUE)
  # load pred_neighbors object
  tic("Prediction"); start_time = Sys.time()
  prediction = BRISC_prediction_stream(
    BRISC_Out = estimation,
    coords.0 = as.matrix(1:nrow(preds)),
    X.0 = get_X(preds),
    neighbor = pred_neighbors,
    obsobs_dist = obsobs_dist,
    obsobs_wt = obsobs_wt,
    verbose = verbose
  )
  stop_time = Sys.time(); toc()
  prediction_time = as.numeric(difftime(stop_time, start_time), units="secs")
  
  # scale up from prediction to estimate the Region 5 population of this species
  # add up (100* predicted fish density * LENGTHKM) for all pred point COMIDs
  # do same but for each HUC12 or HUC8 to get subregional estimates
  # repeat for other species
  # uncertainty?
  
  cat("Scaling up to a regional estimate...", fill=TRUE)
  tic("Regional estimates"); start_time = Sys.time()
  preds_supp = preds %>%
    st_drop_geometry() %>%
    mutate(DensityPer100m_pred = prediction$prediction, Common_Name = common_name) %>%
    # mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred < 0 | DensityPer100m_pred > 10^6, NA, DensityPer100m_pred)) %>%
    mutate(CountReachTotal_pred = 10*DensityPer100m_pred*LENGTHKM) %>%
    select(COMID, Common_Name, DensityPer100m_pred, CountReachTotal_pred)
  preds_supp$obs_density = NA
  preds_supp$obs_density[1:n] = obs_this_species$DensityPer100m
  
  regional_estimate = sum(preds_supp$CountReachTotal_pred, na.rm = TRUE)
  stop_time = Sys.time(); toc()
  scaleup_time = as.numeric(difftime(stop_time, start_time), units="secs")
  
  return(list(
    common_name = common_name,
    estimation = estimation,
    prediction = prediction,
    preds_supp = preds_supp,
    regional_estimate = regional_estimate,
    estimation_time = estimation_time,
    prediction_time = prediction_time,
    scaleup_time = scaleup_time
  ))
}

# note: each row corresponds to a unique COMID-Scientific_Name pair
# n_distinct(fish) = n_distinct(select(fish, COMID, Scientific_Name)) = 171795 for Region 5
count_each_species_in_fish = function(fish, 
                                      out_dir=NULL, 
                                      filename = "nobs_by_species", 
                                      write_to_csv = TRUE,
                                      write_to_rda = TRUE,
                                      return_obj = FALSE){
  
  # table of number of observations by species
  nobs_by_species = fish %>%
    st_drop_geometry() %>%
    group_by(Scientific_Name, Common_Name) %>%
    summarise(
      TotalCount = sum(CountReachTotal),
      nCOMIDs = n_distinct(COMID)
    ) %>%
    ungroup() %>%
    arrange(desc(TotalCount))
  
  if(write_to_csv){
    write_csv(nobs_by_species, file = paste0(out_dir, filename, ".csv"))
  }
  if(write_to_csv){
    save(nobs_by_species, file = paste0(out_dir, filename, ".rda"))
  }
  if(return_obj){
    return(nobs_by_species)
  }
}

get_obs_sf = function(fish, 
                      common_name, 
                      out_dir = NULL, 
                      filename = NULL, 
                      write_to_file = TRUE){
  fish_stream_vars = fish %>%
    select(COMID, HUC8:binaryID) %>%
    unique()
  
  if(n_distinct(fish_stream_vars$COMID) != nrow(fish_stream_vars)){
    stop("Error: fish_stream_vars does not have one row per COMID")
  }
  
  this_species_fish_data = fish %>%
    filter(Common_Name == common_name) %>%
    select(COMID, Count, DensityPer100m, CountReachTotal)
  
  obs_sf = fish_stream_vars %>%
    # add in fish data for the COMIDs at which this species was observed
    st_join(this_species_fish_data, suffix=c("",".y")) %>%
    select(-ends_with(".y")) %>%
    # at these additional COMIDs, no fish of this species were observed,
    # therefore set Count = 0
    mutate(
      Count = replace_na(Count, 0),
      DensityPer100m = replace_na(DensityPer100m, 0),
      CountReachTotal = replace_na(CountReachTotal, 0)
    )
  
  if(write_to_file){
    # note: writing to csv causes binaryIDs to be stored as Inf
    write_csv(obs_sf, file = paste0(out_dir, filename))
  } else{
    return(obs_sf)
  }
}

# write each species' obs layer to file
get_obs_layer_for_each_species = function(fish, out_dir, region = ""){
  cat(paste("Processing", n_distinct(fish$Common_Name), "species."), fill=TRUE)
  i=1
  for(common_name in unique(fish$Common_Name)){
    cat(paste("Processing species", i))
    obs = get_obs_sf(fish, common_name, out_dir = out_dir, 
                     filename = paste0(str_replace(common_name, " ", "_"), "_", region, ".csv"))
    i = i+1
  }
  cat("Done.", fill=TRUE)
  
}

get_X = function(obs) {
  return(obs %>%
           st_drop_geometry() %>%
           data.frame() %>%
           mutate(Intercept = 1) %>%
           select(Intercept, Development:Fldplain_Dis) %>%
           # select(Intercept, Elevation) %>%
           as.matrix())
}

id_quantile = function(this_value, quantiles){
  return(density_quantiles[which(this_value <= density_quantiles)[1]])
}

plot_pred_density_map = function(streams, preds_supp, common_name, out_dir, 
                                 qtls = NULL, save_to_file = TRUE){
  usa <- st_as_sf(maps::map("state", fill=TRUE, plot=FALSE)) %>%
    st_transform(crs = st_crs(streams))
  
  # density_levels = seq(-2, 2, 0.5)
  
  # discrete
  streams = streams %>%
    left_join(select(preds_supp, COMID, DensityPer100m_pred), by="COMID") %>%
    mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred <= 1e-8, 1e-8, DensityPer100m_pred)) %>%
    mutate(log_density_pred = log10(DensityPer100m_pred))
    # mutate(DensityQuantile = factor(
    #   case_when(
    #     log10(DensityPer100m_pred) <= density_levels[1] ~ as.character(density_levels[1]),
    #     log10(DensityPer100m_pred) > density_levels[1] & log10(DensityPer100m_pred) <= density_levels[2] ~ as.character(density_levels[2]),
    #     log10(DensityPer100m_pred) > density_levels[2] & log10(DensityPer100m_pred) <= density_levels[3]  ~ as.character(density_levels[3]),
    #     log10(DensityPer100m_pred) > density_levels[3] & log10(DensityPer100m_pred) <= density_levels[4]   ~ as.character(density_levels[4]),
    #     log10(DensityPer100m_pred) > density_levels[4] & log10(DensityPer100m_pred) <= density_levels[5]   ~ as.character(density_levels[5]),
    #     log10(DensityPer100m_pred) > density_levels[5] & log10(DensityPer100m_pred) <= density_levels[6]   ~ as.character(density_levels[6]),
    #     log10(DensityPer100m_pred) > density_levels[6] & log10(DensityPer100m_pred) <= density_levels[7]   ~ as.character(density_levels[7]),
    #     log10(DensityPer100m_pred) > density_levels[7] & log10(DensityPer100m_pred) <= density_levels[8]   ~ as.character(density_levels[8]),
    #     log10(DensityPer100m_pred) > density_levels[8] & log10(DensityPer100m_pred) <= density_levels[9]   ~ as.character(density_levels[9]),
    #     log10(DensityPer100m_pred) > density_levels[9] ~ paste0("> ", density_levels[9])
    #   ),
    #   levels = c(as.character(density_levels), paste0("> ", density_levels[9]))
    #   # case_when(
    #   #                                DensityPer100m_pred <= 0.02 ~ "< 0.02",
    #   #   DensityPer100m_pred > 0.02 & DensityPer100m_pred <= 0.34 ~ "0.02 - 0.34",
    #   #   DensityPer100m_pred > 0.34 & DensityPer100m_pred <= 1.3  ~ "0.34 - 1.3",
    #   #   DensityPer100m_pred > 1.30 & DensityPer100m_pred <= 68   ~ "1.3 - 68"
    #   # ),
    #   # levels = c("< 0.02", "0.02 - 0.34", "0.34 - 1.3", "1.3 - 68")
    # ))
  # density_quantiles = as.numeric(quantile(streams$DensityPer100m_pred, probs = seq(0,1,0.1)))
  # streams$DensityQuantile = unlist(map(
  #   streams$DensityPer100m_pred, 
  #   function(x) id_quantile(x, density_quantiles)
  # )) %>%
  #   factor(levels = density_quantiles)
  
  # preds = preds %>%
  #   left_join(select(preds_supp, COMID, DensityPer100m_pred), by="COMID") %>%
  #   mutate(DensityPer100m_pred = ifelse(DensityPer100m_pred < 0, 0.00001, DensityPer100m_pred))
  
  # print(min(streams$DensityPer100m_pred))
  # # print(sum(streams$DensityPer100m_pred <= 0))
  # print(summary(streams$log_density_pred))
  # print(quantile(streams$log_density_pred, probs = seq(0,1,0.1)))
  
  if(is.null(qtls)){
    message("No quantiles provided; computing now...")
    qtls = quantile(streams$log_density_pred, probs = seq(0,1,0.1))
    qtls = unique(round(qtls[qtls > -8], 2))
    print(qtls)
  } else {
    message("Quantiles provided:")
    print(qtls)
  }
  
  # my_colors = pnw_palette(
  #   "Bay", length(qtls), type="continuous")
  # my_colors = scale_color_paletteer_d("rcartocolor::Teal", length(qtls), type="continuous")
  # my_colors = scale_color_paletteer_d("jcolors::pal10", length(qtls))
  # my_colors = paletteer_d("jcolors::pal10", n=length(qtls))
  # print(my_colors)
  
  # plot of Subnetwork 6 with Subnetwork 5 highlighted in colors
  fish_map = ggplot() +
    geom_sf(data = usa,
            color = "#2b2b2b",
            fill = "white") +
    geom_sf(data = st_simplify(streams,
                               preserveTopology = FALSE,
                               dTolerance = 1000),
            # aes(color = DensityQuantile), linewidth = 0.3) +
            # aes(color = DensityPer100m_pred), linewidth = 0.3) +
            aes(color = log_density_pred), linewidth = 0.3) +
    # scale_colour_gradientn(
    #   colors = my_colors,
    #   # high = "#132B43",
    #   # low = "#56B1F7",
    #   labels = qtls #round(qtls, 2)
    # ) +
    scale_color_steps(
    breaks = qtls,
    high = "#132B43",
    low = "#BFEFFF" #"#56B1F7"#,
    # labels = qtls #round(qtls, 2)
      ) +
    # scale_color_manual(values = my_colors)  +
    # scale_color_discrete(name = "Fish density per 100m") +
    # geom_sf(data = preds[preds$DensityPer100m_pred > 10,],
    #         color = "yellow", size = 0.5) +
    # scale_color_gradient(trans = 'log') +
    # scale_color_continuous(breaks=quantile(streams$DensityPer100m_pred)) +
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

combine_two_plots = function(p1, p2, cn1, cn2, out_dir){
  
  comb = ggarrange(
    p1 + theme(legend.position = "none"),
    p2, 
    widths = c(2,2)
  )
  
  # plots <- plot_grid(p1 + theme(legend.position = "none"), 
  #                    p2 + theme(legend.position = "none") + ylab(""),
  #                    align = "vh")
  # 
  # legend <- get_legend(p2)
  # 
  # plot_grid(plots, legend, rel_widths = c(2, 1))
  # # grid.arrange(plots, legend,
  # #              ncol = 2, nrow = 1,
  # #              widths = c(1.5, 1),
  # #              clip = FALSE)
  
  ggsave(comb,
         filename = paste0(out_dir, "Region5_pred_density_map_", 
                           str_replace_all(cn1, " ", "_"),
                           str_replace_all(cn2, " ", "_"), ".png"),
         width = 9, height = 4, units = "in")
}

transform_pred_densities = function(preds_supp){
  transpreds = ifelse(preds_supp$DensityPer100m_pred <= 1e-8, 
                      1e-8, preds_supp$DensityPer100m_pred)
  transpreds = log10(transpreds)
  return(transpreds)
}

make_double_plot = function(streams, 
                            preds_supp1, common_name1, 
                            preds_supp2, common_name2, 
                            out_dir, save_to_file = TRUE){
  
  # get a shared set of values for the color scale legend
  combined_densities = c(
    transform_pred_densities(preds_supp1), 
    transform_pred_densities(preds_supp2)
  )
  
  qtls = quantile(combined_densities, probs = seq(0,1,0.1))
  qtls = unique(round(qtls[qtls > -8], 2)) + 0.01
  print(qtls)
  
  message("Making plot 1...")
  p1 = plot_pred_density_map(streams, preds_supp1, common_name1, out_dir, qtls, save_to_file)
  message("Making plot 2...")
  p2 = plot_pred_density_map(streams, preds_supp2, common_name2, out_dir, qtls, save_to_file)
  
  message("Combining plots...")
  comb = ggarrange(
    p1 + theme(legend.position = "none"),
    p2,
    widths = c(2,2)
  )

  ggsave(comb,
         filename = paste0(out_dir, "Region5_pred_density_map_",
                           str_replace_all(common_name1, " ", "_"),
                           str_replace_all(common_name2, " ", "_"), ".png"),
         width = 9, height = 4, units = "in")
}

