#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library(data.table)
iteration = args[1]
per = c(1:9)
for(percent in per){
  percent = percent*0.1
  print(paste0("Iteration is ", percent*10))
  #trsdata = readRDS(paste0("0.05_trs_sample_p_",percent,"_k3.RDS"))
  trsdata = readRDS(paste0("BootStrap/",iteration,"_0.05_trs_sample_p_",percent,"_k2_random.RDS"))
  print(paste0("Advertiser IDs ", length(unique(trsdata$advertiser_id))))
  # Changing the tdiff and ddiff 
  
  trsdata = data.table(trsdata)
  keys <- c("advertiser_id", "time")
  trsdata <- data.table(trsdata, key = keys)
  trsdata[, tdiff := as.numeric(c(0,diff(time, lag = 1))), by="advertiser_id"]
  # Convert the latitude and longitude to eulicdean space
  
  trsdata[, ddiff := as.numeric(sqrt( ( x - shift( x, 1, type = "lag", fill = NA ) ) ^ 2 +
                                        ( y - shift( y, 1, type = "lag", fill = NA ) ) ^ 2 )), by="advertiser_id"]
  
  # Replace NAs with 0.00 
  trsdata$ddiff[is.na(trsdata$ddiff)] = 0 
  
  
  # Building features
  
  weekday_locs <- function(dt){
    return(as.integer(nrow(dt[dt$weekend == 0])))
  }
  
  weekend_locs <- function(dt){
    return(as.integer(nrow(dt[dt$weekend == 1])))
  }
  
  weekend_distance <- function(dt){
    dt = dt[dt$weekend == 1]
    return(as.double(sum(dt$ddiff)))
  }
  
  weekday_distance <- function(dt, week_num){
    dt = dt[dt$weekend == 0]
    return(as.double(sum(dt$ddiff)))
  }
  
  unique_weekday_locs <- function(dt){
    return(as.integer(uniqueN(dt[dt$weekend == 0][,c("cluster")])))
  }
  
  unique_weekend_locs <- function(dt){
    return(as.integer(uniqueN(dt[dt$week == 1][,c("cluster")])))
  }
  
  
  num_most_frequent_loc <- function(dt, week_num){
    dt = dt[dt$week == week_num]
    freq = dt[,.N , by = c("cluster")]
    return(as.integer(freq[order(-freq$N),][1]$N))
  }
  
  num_sec_most_frequent_loc <- function(dt, week_num){
    dt = dt[dt$week == week_num]
    freq = dt[,.N , by = c("cluster")]
    return(as.integer(freq[order(-freq$N),][2]$N))
  }
  
  time_most_frequent_loc <- function(dt, week_num){
    dt = dt[dt$week == week_num]
    freq = dt[,t:= sum(tdiff) , by = c("cluster")]
    return(as.integer(freq[order(-freq$t),][1]$t/3600))
  }
  
  time_sec_most_frequent_loc <- function(dt, week_num){
    dt = dt[dt$week == week_num]
    freq = dt[,t:= sum(tdiff) , by = c("cluster")]
    return(as.integer(freq[order(-freq$t),][2]$t/3600))
  }
  
  n_mobility_entropy <- function(dt, week_num){
    dt = dt[dt$week == week_num]
    return(as.double(na.omit(entropy(dt[,t:= .N , by = c("cluster")]$t))))
  }
  
  t_mobility_entropy <- function(dt, week_num){
    dt = dt[dt$week == week_num]
    return(as.double(entropy(na.omit(dt[,t:= sum(tdiff) , by = c("cluster")]$t))))
  }
  
  d_mobility_entropy <- function(dt, week_num){
    dt = dt[dt$week == week_num]
    return(as.double(entropy(y = na.omit(dt[,t:= sum(ddiff) , by = c("cluster")]$t))))
  }
  
  all_n_mobility_entropy <- function(dt){
    return(as.double(entropy(y = na.omit(dt[,t:= .N, by = c("cluster")]$t))))
  }
  
  all_d_mobility_entropy <- function(dt){
    return(as.double(entropy(y = na.omit(dt[,t:= sum(ddiff) , by = c("cluster")]$t))))
  }
  
  all_t_mobility_entropy <- function(dt){
    return(as.double(entropy(y = na.omit(dt[,t:= sum(tdiff) , by = c("cluster")]$t))))
  }
  
  n_radius_of_gyration_week <- function(dt, week_num){
    dt = dt[dt$week == week_num]
    freq = dt[, t:= .N , by = c("x","y")][,c("x","y","t")]
    return(as.double(sqrt(sum((freq$t*(freq$x - mean(freq$x))^2 + freq$t*(freq$y - mean(freq$y))^2)/nrow(freq)))))
  }
  
  n_radius_of_gyration <- function(dt){
    freq = dt[, t:= .N , by = c("x","y")][,c("x","y","t")]
    return(as.double(sqrt(sum((freq$t*(freq$x - mean(freq$x))^2 + freq$t*(freq$y - mean(freq$y))^2)/nrow(freq)))))
  }
  
  t_radius_of_gyration_week <- function(dt, week_num){
    dt = dt[dt$week == week_num]
    freq = dt[, t:= sum(tdiff) , by = c("x","y")][,c("x","y","t")]
    return(as.double(sqrt(sum((freq$t*(freq$x - mean(freq$x))^2 + freq$t*(freq$y - mean(freq$y))^2)/nrow(freq)))))
  }
  
  t_radius_of_gyration <- function(dt){
    freq = dt[, t:= sum(tdiff) , by = c("x","y")][,c("x","y","t")]
    return(as.double(sqrt(sum((freq$t*(freq$x - mean(freq$x))^2 + freq$t*(freq$y - mean(freq$y))^2)/nrow(freq)))))
  }
  
  library(entropy)
  
  summary_weekday = trsdata[trsdata$weekend == 0,][,  .(weekday_week_avg_locs = .N/5,
                                                        
                                                        weekday_week_avg_distance = sum(ddiff)/5,
                                                        
                                                        weekday_week_avg_unique_locs = uniqueN(.SD[,c("cluster")])/5,
                                                        
                                                        weekday_n_rog = n_radius_of_gyration(.SD[,c("x","y")]),
                                                        
                                                        weekday_t_rog = t_radius_of_gyration(.SD[,c("x","y","tdiff")]),
                                                        
                                                        weekday_n_entropy = all_n_mobility_entropy(.SD[,c("cluster")]),
                                                        
                                                        weekday_t_entropy = all_t_mobility_entropy(.SD[,c("cluster","tdiff")]),
                                                        
                                                        weekday_d_entropy = all_d_mobility_entropy(.SD[,c("cluster","ddiff")]),
                                                        
                                                        weekday_mean_dwell = mean(tdiff)/3600,
                                                        
                                                        weekday_avg_distance = mean(ddiff)), 
                                                   by = .(advertiser_id)]
  
  summary_weekend = trsdata[trsdata$weekend == 1,][,  .(weekend_week_avg_locs = .N/5,
                                                        
                                                        weekend_week_avg_distance = sum(ddiff)/5,
                                                        
                                                        weekend_week_avg_unique_locs = uniqueN(.SD[,c("cluster")])/5,
                                                        
                                                        weekend_n_rog = n_radius_of_gyration(.SD[,c("x","y")]),
                                                        
                                                        weekend_t_rog = t_radius_of_gyration(.SD[,c("x","y","tdiff")]),
                                                        
                                                        weekend_n_entropy = all_n_mobility_entropy(.SD[,c("cluster")]),
                                                        
                                                        weekend_t_entropy = all_t_mobility_entropy(.SD[,c("cluster","tdiff")]),
                                                        
                                                        weekend_d_entropy = all_d_mobility_entropy(.SD[,c("cluster","ddiff")]),
                                                        
                                                        weekend_mean_dwell = mean(tdiff)/3600,
                                                        
                                                        weekend_avg_distance = mean(ddiff)), 
                                                   by = .(advertiser_id)]
  
  summary_whole = trsdata[,  .(whole_week_avg_locs = .N/5,
                               
                               whole_week_avg_distance = sum(ddiff)/5,
                               
                               whole_week_avg_unique_locs = uniqueN(.SD[,c("cluster")])/5,
                               
                               whole_n_rog = n_radius_of_gyration(.SD[,c("x","y")]),
                               
                               whole_t_rog = t_radius_of_gyration(.SD[,c("x","y","tdiff")]),
                               
                               whole_n_entropy = all_n_mobility_entropy(.SD[,c("cluster")]),
                               
                               whole_t_entropy = all_t_mobility_entropy(.SD[,c("cluster","tdiff")]),
                               
                               whole_d_entropy = all_d_mobility_entropy(.SD[,c("cluster","ddiff")]),
                               
                               whole_mean_dwell = mean(tdiff)/3600,
                               
                               whole_avg_distance = mean(ddiff)), 
                          by = .(advertiser_id)]
  
  print(nrow(summary_whole))
  
  summary_whole <- data.table(summary_whole, key="advertiser_id")
  summary_weekday <- data.table(summary_weekday, key="advertiser_id")
  summary_weekend<- data.table(summary_weekend, key="advertiser_id")
  
  mobility = summary_weekday[summary_whole,]
  mobility  = summary_weekend[mobility,]
  
  
  
  total_distance <- function(x)
  {
    return(sum(as.double(x)))
  }
  
  total_time <- function(x)
  {
    return(sum(as.double(x)))
  }
  
  total_visits <-function(x, weekend)
  {
    return(length(unique(x)))
  }
  
  sdata = trsdata
  sdata$id = as.numeric(as.factor(sdata$advertiser_id))
  
  library(data.table)
  freq_sample_user_loc_all = data.table::dcast(sdata, id + advertiser_id ~ cluster, 
                                               value.var = list("time","ddiff","tdiff"), 
                                               fun = list(total_visits,total_distance,total_time))
  
  freq_sample_user_loc_weekend = data.table::dcast(sdata[sdata$weekend ==1,] , 
                                                   id + advertiser_id ~ cluster, value.var = list("time","ddiff","tdiff"), 
                                                   fun = list(total_visits,total_distance,total_time))
  # Weekday
  freq_sample_user_loc_weekday = data.table::dcast(sdata[sdata$weekend ==0,] , 
                                                   id + advertiser_id ~ cluster, value.var = list("time","ddiff","tdiff"), 
                                                   fun = list(total_visits,total_distance,total_time))
  
  nrow(freq_sample_user_loc_all)
  
  
  # Extract the corresponding columns and perform SVD
  
  # Weekday
  library("irlba")
  mat_snapshot = freq_sample_user_loc_weekday
  freq_names = grep("time_total_visits_", names(mat_snapshot), value = TRUE)
  snap_mat = scale(data.matrix(subset(mat_snapshot,select = freq_names)), scale = FALSE, center = TRUE)
  svd_freq = svdr(snap_mat, k = 5)
  feat_freq = as.data.frame(svd_freq$u)
  names(feat_freq) = paste0("weekday_visits_svd","_",seq(1:5))
  
  dist_names = grep("ddiff_total_distance_", names(mat_snapshot), value = TRUE)
  snap_mat = scale(data.matrix(subset(mat_snapshot,select = dist_names)), center = TRUE, scale = FALSE)
  svd_dist = irlba(snap_mat, nu = 5, tol = 1e-03)
  feat_dist = as.data.frame(svd_dist$u)
  names(feat_dist) = paste0("weekday_dist_svd","_",seq(1:5))
  
  time_names = grep("tdiff_total_time_", names(mat_snapshot), value = TRUE)
  snap_mat = scale(data.matrix(subset(mat_snapshot,select = time_names)), center = TRUE, scale = FALSE)
  svd_time = irlba(snap_mat, nu = 5, tol = 1e-03)
  feat_time = as.data.frame(svd_time$u)
  names(feat_time) = paste0("weekday_time_svd","_",seq(1:5))
  
  feat_svd_weekday = cbind(mat_snapshot[, c("advertiser_id","id")], feat_freq, feat_dist, feat_time)
  
  #Weekend
  
  mat_snapshot = freq_sample_user_loc_weekend
  freq_names = grep("time_total_visits_", names(mat_snapshot), value = TRUE)
  snap_mat = scale(data.matrix(subset(mat_snapshot,select = freq_names)), scale = FALSE, center = TRUE)
  svd_freq = svdr(snap_mat, k = 5)
  feat_freq = as.data.frame(svd_freq$u)
  names(feat_freq) = paste0("weekend_visits_svd","_",seq(1:5))
  
  dist_names = grep("ddiff_total_distance_", names(mat_snapshot), value = TRUE)
  snap_mat = scale(data.matrix(subset(mat_snapshot,select = dist_names)), center = TRUE, scale = FALSE)
  svd_dist = svdr(snap_mat, k = 5)
  feat_dist = as.data.frame(svd_dist$u)
  names(feat_dist) = paste0("weekend_dist_svd","_",seq(1:5))
  
  time_names = grep("tdiff_total_time_", names(mat_snapshot), value = TRUE)
  snap_mat = scale(data.matrix(subset(mat_snapshot,select = time_names)), center = TRUE, scale = FALSE)
  svd_time = irlba(snap_mat, nu = 5, tol = 1e-03)
  feat_time = as.data.frame(svd_time$u)
  names(feat_time) = paste0("weekend_time_svd","_",seq(1:5))
  
  feat_svd_weekend = cbind(mat_snapshot[, c("advertiser_id","id")], feat_freq, feat_dist, feat_time)
  
  
  #All
  
  mat_snapshot = freq_sample_user_loc_all
  freq_names = grep("time_total_visits_", names(mat_snapshot), value = TRUE)
  snap_mat = scale(data.matrix(subset(mat_snapshot,select = freq_names)), scale = FALSE, center = TRUE)
  svd_freq = svdr(snap_mat, k = 5)
  feat_freq = as.data.frame(svd_freq$u)
  names(feat_freq) = paste0("whole_visits_svd","_",seq(1:5))
  
  dist_names = grep("ddiff_total_distance_", names(mat_snapshot), value = TRUE)
  snap_mat = scale(data.matrix(subset(mat_snapshot,select = dist_names)), center = TRUE, scale = FALSE)
  svd_dist = svdr(snap_mat, k = 5)
  feat_dist = as.data.frame(svd_dist$u)
  names(feat_dist) = paste0("whole_dist_svd","_",seq(1:5))
  
  time_names = grep("tdiff_total_time_", names(mat_snapshot), value = TRUE)
  snap_mat = scale(data.matrix(subset(mat_snapshot,select = time_names)), center = TRUE, scale = FALSE)
  svd_time = irlba(snap_mat, nu = 5, tol = 1e-03)
  feat_time = as.data.frame(svd_time$u)
  names(feat_time) = paste0("whole_time_svd","_",seq(1:5))
  
  feat_svd_all = cbind(mat_snapshot[, c("advertiser_id","id")], feat_freq, feat_dist, feat_time)
  
  feat_svd_all <- data.table(feat_svd_all, key=c("advertiser_id", "id"))
  feat_svd_weekday <- data.table(feat_svd_weekday, key=c("advertiser_id", "id"))
  feat_svd_weekend <- data.table(feat_svd_weekend, key=c("advertiser_id", "id"))
  
  join = feat_svd_weekday[feat_svd_all,]
  svd_join  = feat_svd_weekend[join,]
  full_join_svd = data.table(svd_join[mobility,])
  
  nrow(full_join_svd)
  
  #Weekday
  
  # Frequency
  sdata = trsdata
  m <- xtabs(~advertiser_id+cluster,sdata[sdata$weekend == 0])
  freq_user_user = tcrossprod(m,m)
  library(irlba)
  snap_mat = scale(data.matrix(freq_user_user), scale = FALSE, center = TRUE)
  svd_freq_uu = svdr(snap_mat, k = 5)
  feat_freq_uu = as.data.frame(svd_freq_uu$u)
  names(feat_freq_uu) = paste0("weekday_user_visits_svd","_",seq(1:5))
  
  # Distance
  sdata$ddiff = as.numeric(sdata$ddiff)
  m_d <- xtabs(ddiff~advertiser_id+cluster,sdata[sdata$weekend == 0])
  dist_user_user = tcrossprod(m_d,m)
  
  # Take the mean
  dist_user_user = (dist_user_user + t(dist_user_user))/2
  
  snap_mat = scale(data.matrix(dist_user_user), scale = FALSE, center = TRUE)
  svd_dist_uu = svdr(snap_mat, k = 5)
  feat_dist_uu = as.data.frame(svd_dist_uu$u)
  names(feat_dist_uu) = paste0("weekday_user_distance_svd","_",seq(1:5))
  
  # Time
  sdata$tdiff = as.numeric(sdata$tdiff)
  m_t <- xtabs(tdiff~advertiser_id+cluster,sdata[sdata$weekend == 0])
  time_user_user = tcrossprod(m_t,m)
  
  # Take the mean
  time_user_user = (time_user_user + t(time_user_user))/2
  
  snap_mat = scale(data.matrix(time_user_user), scale = FALSE, center = TRUE)
  svd_time_uu = svdr(snap_mat, k = 5)
  feat_time_uu = as.data.frame(svd_time_uu$u)
  names(feat_time_uu) = paste0("weekday_user_time_svd","_",seq(1:5))
  
  feat_freq_uu$advertiser_id = NULL
  feat_dist_uu$advertiser_id = NULL
  feat_time_uu$advertiser_id = row.names(time_user_user)
  feat_weekday_uu_svd = cbind(feat_freq_uu, feat_dist_uu, feat_time_uu)
  
  #Weekend
  
  # Frequency
  sdata = trsdata
  m <- xtabs(~advertiser_id+cluster,sdata[sdata$weekend == 1])
  freq_user_user = tcrossprod(m,m)
  library(irlba)
  snap_mat = scale(data.matrix(freq_user_user), scale = FALSE, center = TRUE)
  svd_freq_uu = svdr(snap_mat, k = 5)
  feat_freq_uu = as.data.frame(svd_freq_uu$u)
  names(feat_freq_uu) = paste0("weekend_user_visits_svd","_",seq(1:5))
  
  # Distance
  sdata$ddiff = as.numeric(sdata$ddiff)
  m_d <- xtabs(ddiff~advertiser_id+cluster,sdata[sdata$weekend == 1])
  dist_user_user = tcrossprod(m_d,m)
  
  # Take the mean
  dist_user_user = (dist_user_user + t(dist_user_user))/2
  
  snap_mat = scale(data.matrix(dist_user_user), scale = FALSE, center = TRUE)
  svd_dist_uu = svdr(snap_mat, k = 5)
  feat_dist_uu = as.data.frame(svd_dist_uu$u)
  names(feat_dist_uu) = paste0("weekend_user_distance_svd","_",seq(1:5))
  
  # Time
  sdata$tdiff = as.numeric(sdata$tdiff)
  m_t <- xtabs(tdiff~advertiser_id+cluster,sdata[sdata$weekend == 1])
  time_user_user = tcrossprod(m_t,m)
  
  # Take the mean
  time_user_user = (time_user_user + t(time_user_user))/2
  
  snap_mat = scale(data.matrix(time_user_user), scale = FALSE, center = TRUE)
  svd_time_uu = svdr(snap_mat, k = 5)
  feat_time_uu = as.data.frame(svd_time_uu$u)
  names(feat_time_uu) = paste0("weekend_user_time_svd","_",seq(1:5))
  
  feat_freq_uu$advertiser_id = NULL
  feat_dist_uu$advertiser_id = NULL
  feat_time_uu$advertiser_id = row.names(time_user_user)
  feat_weekend_uu_svd = cbind(feat_freq_uu, feat_dist_uu, feat_time_uu)
  
  #Whole
  
  # # Frequency
  sdata = trsdata
  m <- xtabs(~advertiser_id+cluster,sdata)
  freq_user_user = tcrossprod(m,m)
  library(irlba)
  snap_mat = scale(data.matrix(freq_user_user), scale = FALSE, center = TRUE)
  svd_freq_uu = svdr(snap_mat, k = 5)
  feat_freq_uu = as.data.frame(svd_freq_uu$u)
  names(feat_freq_uu) = paste0("whole_user_visits_svd","_",seq(1:5))
  
  # Distance
  sdata$ddiff = as.numeric(sdata$ddiff)
  m_d <- xtabs(ddiff~advertiser_id+cluster,sdata)
  dist_user_user = tcrossprod(m_d,m)
  
  # Take the mean
  dist_user_user = (dist_user_user + t(dist_user_user))/2
  
  snap_mat = scale(data.matrix(dist_user_user), scale = FALSE, center = TRUE)
  svd_dist_uu = svdr(snap_mat, k = 5)
  feat_dist_uu = as.data.frame(svd_dist_uu$u)
  names(feat_dist_uu) = paste0("whole_user_distance_svd","_",seq(1:5))
  
  # Time
  sdata$tdiff = as.numeric(sdata$tdiff)
  m_t <- xtabs(tdiff~advertiser_id+cluster,sdata)
  time_user_user = tcrossprod(m_t,m)
  
  # Take the mean
  time_user_user = (time_user_user + t(time_user_user))/2
  
  snap_mat = scale(data.matrix(time_user_user), scale = FALSE, center = TRUE)
  svd_time_uu = svdr(snap_mat, k = 5)
  feat_time_uu = as.data.frame(svd_time_uu$u)
  names(feat_time_uu) = paste0("whole_user_time_svd","_",seq(1:5))
  
  feat_freq_uu$advertiser_id = NULL
  feat_dist_uu$advertiser_id = NULL
  feat_time_uu$advertiser_id = row.names(time_user_user)
  feat_whole_uu_svd = cbind(feat_freq_uu, feat_dist_uu, feat_time_uu)
  
  fdata = data.table(full_join_svd, key = c("advertiser_id"))
  feat_whole_uu_svd = data.table(feat_whole_uu_svd, key = c("advertiser_id"))
  feat_weekend_uu_svd = data.table(feat_weekend_uu_svd, key = c("advertiser_id"))
  feat_weekday_uu_svd = data.table(feat_weekday_uu_svd, key = c("advertiser_id"))
  
  join = feat_weekday_uu_svd[feat_whole_uu_svd,]
  svd_join  = feat_weekend_uu_svd[join,]
  full_join_svd_user = data.table(svd_join[fdata,])
  
  saveRDS(full_join_svd_user,paste0("BootStrap/",iteration,"_0.05_split_features_p_",percent,"_v1_k2_random.RDS"))
  #saveRDS(full_join_svd_user,paste0("0.05_split_features_notworking_v1.RDS"))
  
  
  
  ### Time splitting ## 
  #trsdata = readRDS(paste0("0.05_trs_sample_p_",percent,"_k3.RDS"))
  trsdata = readRDS(paste0("BootStrap/",iteration,"_0.05_trs_sample_p_",percent,"_k2_random.RDS"))
  #sdata = sdata[!(sdata$cluster ==0),]
  #sdata$cluster = sdata$clusternew
  # Split into week 1-4 and 5
  
  length(unique(trsdata$advertiser_id))
  
  
  # Tensor for locations
  sdata = trsdata
  total_distance <- function(x)
  {
    return(sum(as.double(x)))
  }
  
  total_time <- function(x)
  {
    return(sum(as.double(x)))
  }
  
  total_visits <-function(x)
  {
    return(length(unique(x)))
  }
  
  
  sdata = data.table(sdata,key = "advertiser_id")
  
  all_temp_mat = data.table::dcast(sdata, advertiser_id + week ~ cluster, 
                                   value.var = list("time", "tdiff", "ddiff"),
                                   fun = list(total_visits, total_time, total_distance),
                                   fill = 0, drop = c(FALSE, TRUE))
  
  # Frequency 
  freq_names = grep("time_total_visits_", names(all_temp_mat), value = TRUE)
  library(tensr)
  nweeks = length(unique(sdata$week))
  p <- c(nrow(all_temp_mat)/nweeks, 100, nweeks)
  freq_tnsr = array(stats::rnorm(prod(p)), dim = p)
  all_temp_mat =  data.table(all_temp_mat, key = "advertiser_id")                       
  #freq_mat = subset(all_temp_mat, select = freq_names)  
  library("irlba")
  for(i in 1:length(unique(sdata$week)))
  {
    freq_mat = subset(all_temp_mat[all_temp_mat$week == unique(sdata$week)[i]], 
                      select = freq_names)  
    
    snap_mat = scale(data.matrix(freq_mat), scale = FALSE, center = TRUE)
    svd_freq = svdr(snap_mat, k = 100)
    print(i)
    freq_tnsr[,,i] = svd_freq$u
  }
  
  saveRDS(freq_tnsr,paste0(iteration,"_split_freq_week_tensor_alt.RDS"))
  
  # Distance 
  freq_names = grep("ddiff_total_distance_", names(all_temp_mat), value = TRUE)
  library(tensr)
  nweeks = length(unique(sdata$week))
  p <- c(nrow(all_temp_mat)/nweeks, 100, nweeks)
  freq_tnsr = array(stats::rnorm(prod(p)), dim = p)
  #all_temp_mat =  data.table(all_temp_mat, key = "advertiser_id")                       
  #freq_mat = subset(all_temp_mat, select = freq_names)  
  library("irlba")
  for(i in 1:length(unique(sdata$week)))
  {
    freq_mat = subset(all_temp_mat[all_temp_mat$week == unique(sdata$week)[i]], 
                      select = freq_names)  
    
    snap_mat = scale(data.matrix(freq_mat), scale = FALSE, center = TRUE)
    svd_freq = svdr(snap_mat, k = 100)
    print(i)
    freq_tnsr[,,i] = svd_freq$u
  }
  
  saveRDS(freq_tnsr,paste0(iteration,"_split_dist_week_tensor_alt.RDS"))
  
  # Time
  freq_names = grep("tdiff_total_time_", names(all_temp_mat), value = TRUE)
  library(tensr)
  nweeks = length(unique(sdata$week))
  p <- c(nrow(all_temp_mat)/nweeks, 100, nweeks)
  freq_tnsr = array(stats::rnorm(prod(p)), dim = p)
  #all_temp_mat =  data.table(all_temp_mat, key = "advertiser_id")                       
  #freq_mat = subset(all_temp_mat, select = freq_names)  
  library("irlba")
  for(i in 1:length(unique(sdata$week)))
  {
    freq_mat = subset(all_temp_mat[all_temp_mat$week == unique(sdata$week)[i]], 
                      select = freq_names)  
    
    snap_mat = scale(data.matrix(freq_mat), scale = FALSE, center = TRUE)
    svd_freq = svdr(snap_mat, k = 100)
    print(i)
    freq_tnsr[,,i] = svd_freq$u
  }
  
  saveRDS(freq_tnsr,paste0(iteration,"_split_time_week_tensor_alt.RDS"))
  
  #freq_tnsr = readRDS("freq_week_tensor.RDS")
  
  #sample = freq_tnsr[,1:1000,]
  freq_tnsr = readRDS(paste0(iteration,"_split_freq_week_tensor_alt.RDS"))
  ranks = c(5, 1, 1)
  hooi_x <- hosvd(freq_tnsr, r = ranks)
  saveRDS(hooi_x$U, paste0(iteration,"_split_freq_week_tensor_alt_svd.RDS"))
  
  freq_tnsr = readRDS(paste0(iteration,"_split_dist_week_tensor_alt.RDS"))
  ranks = c(5, 1, 1)
  hooi_x <- hosvd(freq_tnsr, r = ranks)
  saveRDS(hooi_x$U, paste0(iteration,"_split_dist_week_tensor_alt_svd.RDS"))
  
  freq_tnsr = readRDS(paste0(iteration,"_split_time_week_tensor_alt.RDS"))
  ranks = c(5, 1, 1)
  hooi_x <- hosvd(freq_tnsr, r = ranks)
  saveRDS(hooi_x$U, paste0(iteration,"_split_time_week_tensor_alt_svd.RDS"))
  
  # Users
  
  m <- xtabs(~advertiser_id+cluster+week,sdata)
  
  nweeks = length(unique(sdata$week))
  p <- c(dim(m)[1], 100, nweeks)
  freq_tnsr = array(stats::rnorm(prod(p)), dim = p)
  library("irlba")
  for(i in 1:length(unique(sdata$week)))
  {
    freq_mat = m[,,i]  
    freq_user_user = tcrossprod(freq_mat,freq_mat)
    
    snap_mat = scale(data.matrix(freq_user_user), scale = FALSE, center = TRUE)
    svd_freq = svdr(snap_mat, k = 100)
    print(i)
    freq_tnsr[,,i] = svd_freq$u
  }
  saveRDS(freq_tnsr,paste0(iteration,"_split_freq_week_user_tensor.RDS"))
  
  m_d <- xtabs(ddiff~advertiser_id+cluster+week,sdata)
  
  nweeks = length(unique(sdata$week))
  p <- c(dim(m)[1], 100, nweeks)
  freq_tnsr = array(stats::rnorm(prod(p)), dim = p)
  library("irlba")
  for(i in 1:length(unique(sdata$week)))
  {
    freq_mat = m[,,i]  
    dist_mat = m_d[,,i]
    freq_user_user = tcrossprod(freq_mat,dist_mat)
    
    snap_mat = scale(data.matrix(freq_user_user), scale = FALSE, center = TRUE)
    svd_freq = svdr(snap_mat, k = 100)
    print(i)
    freq_tnsr[,,i] = svd_freq$u
  }
  saveRDS(freq_tnsr,paste0(iteration,"_split_dist_week_user_tensor.RDS"))
  
  
  m_t <- xtabs(tdiff~advertiser_id+cluster+week,sdata)
  
  nweeks = length(unique(sdata$week))
  p <- c(dim(m)[1], 100, nweeks)
  freq_tnsr = array(stats::rnorm(prod(p)), dim = p)
  library("irlba")
  for(i in 1:length(unique(sdata$week)))
  {
    freq_mat = m[,,i]  
    time_mat = m_t[,,i]
    freq_user_user = tcrossprod(freq_mat,time_mat)
    
    snap_mat = scale(data.matrix(freq_user_user), scale = FALSE, center = TRUE)
    svd_freq = svdr(snap_mat, k = 100)
    print(i)
    freq_tnsr[,,i] = svd_freq$u
  }
  saveRDS(freq_tnsr,paste0(iteration,"_split_time_week_user_tensor.RDS"))
  
  library(tensr)
  freq_tnsr = readRDS(paste0(iteration,"_split_freq_week_user_tensor.RDS"))
  ranks = c(5, 1, 1)
  hooi_x <- hosvd(freq_tnsr, r = ranks)
  saveRDS(hooi_x$U, paste0(iteration,"_split_freq_week_user_tensor_svd.RDS"))
  
  freq_tnsr = readRDS(paste0(iteration,"_split_dist_week_user_tensor.RDS"))
  ranks = c(5, 1, 1)
  hooi_x <- hosvd(freq_tnsr, r = ranks)
  saveRDS(hooi_x$U, paste0(iteration,"_split_dist_week_user_tensor_svd.RDS"))
  
  freq_tnsr = readRDS(paste0(iteration,"_split_time_week_user_tensor.RDS"))
  ranks = c(5, 1, 1)
  hooi_x <- hosvd(freq_tnsr, r = ranks)
  saveRDS(hooi_x$U, paste0(iteration,"_split_time_week_user_tensor_svd.RDS"))
  
  
  ## Saving into the other features
  sdata = readRDS(paste0("BootStrap/",iteration,"_0.05_split_features_p_",percent,"_v1_k2_random.RDS"))
  #sdata = readRDS(paste0("0.05_split_features_notworking_v1.RDS"))
  f_t = readRDS(paste0(iteration,"_split_freq_week_tensor_alt_svd.RDS"))
  d_t = readRDS(paste0(iteration,"_split_dist_week_tensor_alt_svd.RDS"))
  t_t = readRDS(paste0(iteration,"_split_time_week_tensor_alt_svd.RDS"))
  f_t = as.data.frame(f_t[1])
  names(f_t) = paste0("freq_tensor_svd_alt","_",seq(1:5))
  
  d_t = as.data.frame(d_t[1])
  names(d_t) = paste0("dist_tensor_svd_alt","_",seq(1:5))
  
  t_t = as.data.frame(t_t[1])
  names(t_t) = paste0("time_tensor_svd_alt","_",seq(1:5))
  
  tdata = cbind(sdata,f_t,d_t,t_t)
  
  f_t = readRDS(paste0(iteration,"_split_freq_week_user_tensor_svd.RDS"))
  d_t = readRDS(paste0(iteration,"_split_dist_week_user_tensor_svd.RDS"))
  t_t = readRDS(paste0(iteration,"_split_time_week_user_tensor_svd.RDS"))
  f_t = as.data.frame(f_t[1])
  names(f_t) = paste0("freq_user_tensor_svd_alt","_",seq(1:5))
  
  d_t = as.data.frame(d_t[1])
  names(d_t) = paste0("dist_user_tensor_svd_alt","_",seq(1:5))
  
  t_t = as.data.frame(t_t[1])
  names(t_t) = paste0("time_user_tensor_svd_alt","_",seq(1:5))
  
  tdata = cbind(tdata,f_t,d_t,t_t)
  saveRDS(tdata,paste0("BootStrap/",iteration,"_0.05_split_features_p_",percent,"_v2_k2_random.RDS"))
  print(nrow(tdata))
  #saveRDS(tdata,paste0("0.05_split_features_notworking_v2.RDS"))
  
  # KNN 
  
  ## Whole Mobility + Alt. Loc. and User Context in Tensor
  fdata = data.table(readRDS(paste0("BootStrap/",iteration,"_0.05_split_features_p_",percent,"_v2_k2_random.RDS")))
  #fdata = data.table(readRDS(paste0("0.05_split_features_notworking_v2.RDS")))
  
  feature_names = c('whole_week_avg_locs' ,'whole_week_avg_distance' ,'whole_week_avg_unique_locs', 'whole_n_rog' ,
                    'whole_n_entropy', 'whole_t_entropy' ,'whole_d_entropy', 'whole_mean_dwell', 'whole_avg_distance', "advertiser_id",
                    'freq_tensor_svd_alt_1' ,'freq_tensor_svd_alt_2', 'freq_tensor_svd_alt_3', 'freq_tensor_svd_alt_4' ,'freq_tensor_svd_alt_5' ,'time_tensor_svd_alt_1' ,
                    'time_tensor_svd_alt_2', 'time_tensor_svd_alt_3', 'time_tensor_svd_alt_4' ,'time_tensor_svd_alt_5' ,'dist_tensor_svd_alt_1' ,'dist_tensor_svd_alt_2',
                    'dist_tensor_svd_alt_3' ,'dist_tensor_svd_alt_4' ,'dist_tensor_svd_alt_5' ,'freq_user_tensor_svd_alt_1', 'freq_user_tensor_svd_alt_2' ,
                    'freq_user_tensor_svd_alt_3', 'freq_user_tensor_svd_alt_4' ,'freq_user_tensor_svd_alt_5' ,'dist_user_tensor_svd_alt_1' ,
                    'dist_user_tensor_svd_alt_2' ,
                    'dist_user_tensor_svd_alt_3' ,'dist_user_tensor_svd_alt_4', 'dist_user_tensor_svd_alt_5' ,
                    'time_user_tensor_svd_alt_1' ,'time_user_tensor_svd_alt_2' ,'time_user_tensor_svd_alt_3', 'time_user_tensor_svd_alt_4' ,
                    'time_user_tensor_svd_alt_5')
  
  sdata = fdata[, c('whole_week_avg_locs' ,'whole_week_avg_distance' ,'whole_week_avg_unique_locs', 'whole_n_rog' ,
                    'whole_n_entropy', 'whole_t_entropy' ,'whole_d_entropy', 'whole_mean_dwell', 'whole_avg_distance', "advertiser_id",
                    'freq_tensor_svd_alt_1' ,'freq_tensor_svd_alt_2', 'freq_tensor_svd_alt_3', 'freq_tensor_svd_alt_4' ,'freq_tensor_svd_alt_5' ,'time_tensor_svd_alt_1' ,
                    'time_tensor_svd_alt_2', 'time_tensor_svd_alt_3', 'time_tensor_svd_alt_4' ,'time_tensor_svd_alt_5' ,'dist_tensor_svd_alt_1' ,'dist_tensor_svd_alt_2',
                    'dist_tensor_svd_alt_3' ,'dist_tensor_svd_alt_4' ,'dist_tensor_svd_alt_5' ,'freq_user_tensor_svd_alt_1', 'freq_user_tensor_svd_alt_2' ,
                    'freq_user_tensor_svd_alt_3', 'freq_user_tensor_svd_alt_4' ,'freq_user_tensor_svd_alt_5' ,'dist_user_tensor_svd_alt_1' ,
                    'dist_user_tensor_svd_alt_2' ,
                    'dist_user_tensor_svd_alt_3' ,'dist_user_tensor_svd_alt_4', 'dist_user_tensor_svd_alt_5' ,
                    'time_user_tensor_svd_alt_1' ,'time_user_tensor_svd_alt_2' ,'time_user_tensor_svd_alt_3', 'time_user_tensor_svd_alt_4' ,
                    'time_user_tensor_svd_alt_5')]
  features = sdata[,-c("advertiser_id")]
  f.mat = as.matrix(features)
  f.mat = apply(f.mat, 2, scale)
  f.mat = replace(f.mat,is.na(f.mat),0)
  library(coop)
  sim.mat <- cosine(t(f.mat), use = "pairwise.complete.obs")
  mean(sim.mat)
  
  n_neighbs = c(50)
  
  unique_locs <- function(dt){
    freq = dt[,.N , by = c("cluster")]
    return(as.integer(freq$cluster))
  }
  
  k.nearest<- function (i, distance_matrix, k = 5) 
  {
    ordered_neighbors <- rev(grr::order2(distance_matrix[i,]))
    return(ordered_neighbors[1:(k)])
  }
  
  k.nearest.scores<- function (i, distance_matrix, k = 5) 
  {
    
    ordered_neighbors <- rev(grr::sort2(distance_matrix[i,]))
    return(ordered_neighbors[1:(k)])
  }
  freq_locs <- function(dt){
    freq = dt[,.N , by = c("cluster")]
    return(freq$N)
  }
  recs_sim_weighing <- function(dt, unique_locs, num_rec){
    locs = sapply(dt$neighbors, function(x){ unique_locs[x]})
    #freqs = sapply(dt$neighbors, function(x){ freq_locs[x]})
    weights = mapply(function(x , y) x*0 + y ,locs, unlist(dt$sims))
    loc.long.dt = data.table(locs = unlist(locs), weights = unlist(weights), key = "locs")
    loc.dt = loc.long.dt[,.(sim.rank = max(weights)),  by = "locs"]
    return(list(loc.dt[order(-loc.dt$sim.rank),][1:num_rec]$locs))
  }
  
  recs_freq_weighing <- function(dt, unique_locs, freq_locs, num_rec){
    locs = sapply(dt$neighbors, function(x){ unique_locs[x]})
    freqs = sapply(dt$neighbors, function(x){ freq_locs[x]})
    weights = mapply(function(x , y) x*0 + y ,locs, unlist(dt$sims))
    loc.long.dt = data.table(locs = unlist(locs), weights = unlist(weights), freqs = unlist(freqs), key = "locs")
    loc.dt = loc.long.dt[,.(freq.rank = freq_weighing(.SD[,c("weights","freqs")])),  by = "locs"]
    return(list(loc.dt[order(-loc.dt$freq.rank),][1:num_rec]$locs))
  }
  
  for (n in n_neighbs){
    print(paste0("Negihbors", n))
    neigh.mat <- lapply(1:nrow(f.mat), function(x) {k.nearest(x, sim.mat, n)})
    simscores.mat <- lapply(1:nrow(f.mat), function(x) {k.nearest.scores(x, sim.mat, n)})
    
    stdata = trsdata[, .(unique_locs = list(unique_locs(.SD[,c("cluster")])), 
                         freq_locs = list(freq_locs(.SD[,c("cluster")])))
                     , by = .(advertiser_id)]
    stdata = data.table(stdata, key = "advertiser_id")
    fdata = data.table(sdata, key = "advertiser_id")
    ftdata = stdata[fdata,]
    ftdata$neighbors = neigh.mat
    ftdata$sims = simscores.mat
    num_rec = 20
    #recom = ftdata[, .(recom_sim = recs_sim_weighing(.SD[,c("sims","neighbors")], ftdata$unique_locs, ftdata$freq_locs, num_rec),
    #                   recom_freq = recs_freq_weighing(.SD[,c("sims","neighbors")], ftdata$unique_locs, ftdata$freq_locs, num_rec)),
    #               by = c("advertiser_id")]
    recom = ftdata[, .(recom_sim = recs_sim_weighing(.SD[,c("sims","neighbors")], ftdata$unique_locs, num_rec)),
                   by = c("advertiser_id")]
    saveRDS(recom,paste0("BootStrap/",iteration,"_0.05_split_p_",percent,"_20recs_k2_random_",n,"_norm_neigh_wmtcuav.RDS"))
    #saveRDS(recom,paste0("0.05_split_notworking_20recs_",n,"_norm_neigh_wmtcuav.RDS"))
  }
  
