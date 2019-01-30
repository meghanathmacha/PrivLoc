sdata = readRDS("0.05_fsamplev4.RDS")
sdata$hour = strftime(as.POSIXct(sdata$captured_at, format = "%Y-%m-%dT%H:%M:%S"),
                      format = "%H")
sdata$hour = as.numeric(sdata$hour)
sdata$home = rep(FALSE,nrow(sdata))
sdata[sdata$weekend == 0,]$home = sdata[sdata$weekend == 0,]$hour %in% c(20:23,0:10)
sdata[sdata$weekend == 1,]$home = sdata[sdata$weekend == 1,]$hour %in% c(22:23,0:16)


get_home <- function(dt)
{
  home_list = dt$cluster[dt$home]
  if(length(home_list) == 0){
    return(NA)
  }
  else{
    return(as.numeric(names(table(home_list))[1]))
  }
}

# Populate the home for each advertiser_id
sdata = data.table(sdata, key = "advertiser_id")
home_data = sdata[, .(home_cluster = list(get_home(.SD[,c("home","cluster")]))), 
                          by = .(advertiser_id)]
home_data$home_cluster = as.numeric(home_data$home_cluster)
sum(is.na(home_data$home_cluster))


# Assign cluster centroids for the clusters to make the regression
centroid_data = sdata[, .(mean_x = mean(unique(x)), mean_y = mean(unique(y))), 
                  by = .(cluster)] 

# Append the clusters to home_data

names(centroid_data)[1] = c("home_cluster")
centroid_data = data.table(centroid_data, key = "home_cluster")
home_data = data.table(home_data, key = "home_cluster")
home_cluster_data = centroid_data[home_data,]
home_cluster_data = data.table(home_cluster_data, key = "advertiser_id")

# Get the full feature set.
full_fdata = data.table(readRDS("0.05_features_v4.RDS"), key = "advertiser_id")

# Append the home clusters with centroids
full_fdata_home = home_cluster_data[full_fdata,]


#Regression for mean_x
features = c('whole_week_avg_locs' ,'whole_week_avg_distance' ,'whole_week_avg_unique_locs', 'whole_n_rog' ,
             'whole_n_entropy', 'whole_t_entropy' ,'whole_d_entropy', 'whole_mean_dwell', 'whole_avg_distance',
             'freq_tensor_svd_alt_1' ,'freq_tensor_svd_alt_2', 'freq_tensor_svd_alt_3', 'freq_tensor_svd_alt_4' ,'freq_tensor_svd_alt_5' ,'time_tensor_svd_alt_1' ,
             'time_tensor_svd_alt_2', 'time_tensor_svd_alt_3', 'time_tensor_svd_alt_4' ,'time_tensor_svd_alt_5' ,'dist_tensor_svd_alt_1' ,'dist_tensor_svd_alt_2',
             'dist_tensor_svd_alt_3' ,'dist_tensor_svd_alt_4' ,'dist_tensor_svd_alt_5' ,'freq_user_tensor_svd_alt_1', 'freq_user_tensor_svd_alt_2' ,
             'freq_user_tensor_svd_alt_3', 'freq_user_tensor_svd_alt_4' ,'freq_user_tensor_svd_alt_5' ,'dist_user_tensor_svd_alt_1' ,
             'dist_user_tensor_svd_alt_2' ,
             'dist_user_tensor_svd_alt_3' ,'dist_user_tensor_svd_alt_4', 'dist_user_tensor_svd_alt_5' ,
             'time_user_tensor_svd_alt_1' ,'time_user_tensor_svd_alt_2' ,'time_user_tensor_svd_alt_3', 'time_user_tensor_svd_alt_4' ,
             'time_user_tensor_svd_alt_5')

features = append(features,"mean_x")

train = subset(full_fdata_home, select = features)
train = train[!is.na(train$mean_x),]
train[is.na(train)] <- 0

rf.Grid <- expand.grid(mtry = seq(from = 3, to = length(features), by = 6))

set.seed(1)
rf.seeds <- vector(mode = "list", length = 11) # length is = (nresampling)+1
for(i in 1:10) rf.seeds[[i]]<- sample.int(n=1000, 15) # 6 is the number of tuning parameters (mtry possibilities)
rf.seeds[[11]] <- 1 # for the last model
remove(i)
rf.seeds

library(pROC)
library(caret)
rf.Control <- trainControl(method = "repeatedcv", # use N-fold cross validation
                           number = 5, # the number of folds
                           repeats = 2, summaryFunction = defaultSummary,
                           seeds = rf.seeds)

library(doParallel)
cl = makeCluster(8)
registerDoParallel(cl)
formula <- mean_x ~ .
rf.mean_x <- train(formula, data=train ,
                   method='rf',
                   ntree=100,
                   importance=TRUE,
                   na.action=na.omit,
                   tuneGrid = rf.Grid,
                   trControl= rf.Control,
                   metric = 'RMSE',
                   allowParallel=TRUE)
stopCluster(cl)
remove(cl)
registerDoSEQ()

remove(rf.Grid, rf.Control, rf.seeds)


#Regression for mean_y
features = c('whole_week_avg_locs' ,'whole_week_avg_distance' ,'whole_week_avg_unique_locs', 'whole_n_rog' ,
             'whole_n_entropy', 'whole_t_entropy' ,'whole_d_entropy', 'whole_mean_dwell', 'whole_avg_distance',
             'freq_tensor_svd_alt_1' ,'freq_tensor_svd_alt_2', 'freq_tensor_svd_alt_3', 'freq_tensor_svd_alt_4' ,'freq_tensor_svd_alt_5' ,'time_tensor_svd_alt_1' ,
             'time_tensor_svd_alt_2', 'time_tensor_svd_alt_3', 'time_tensor_svd_alt_4' ,'time_tensor_svd_alt_5' ,'dist_tensor_svd_alt_1' ,'dist_tensor_svd_alt_2',
             'dist_tensor_svd_alt_3' ,'dist_tensor_svd_alt_4' ,'dist_tensor_svd_alt_5' ,'freq_user_tensor_svd_alt_1', 'freq_user_tensor_svd_alt_2' ,
             'freq_user_tensor_svd_alt_3', 'freq_user_tensor_svd_alt_4' ,'freq_user_tensor_svd_alt_5' ,'dist_user_tensor_svd_alt_1' ,
             'dist_user_tensor_svd_alt_2' ,
             'dist_user_tensor_svd_alt_3' ,'dist_user_tensor_svd_alt_4', 'dist_user_tensor_svd_alt_5' ,
             'time_user_tensor_svd_alt_1' ,'time_user_tensor_svd_alt_2' ,'time_user_tensor_svd_alt_3', 'time_user_tensor_svd_alt_4' ,
             'time_user_tensor_svd_alt_5')

features = append(features,"mean_y")

train = subset(full_fdata_home, select = features)
train = train[!is.na(train$mean_y),]
train[is.na(train)] <- 0

rf.Grid <- expand.grid(mtry = seq(from = 3, to = length(features), by = 6))

set.seed(1)
rf.seeds <- vector(mode = "list", length = 11) # length is = (nresampling)+1
for(i in 1:10) rf.seeds[[i]]<- sample.int(n=1000, 15) # 6 is the number of tuning parameters (mtry possibilities)
rf.seeds[[11]] <- 1 # for the last model
remove(i)
rf.seeds

library(pROC)
library(caret)
rf.Control <- trainControl(method = "repeatedcv", # use N-fold cross validation
                           number = 5, # the number of folds
                           repeats = 2, summaryFunction = defaultSummary,
                           seeds = rf.seeds)

library(doParallel)
cl = makeCluster(8)
registerDoParallel(cl)
formula <- mean_y ~ .
rf.mean_y <- train(formula, data=train ,
                   method='rf',
                   ntree=100,
                   importance=TRUE,
                   na.action=na.omit,
                   tuneGrid = rf.Grid,
                   trControl= rf.Control,
                   metric = 'RMSE',
                   allowParallel=TRUE)
stopCluster(cl)
remove(cl)
registerDoSEQ()

remove(rf.Grid, rf.Control, rf.seeds)


## Make predictions x
features = c('whole_week_avg_locs' ,'whole_week_avg_distance' ,'whole_week_avg_unique_locs', 'whole_n_rog' ,
             'whole_n_entropy', 'whole_t_entropy' ,'whole_d_entropy', 'whole_mean_dwell', 'whole_avg_distance',
             'freq_tensor_svd_alt_1' ,'freq_tensor_svd_alt_2', 'freq_tensor_svd_alt_3', 'freq_tensor_svd_alt_4' ,'freq_tensor_svd_alt_5' ,'time_tensor_svd_alt_1' ,
             'time_tensor_svd_alt_2', 'time_tensor_svd_alt_3', 'time_tensor_svd_alt_4' ,'time_tensor_svd_alt_5' ,'dist_tensor_svd_alt_1' ,'dist_tensor_svd_alt_2',
             'dist_tensor_svd_alt_3' ,'dist_tensor_svd_alt_4' ,'dist_tensor_svd_alt_5' ,'freq_user_tensor_svd_alt_1', 'freq_user_tensor_svd_alt_2' ,
             'freq_user_tensor_svd_alt_3', 'freq_user_tensor_svd_alt_4' ,'freq_user_tensor_svd_alt_5' ,'dist_user_tensor_svd_alt_1' ,
             'dist_user_tensor_svd_alt_2' ,
             'dist_user_tensor_svd_alt_3' ,'dist_user_tensor_svd_alt_4', 'dist_user_tensor_svd_alt_5' ,
             'time_user_tensor_svd_alt_1' ,'time_user_tensor_svd_alt_2' ,'time_user_tensor_svd_alt_3', 'time_user_tensor_svd_alt_4' ,
             'time_user_tensor_svd_alt_5',"advertiser_id")

features = append(features,"mean_y")
features = append(features,"mean_x")

train = subset(full_fdata_home, select = features)
train = train[!is.na(train$mean_y),]
train[is.na(train)] <- 0

preds_x = predict(rf.mean_x, newdata = train)
preds_y = predict(rf.mean_y, newdata = train)
risks = cbind(train,preds_x,preds_y)
# Compute Root mean square error
risks$risk_home = -1*abs((risks$mean_y - risks$preds_y) + (risks$mean_x - risks$preds_x))
#Normalize the risks between 0 and 1
risks$risk_home = (risks$risk_home - min(risks$risk_home))/(max(risks$risk_home) - min(risks$risk_home))
#Combine with the full_fdata
subset_risk = risks[,c("advertiser_id","risk_home","mean_x","mean_y")]
full_fdata_home = subset_risk[full_fdata,]
full_fdata_home[is.na(full_fdata_home),] = 0

# Combine with raw GPS data
sdata = data.table(readRDS("0.05_fsamplev4.RDS"))

trsdata = data.table(sdata[!(sdata$week %in% c('35')),])
full_fdata_home = data.table(readRDS("0.05_features_v5.RDS"))
risk_home_data = full_fdata_home[,c("advertiser_id","risk_home")]
risk_trsdata = risk_home_data[trsdata,]
#Sample according to the risk scores
per = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
i = 1
for(p in per){
  risk_trsdata$indicator = !as.logical(rbinom(nrow(risk_trsdata), prob = risk_trsdata$risk_home*p, size = 1))
  new_trsdata = risk_trsdata[risk_trsdata$indicator == TRUE,]
  saveRDS(new_trsdata,paste0("BootStrap/",i,"_0.05_trs_sample_p_",p,"_home_meanbase.RDS"))
}
## Baseline - Assign mean of the risks to each user as the risk score


## Baseline - Global suppression of trajectories.
traj_max_data = risk_trsdata[, .(max_risk_home = mean(risk_home)), by = "cluster"]
risk_trsdata = data.table(risk_trsdata,key = "cluster")
traj_max_data = data.table(traj_max_data, key = "cluster")
risk_trsdata = traj_max_data[risk_trsdata,]
per = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
i = 2
for(p in per){
  risk_trsdata$indicator = !as.logical(rbinom(nrow(risk_trsdata), prob = risk_trsdata$max_risk_home*p, size = 1))
  new_trsdata = risk_trsdata[risk_trsdata$indicator == TRUE,]
  saveRDS(new_trsdata,paste0("BootStrap/",i,"_0.05_trs_sample_p_",p,"_home_global.RDS"))
}

## Baseline - Random assignment of risk scores
risk_home_data$risk_home_random = sample(risk_home_data$risk_home)
risk_trsdata = risk_home_data[trsdata,]
per = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
i = 3
for(p in per){
  risk_trsdata$indicator = !as.logical(rbinom(nrow(risk_trsdata), prob = risk_trsdata$risk_home_random*p, size = 1))
  new_trsdata = risk_trsdata[risk_trsdata$indicator == TRUE,]
  saveRDS(new_trsdata,paste0("BootStrap/",i,"_0.05_trs_sample_p_",p,"_home_random.RDS"))
}


########### 
class.rf = readRDS("risk_classifier_os.RDS")
tdata = readRDS("0.05_features_v5.RDS")
ggplot(tdata, aes(riskos)) +  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.01, alpha=0.5,
                                             lwd = 2)+
  geom_density(alpha=.4, fill="#FF6666") +  theme_classic() +
  labs(x = "User risk", y = "Density") +
theme(text = element_text(size=60), plot.title = element_text(hjust = 0.5, vjust=-0.5)) 
