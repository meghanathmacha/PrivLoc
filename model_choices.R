library(e1071)
library(caret)
# Features All mobility features
features = whole_mob_features
features = append(features,"os.family")
train = sdata[, ..features ]
features

# default number of predictors is predictors/3 or about 6
rf.Grid <- expand.grid(mtry = seq(from = 3, to = length(features), by = 3))

set.seed(1)
rf.seeds <- vector(mode = "list", length = 11) # length is = (nresampling)+1
for(i in 1:10) rf.seeds[[i]]<- sample.int(n=1000, 6) # 6 is the number of tuning parameters (mtry possibilities)
rf.seeds[[11]] <- 1 # for the last model
remove(i)
rf.seeds

library(pROC)
rf.Control <- trainControl(method = "repeatedcv", # use N-fold cross validation
 number = 10, # the number of folds
 repeats = 2, summaryFunction = multiClassSummary,
 seeds = rf.seeds)

library(doParallel)
cl = makeCluster(10)
registerDoParallel(cl)
formula <- os.family ~ .
class.rf.wm <- train(formula, data=train ,
                  method='rf',
                  ntree=100,
                  importance=TRUE,
                  na.action=na.omit,
                  tuneGrid = rf.Grid,
                  trControl= rf.Control,
                  metric = 'Accuracy',
                  allowParallel=TRUE)
stopCluster(cl)
remove(cl)
registerDoSEQ()

remove(rf.Grid, rf.Control, rf.seeds)
results <- resamples(list("User Mobility"=class.rf.wm, "User Mobility + \\User-Location affinity"=class.rf.wmc, 
                          "Whole Mobility + \\User-Location & User-User affinity"=class.rf.wmtcu,
                          "Whole Mobility + \\User-Location Context"= class.rf.wemc))
#results$values

# summarize the distributions
summary(results)



### Background privacy score building ## 
sdata = data.table(readRDS("0.05_fsamplev4.RDS"))

library(data.table)
tsdata = data.table(sdata[sdata$week %in% c('35'),])
trsdata = data.table(sdata[!(sdata$week %in% c('35')),])
trsdata$hour = strftime(as.POSIXct(trsdata$captured_at, format = "%Y-%m-%dT%H:%M:%S"), format = "%H")
trsdata$hour = as.numeric(trsdata$hour)

ssdata = trsdata
total_locs = unique(ssdata$cluster)
# Summarize the locations, only collect unique locations per user.
stdata = ssdata[, .(unique_locs = list(unique(cluster)), num_locs = length(unique(cluster))) ,by = .(advertiser_id)]
# Background knowledge with k

calc_total_occurences <- function(x, unique_locs){
  return(sum(sapply(unique_locs, function(y){ all(x %in% y)})))
}

calc_occurences <- function(x, kcombs){
  return(as.integer(sapply(kcombs, function(y){ all(x  %in% y)})))
}

divide <- function(x, denom){
  return(sapply(x, "/", unlist(denom), simplify = FALSE))
}

maxi <-function(x){
  x = x[is.finite(x)]
  x = x[!is.nan(x)]
  return(max(x))
}

remove_locs <- function(stdata, p){
  temp.stdata = stdata
  # stdata will have the risks
  # randomly remove locations based on the risk scores.
  select_locs <- function(unique_locs, num_locs, risk){
    locs = unlist(unique_locs)
    percent = risk*p
    return(list(locs[!as.logical(rbinom(n = num_locs, prob = percent, size = 1))]))
  }
  temp.stdata[,new_unique_locs:=list(select_locs(unique_locs, num_locs , risk)), by = seq_len(nrow(temp.stdata))]
  temp.stdata[,new_num_locs:=length(unlist(new_unique_locs)), by = seq_len(nrow(temp.stdata))]
  return(temp.stdata)
}

remove_locs_raw <- function(risk_sdata, p){
  temp.risk.sdata = risk_sdata
  # temp.risk.sdata will have the risks
  # randomly remove locations based on the risk scores.
  select_locs <- function(unique_locs, num_locs, risk){
    locs = unlist(unique_locs)
    percent = risk*p
    return(list(locs[!as.logical(rbinom(n = num_locs, prob = percent, size = 1))]))
  }
  temp.stdata[,new_unique_locs:=list(select_locs(unique_locs, num_locs , risk)), by = seq_len(nrow(temp.stdata))]
  temp.stdata[,new_num_locs:=length(unlist(new_unique_locs)), by = seq_len(nrow(temp.stdata))]
  return(temp.stdata)
}

k = 2
library(utils)
kcombs = unique(stdata[stdata$num_locs == k,]$unique_locs)

denom = lapply(kcombs, calc_total_occurences, stdata$unique_locs)
numer = lapply(stdata$unique_locs, calc_occurences, kcombs) 
scores = sapply(numer, "/", unlist(denom), simplify = FALSE)
ind_scores = lapply(scores, maxi)
stdata$risk = unlist(ind_scores)
risk_stdata = stdata[,c("advertiser_id","risk")]
# Combine with the raw GPS data
risk_trsdata = risk_stdata[trsdata]
#Sample according to the risk scores
p = 0.8
for(i in 1:10){
risk_trsdata$indicator = !as.logical(rbinom(nrow(risk_trsdata), prob = risk_trsdata$risk*p, size = 1))
new_trsdata = risk_trsdata[risk_trsdata$indicator == TRUE,]
saveRDS(new_trsdata,paste0("BootStrap/",i,"_0.05_trs_sample_p_",p,"_k2.RDS"))
}
