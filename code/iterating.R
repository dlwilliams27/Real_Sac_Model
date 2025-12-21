#imports
library('hydromad')
library('zoo')
library('ggplot2')
library('rematch')
library('stringr')

#path for all forcing data
files <- list.files(path='Forcings', pattern = '\\.csv$', full.names=TRUE)
#global
empty_coef<- list()
empty_kge<- list()
#loop to execute modeling for forcing data
for (file in files) {
  patt<- "(T|B)\\d_(\\d+)_[a-z]+"
  match<- str_match(basename(file) , pattern=patt)
  gage_id <- match[,3]
  #pre-processing
  datasetL<- read.csv(file)
  print(colnames(dataset))
  names(dataset) <- c('Datetime','P','T','Q','PET')
  dataset$P[dataset$P < 0] <-NA
  dataset$Q[dataset$Q < 0] <-NA
  dataset$PET[dataset$PET < 0] <- NA
  #dataset <- subset(dataset, select = -c(NA))
  new_dataset=dataset[!duplicated(dataset$Datetime),]
  
  #switching to correct format
  P <- zoo(new_dataset$P ,as.Date(new_dataset$Datetime))
  E <- zoo(new_dataset$PET ,as.Date(new_dataset$Datetime))
  Q <- zoo(new_dataset$Q ,as.Date(new_dataset$Datetime))
  partial=merge(P,E, all=TRUE)
  complete=merge(partial, Q, all=TRUE)
  head(complete)
  range(time(complete))
  complete <- na.trim(complete)
  head(complete)
  
  #setting variable calibration dates
  date_length <- (diff(range(index(complete))))/2
  start <- start(complete)
  med <- start + date_length
  end <- end(complete)
  
  #Fitting model
  cal <- window(complete, start=start, end=med)
  val <- window(complete, start=med, end=end)
  Mod <- hydromad(cal, sma="sacramento", routing = "hbvrouting")
  Mod <- update(Mod, cal)
  
  #Calibration optimizing
  rmse <- hmadstat("RMSE")
  kge <- hmadstat("KGE")
  Fit <-fitByOptim(Mod, kge)
  
  #coefficient values and KGE
  coef<-coef(Fit)
  obj<-objFunVal(Fit)
  coef_df<- data.frame(coef)
  kge_df<-data.frame(obj)
  t(coef_df)
  colnames(kge_df)<- c("KGE")
  coef_df[,'Gage'] <- gage_id
  kge_df[,'Gage'] <-gage_id
  empty_coef[[length(empty_coef)+1]]<-coef_df
  empty_kge[[length(empty_kge)+1]]<-kge_df
}
#concatenate lists of dfs
full_kge<-Reduce(rbind, empty_kge)
full_coef<-Reduce(rbind, empty_coef)

#no path set, should go to active directory
write.csv(full_kge, "KGE_Data.csv", row.names=TRUE)
write.csv(full_coef, "Coef_Data.csv", row.names=TRUE)
