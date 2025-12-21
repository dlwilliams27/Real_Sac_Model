#imports
library('hydromad')
library('zoo')
library('ggplot2')
# 
#reading in file
 #top
dataset <-read.csv("C:/Users/Delanie Williams/OneDrive - The University of Alabama/Research/Thesis Materials/R_modeling/Real_Sac_Model/Forcings/B2_1112862_forcing.csv")
#bottom
#dataset <- read.csv("B1_1076281_forcing.csv")
 
#pre-processing
names(dataset) <- c('NA1','Datetime','P','T','Q','PET')
dataset$P[dataset$P < 0] <-NA
dataset$Q[dataset$Q < 0] <-NA
dataset$PET[dataset$PET < 0] <- NA
dataset <- subset(dataset, select = -c(NA1))
new_dataset=dataset[!duplicated(dataset$Datetime),]
print(any(duplicated(index(new_dataset))))
 
#switching to correct format
P <- zoo(new_dataset$P , as.Date(new_dataset$Datetime))
E <- zoo(new_dataset$PET ,as.Date(new_dataset$Datetime))
Q <- zoo(new_dataset$Q ,as.Date(new_dataset$Datetime))

#setting variable calibration dates
date_length <- (diff(range(index(P))))/2
start <- start(P)
med <- start + date_length
end <- end(P)

partial=merge(P,E, all=TRUE)
complete=merge(partial, Q, all=TRUE)
head(complete)
range(time(complete))
complete <- na.trim(complete)
head(complete)

#Fitting model
cal <- window(complete, start="1996-01-01", end ="1998-12-31")
val <- window(complete, start="2002-01-01", end="2007-12-31")
Mod <- hydromad(cal, sma="sacramento", routing = "hbvrouting")
Mod <- update(Mod, cal)
print(Mod)
#other optimization methods take a longggggg time
rmse <- hmadstat("RMSE")
kge <- hmadstat("KGE")
Fit <-fitBySCE(Mod, kge)

#Returned Data and stats
results<-summary(Fit)
write.csv(results, "trial_SAC", row.names=FALSE)
print(summary(Fit))

obs <- observed(Fit)
sim <- fitted(Fit)
start<-"2002-01-01"
med<-"2003-01-01"
end<- "2007-01-01"
winobs <-window(obs, start, end)
winsim <- window(sim, med, end)

print(rmse)
#Plots
xlim=c(start, med)
plot(winobs, xlim, type='l', col="blue", xlab = "Time (days)", ylab = "Observed Q (mm/day)")
par(new=TRUE)
plot(winsim,axes = FALSE, type = 'l', col = 'red', xlab= "", ylab="")
axis(4)
mtext("Simulated Q", side = 4, line= 2.5)

plot(obs, sim, xlim=c(0,10), ylim=c(0,10), xlab="Observed Q", ylab="Simulated Q")
xyplot(optimtrace(Fit), type='b', xlab = "Function Count", ylab="objective function value")
xyplot(Fit, ylim=c(0,10), ylab='Q mm/day')

plot(Q)
plot(P)

#checking water balance
yearP <- window(P, start=as.Date("1996-01-01"), end=as.Date("1997-01-01"))
yearQ <- window(Q, start="1996-01-01", end= "1997-01-01")
yearE <- window(E, start="1996-01-01", end= "1997-01-01")
print(range(yearE))
sumP <- sum(yearP)
sumQ <- sum(yearQ)
sumE <- sum(yearE)
out <-sum(yearQ,yearE)
ratio <- out/sumP
dif <- (sumP - out)
print(paste("Yearly PET:", sumE))
print(paste("Yearly P:", sumP))
print(paste("Yearly Q", sumQ))
print(paste("Outflow:",out))
print(paste("Inflow:", sumP))
print(paste("Mass Balance Ratio:",ratio))
print(paste("Change in storage:", dif))

write.table(results, file=" ", append=False)


coef<-coef(Fit)
obj<-objFunVal(Fit)
print(obj)

coef_df<- data.frame(coef)
kge_df<-data.frame(obj)
t(df)
print(kge_df)
colnames(kge_df)<- c("KGE")
rownames(coef_df)<- c("Insert_gage_here")
rownames(coef_df) <-c("insert_gage_here",)
