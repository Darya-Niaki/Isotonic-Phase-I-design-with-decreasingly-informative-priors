#install.packages("Iso")
library(Iso)
    
dir<-"path"  #specify directory containing Single-agent-CDP-one-trial and Single-agent-CDP-multiple-trials.R
source(paste(dir,"/Single-agent-CDP-one-trial.R",sep="")) #code to run one simulated trial
source(paste(dir,"/Single-agent-CDP-multiple-trials.R",sep="")) #code to run multiple simulated trials

####################################################################
####################################################################
#### This part is for target DLT of 0.2#############################
####################################################################


#True DLT probability scenarios
r1 <- c(0.20,0.26,0.40,0.45,0.46)
r2 <- c(0.20,0.29,0.35,0.50,0.58)
r3 <- c(0.10,0.20,0.25,0.35,0.40)
r4 <- c(0.08,0.20,0.30,0.45,0.65)
r5 <- c(0.04,0.06,0.20,0.32,0.50)
r6 <- c(0.01,0.10,0.20,0.26,0.35)
r7 <- c(0.05,0.06,0.07,0.20,0.31)
r8 <- c(0.02,0.04,0.10,0.20,0.25)
r9 <- c(0.01,0.02,0.07,0.08,0.20)
r10 <- c(0.01,0.02,0.03,0.04,0.20)



#calculate prior
x<-2*target
mu<-target
ui<-0.95
f<-function(b){
	pbeta(x,mu*b/(1-mu),b)-ui
}
b0<-uniroot(f,c(0.0001,100))$root
a0<-mu*b0/(1-mu)

paper_scenarios_0.2 = rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)
write.csv(paper_scenarios_0.2,"/sample_0.2.csv") # path to save the True DLT probabilities 

ntrial <- 10000    #number of simulated trials
target <- 0.20	#target DLT rate
n.stop <- 31	#number of pts to accrue on one dose level to stop
ssize <- 30 	#total sample size
cl <- 0.95		#probability cutoff that defines the safety stopping rule

set.seed(1377)
m = matrix(0,4,10)
df = data.frame(m,row.names = c("PCS","patient over dosed","safety stopping","total sample"))#for evaluation results 


for(i in 1:nrow(paper_scenarios_0.2)){
  results = simcdp(p0 =paper_scenarios_0.2[i,],target,a0 = 2.595,b0 = 10.381,ssize,n.stop,ntrial,cl)
  MTD = which(results[1,] == 0.2)#which dose is the MTD
  pcs = results[2,MTD]
  overdose_patient = sum(results[4,-(1:MTD)])
  safety = results[5,1]#all the values are equal
  total_sample = results[6,1]
  df[,i] = c(pcs,overdose_patient,safety,total_sample)
  add = paste0("path/target_0.2/CDP/",i,".csv") #path to save the results of simulations
  write.csv(results,add)
}

ad = paste0("path/CDP/evaluation.csv") # This summary is used to create figures
write.csv(df,ad)

####################################################################
####################################################################
#### This part is for target DLT of 0.3#############################
####################################################################


###target 30%
target<-0.3
r1<-c(0.3,	0.36,	0.42,	0.45,	0.46)
r2<-c(0.3,	0.4,	0.55,	0.6,	0.7)
r3<-c(0.08,	0.3,	0.38,	0.42,	0.52)
r4<-c(0.13, 0.3,	0.42, 0.5,	0.8)
r5<-c(0.04,	0.07,	0.3,	0.35,	0.42)
r6<-c(0.01,	0.12,	0.3,	0.41,	0.55)
r7<-c(0.06,	0.07,	0.12,	0.3,	0.4)
r8<-c(0.02,	0.05,	0.16,	0.3,	0.36)
r9<-c(0.01,	0.02,	0.04,	0.06,	0.3)
r10<-c(0.06, 0.07, 0.08, 0.12, 0.3)

ntrial <- 10000    #number of simulated trials
target <- 0.30	#target DLT rate
n.stop <- 31	#number of pts to accrue on one dose level to stop
ssize <- 30		#total sample size
cl <- 0.95		#probability cutoff that defines the safety stopping rule

#calculate prior
x<-2*target
mu<-target
ui<-0.95
f<-function(b){
	pbeta(x,mu*b/(1-mu),b)-ui
}
b0<-uniroot(f,c(0.0001,100))$root
a0<-mu*b0/(1-mu)

p0<-r2
set.seed(1377)
simcdp(p0=r11,target,a0,b0,ssize,n.stop,ntrial,cl)


paper_scenarios_0.3 = rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)


ntrial <- 10000    #number of simulated trials
target <- 0.30	#target DLT rate
n.stop <- 12	#number of pts to accrue on one dose level to stop
ssize <- 56		#total sample size
cl <- 0.999		#probability cutoff that defines the safety stopping rule


set.seed(1377)
m = matrix(0,4,10)
df = data.frame(m,row.names = c("PCS","patient over dosed","safety stopping","total sample"))#for evaluation results 


for(i in 1:nrow(paper_scenarios_0.3)){
  results = simcdp(p0 =paper_scenarios_0.3[i,],target,a0 = 2.07,b0 = 4.83,ssize,n.stop,ntrial,cl)
  MTD = which(results[1,] == 0.3)#which dose is the MTD
  pcs = results[2,MTD]
  overdose_patient = sum(results[4,-(1:MTD)])
  safety = results[5,1]#all the values are equal
  total_sample = results[6,1]
  df[,i] = c(pcs,overdose_patient,safety,total_sample)
  add = paste0("path/target_0.3/N30",i,".csv")
  write.csv(results,add)
}
ad = paste0("path/target_0.3/N30_evaluation.csv")
write.csv(df,ad)


####################################################################
####################################################################
#### This part is for target DLT of 0.25#############################
####################################################################


###target 25%
target<-0.25
s1 <- c(0.25, 0.31, 0.38, 0.44, 0.5)
s2 <- c(0.25, 0.37, 0.49, 0.59, 0.69)
s3 <- c(0.2, 0.25, 0.3, 0.35, 0.41)
s4 <- c(0.15, 0.25, 0.37, 0.49, 0.59)
s5 <- c(0.16, 0.2, 0.25, 0.3, 0.35)
s6 <- c(0.07, 0.15, 0.25, 0.37, 0.49)
s7 <- c(0.12, 0.16, 0.2, 0.25, 0.3)
s8 <- c(0.03, 0.07, 0.15, 0.25, 0.37)
s9 <- c(0.08, 0.12, 0.16, 0.2, 0.25)
s10 <- c(0.01, 0.03, 0.07, 0.15, 0.25)

ntrial <- 10000    #number of simulated trials
target <- 0.25	#target DLT rate
n.stop <- 31	#number of pts to accrue on one dose level to stop
ssize <- 30		#total sample size
cl <- 0.95		#probability cutoff that defines the safety stopping rule

#calculate prior
x<-2*target
mu<-target
ui<-0.95
f<-function(b){
  pbeta(x,mu*b/(1-mu),b)-ui
}
b0<-uniroot(f,c(0.0001,100))$root
a0<-mu*b0/(1-mu)

paper_scenarios_0.25 = rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)

ntrial <- 10000    #number of simulated trials
target <- 0.25	#target DLT rate
n.stop <- 31	#number of pts to accrue on one dose level to stop
ssize <- 30		#total sample size
cl <- 0.95		#probability cutoff that defines the safety stopping rule


set.seed(1377)
m = matrix(0,4,10)
df = data.frame(m,row.names = c("PCS","patient over dosed","safety stopping","total sample"))#for evaluation results 


for(i in 1:nrow(paper_scenarios_0.25)){
  results = simcdp(p0 =paper_scenarios_0.25[i,],target,a0 = 2.34 ,b0 = 7.02,ssize,n.stop,ntrial,cl)
  MTD = which(results[1,] == 0.25)#which dose is the MTD
  pcs = results[2,MTD]
  overdose_patient = sum(results[4,-(1:MTD)])
  safety = results[5,1]#all the values are equal
  total_sample = results[6,1]
  df[,i] = c(pcs,overdose_patient,safety,total_sample)
  add = paste0("path/target_0.25/CDP/",i,".csv")
  write.csv(results,add)
}
ad = paste0("path/target_0.25/CDP/eval.csv")
write.csv(df,ad)









