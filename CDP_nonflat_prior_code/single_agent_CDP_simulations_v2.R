#install.packages("Iso")
library(Iso)

dir<-"C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code_final results/CDP_nonflat_prior"  #specify directory containing following code files
source(paste(dir,"/Single_agent_CDP_one_trial_v2.R",sep="")) #code to run one simulated trial
source(paste(dir,"/Single_agent_CDP_multiple_trial_v2.R",sep="")) #code to run multiple simulated trials

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



paper_scenarios_0.2 = rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)

set.seed(1377)
ntrial <- 10000    #number of simulated trials
target <- 0.20	#target DLT rate
n.stop <- 31	#number of pts to accrue on one dose level to stop
ssize <- 30 	#total sample size
cl <- 0.95		#probability cutoff that defines the safety stopping rule

m = matrix(0,2,10)
df = data.frame(m,row.names = c("PCS","patient over dosed"))#for evaluation results 
summary_list <- list()

for(i in 1:nrow(paper_scenarios_0.2)){
  results = simcdp2(p0 =paper_scenarios_0.2[i,],target,ssize,n.stop,ntrial,cl)
  MTD = which(results[1,] == 0.2) # which dose is the MTD
  pcs = results[2,MTD]
  overdose_patient = results[4,1]
  df[,i] = c(pcs,overdose_patient)# This is for the plots
  summary_list[[i]] <- results
}

all_summary = do.call("rbind", summary_list)
write.csv(all_summary,"C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code_final results/final_results_02172026/CDP_2/target_0.2/CDP2_0.2_allscenarios.csv")



# This part is very helpful to create the plots
write.csv(df,"C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code_final results/final_results_02172026/CDP_2/target_0.2/CDP2_0.2_overall.csv")


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

paper_scenarios_0.3 = rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)


set.seed(1377)
ntrial <- 10000    #number of simulated trials
target <- 0.30	#target DLT rate
n.stop <- 31	#number of pts to accrue on one dose level to stop
ssize <- 30 	#total sample size
cl <- 0.95		#probability cutoff that defines the safety stopping rule

m = matrix(0,2,10)
df = data.frame(m,row.names = c("PCS","patient over dosed"))#for evaluation results 
summary_list <- list()

for(i in 1:nrow(paper_scenarios_0.3)){
  results = simcdp2(p0 =paper_scenarios_0.3[i,],target,ssize,n.stop,ntrial,cl)
  MTD = which(results[1,] == 0.3) # which dose is the MTD
  pcs = results[2,MTD]
  overdose_patient = results[4,1]
  df[,i] = c(pcs,overdose_patient)# This is for the plots
  summary_list[[i]] <- results
}

all_summary = do.call("rbind", summary_list)
write.csv(all_summary,"C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code_final results/final_results_02172026/CDP_2/target_0.3/CDP2_0.3_allscenarios.csv")



# This part is very helpful to create the plots
write.csv(df,"C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code_final results/final_results_02172026/CDP_2/target_0.3/CDP2_0.3_overall.csv")



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
paper_scenarios_0.25 = rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)


ntrial <- 10000    #number of simulated trials
target <- 0.25	#target DLT rate
n.stop <- 31	#number of pts to accrue on one dose level to stop
ssize <- 30		#total sample size
cl <- 0.95		#probability cutoff that defines the safety stopping rule


set.seed(1377)
m = matrix(0,2,10)
df = data.frame(m,row.names = c("PCS","patient over dosed"))#for evaluation results 
summary_list <- list()

for(i in 1:nrow(paper_scenarios_0.25)){
  results = simcdp2(p0 =paper_scenarios_0.25[i,],target,ssize,n.stop,ntrial,cl)
  MTD = which(results[1,] == 0.25) # which dose is the MTD
  pcs = results[2,MTD]
  overdose_patient = results[4,1]
  df[,i] = c(pcs,overdose_patient)# This is for the plots
  summary_list[[i]] <- results
}

all_summary = do.call("rbind", summary_list)
write.csv(all_summary,"C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code_final results/final_results_02172026/CDP_2/target_0.25/CDP2_0.25_allscenarios.csv")



# This part is very helpful to create the plots
write.csv(df,"C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code_final results/final_results_02172026/CDP_2/target_0.25/CDP2_0.25_overall.csv")





