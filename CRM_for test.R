# This for CRM
library(dfcrm)
# Target 0.3
# Sceanrios used for paper
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

nsim <- 10000   #number of simulated trials
target <- 0.30	#target DLT rate
n.stop <- 31	#number of pts to accrue on one dose level to stop
n <- 30		#total sample size
cl <- 0.95		#probability cutoff that defines the safety stopping rule

result = crmsim(r1, r1, target, n, x0 =1, nsim = 1, mcohort = 1, restrict = TRUE,
       count = TRUE, method = "bayes", model = "empiric", intcpt = 3,
       scale = sqrt(1.34), seed = 1009)


