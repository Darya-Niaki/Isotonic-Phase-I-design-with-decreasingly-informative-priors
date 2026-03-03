# This is for the original DIP method that hyper parameters are based on the p0.

simcdp_DIP<-function(p0,target,ssize,n.stop,ntrial,cl){
  ndose=length(p0)
  
  a0<-b0<-d.select<-tox<-pts<-matrix(nrow=ntrial,ncol=ndose)
  samplesize<-rep(0,ntrial)
  nstop=0
  
  for(i in 1:ntrial){
    result<-cdp_DIP(p0,target,ssize,n.stop,cl)
    d.select[i,]=result$dose.select
    tox[i,]=result$tox.data
    pts[i,]=result$pt.allocation
    samplesize[i]=sum(result$pt.allocation)
    nstop=nstop+result$stop
    a0[i,] = result$a0
    b0[i,] = result$b0
  }
  cat("Simulation results for Conaway, Dunbar, Peddada (2004)\n");
  cat("method targeting a DLT rate of", target,"\n\n");
  cat("True DLT probability:\n")
  cat(round(p0,3),  sep="\t",  "\n")
  cat("MTD Selection percentage:\n")
  cat(formatC(colMeans(d.select)*100, digits=1, format="f"), sep="\t",  "\n")
  cat("Average number of DLTs:\n")
  cat(formatC(colMeans(tox), digits=1, format="f"), sep="\t",   "\n")
  cat("Average number of patients:\n")
  cat(formatC(colMeans(pts), digits=1, format="f"), sep="\t",   "\n")
  cat("Percent stopped for safety:\n");
  cat(round(nstop/ntrial*100,2), sep="\t",   "\n");
  cat("Average sample size:\n");
  cat(round(mean(samplesize),2), sep="\t",  "\n\n");
  cat ("Number of patients treated above the MTD:\n");
  cat(formatC(sum(colMeans(pts)[(which(p0==target,arr.ind = TRUE) + 1):length(p0)]), digits=1, format="f"), sep="\t",   "\n")
  cat("Design specifications: \n");
  cat("Prior distribution on DLT rate at each dose level:\n");
  cat("beta \n");
  cat(colMeans(a0), colMeans(b0), "\n");
  cat("Safety stopping rule:\n");
  cat("Stop the trial if the Pr(DLT rate at the lowest study dose level > target | data) > ",cl,"\n");
  
} 

#------------------------------------------------------------------------------

# This is for the new DIP method, where p0 is equal to the scenarios.
simcdp_DIP2<-function(p0,target,ssize,n.stop,ntrial,cl,N){
  ndose=length(p0)
  
  a0<-b0<-d.select<-tox<-pts<-matrix(nrow=ntrial,ncol=ndose)
  samplesize<-rep(0,ntrial)
  nstop=0
  
  for(i in 1:ntrial){
    result<-cdp_DIP2(p0,target,ssize,n.stop,cl,N)
    d.select[i,]=result$dose.select
    tox[i,]=result$tox.data
    pts[i,]=result$pt.allocation
    samplesize[i]=sum(result$pt.allocation)
    nstop=nstop+result$stop
    a0[i,] = result$a0
    b0[i,] = result$b0
  }
  cat("Simulation results for Conaway, Dunbar, Peddada (2004)\n");
  cat("method targeting a DLT rate of", target,"\n\n");
  cat("True DLT probability:\n")
  cat(round(p0,3),  sep="\t",  "\n")
  cat("MTD Selection percentage:\n")
  cat(formatC(colMeans(d.select)*100, digits=1, format="f"), sep="\t",  "\n")
  cat("Average number of DLTs:\n")
  cat(formatC(colMeans(tox), digits=1, format="f"), sep="\t",   "\n")
  cat("Average number of patients:\n")
  cat(formatC(colMeans(pts), digits=1, format="f"), sep="\t",   "\n")
  cat("Percent stopped for safety:\n");
  cat(round(nstop/ntrial*100,2), sep="\t",   "\n");
  cat("Average sample size:\n");
  cat(round(mean(samplesize),2), sep="\t",  "\n\n");
  cat ("Average number of patients treated above the MTD:\n");
  cat(formatC(sum(colMeans(pts)[(which(p0==target,arr.ind = TRUE) + 1):length(p0)]), digits=1, format="f"), sep="\t",   "\n")
  cat("Design specifications: \n");
  cat("Prior distribution on DLT rate at each dose level:\n");
  cat("beta \n");
  cat(colMeans(a0), colMeans(b0), "\n");
  cat("Safety stopping rule:\n");
  cat("Stop the trial if the Pr(DLT rate at the lowest study dose level > target | data) > ",cl,"\n");
  
}
# p0 = c(0.3,	0.36,	0.42,	0.45,	0.46)
# target = 0.25
# ssize = 30
# n.stop = 31
# cl = 0.95
# N = 20
# ntrial = 100
# simcdp_DIP2(p0,target,ssize,n.stop,ntrial,cl,N)


