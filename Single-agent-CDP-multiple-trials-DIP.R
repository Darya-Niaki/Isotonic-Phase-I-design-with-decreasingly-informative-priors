# This function generates a clean and visually organized summary of the results for DIP with a flat prior and DIP_mean with a non-flat prior.
# simcdp_DIP and simcdp_DIP2 contain the results from multiple trials within a single simulation run.

simcdp_DIP<-function(p0,target,ssize,n.stop,ntrial,cl,N,method){
  ndose=length(p0)
  
  a0<-b0<-d.select<-tox<-pts<-matrix(nrow=ntrial,ncol=ndose)
  samplesize<-rep(0,ntrial)
  nstop=0
  
  for(i in 1:ntrial){
    result<-cdp_DIP(p0,target,ssize,n.stop,cl,N,method)
    d.select[i,]=result$dose.select
    tox[i,]=result$tox.data
    pts[i,]=result$pt.allocation
    samplesize[i]=sum(result$pt.allocation)
    nstop=nstop+result$stop
    a0[i,] = result$a0
    b0[i,] = result$b0
  }
  cat("Simulation results for DIP with flat prior\n");
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
  cat("Safety stopping rule:\n");
  cat("Stop the trial if the Pr(DLT rate at the lowest study dose level > target | data) > ",cl,"\n");
} 

#------------------------------------------------------------------------------

# 
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
  cat("Simulation results for DIP with non-flat prior\n");
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
# simcdp_DIP(p0,target,ssize,n.stop,ntrial,cl,N=30,method="mean")
# simcdp_DIP2(p0,target,ssize,n.stop,ntrial,cl,N)



