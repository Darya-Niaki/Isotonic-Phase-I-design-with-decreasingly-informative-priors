

simcdp2<-function(p0,target,ssize,n.stop,ntrial,cl){
  ndose=length(p0)
  
  d.select<-tox<-pts<-matrix(nrow=ntrial,ncol=ndose)
  samplesize<-rep(0,ntrial)
  nstop=0
  
  for(i in 1:ntrial){
    result<-cdp2(p0,target,ssize,n.stop,cl)
    d.select[i,]=result$dose.select
    tox[i,]=result$tox.data
    pts[i,]=result$pt.allocation
    samplesize[i]=sum(result$pt.allocation)
    nstop=nstop+result$stop
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
  cat("Safety stopping rule:\n");
  cat("Stop the trial if the Pr(DLT rate at the lowest study dose level > target | data) > ",cl,"\n");
  
  results_data = data.frame(rbind(p0,
                                  colMeans(d.select)*100,
                                  colMeans(pts),
                                  sum(colMeans(pts)[(which(p0==target,arr.ind = TRUE) + 1):length(p0)]),
                                  round(nstop/ntrial*100,2),
                                  colMeans(tox),
                                  round(mean(samplesize))
  ))
  
  rownames(results_data) = c("scenario","MTD Selection percentage",
                             "Average number of patients",
                             "Average number of patients treated above the MTD",
                             "Percent stopped for safety",
                             "Average number of DLTs",
                             "Average sample size")
  return(results_data)    
  
} 

#simcdp2(p0,target,ssize,n.stop,ntrial,cl)
