# This is very similar to the multiple trials, it just saves the data in a datframe format with no pasting the resutls. 

simcdp_DIP_csv<-function(p0,target,ssize,n.stop,ntrial,cl){
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
  results_data = data.frame(rbind(p0,colMeans(d.select)*100,colMeans(tox),
                            colMeans(pts),round(nstop/ntrial*100,2),
                            round(mean(samplesize)),
                            colMeans(a0),colMeans(b0)))
  
  rownames(results_data) = c("scenario","MTD Selection percentage",
                             "Average number of DLTs",
                             "Average number of patients",
                             "Percent stopped for safety",
                             "Average sample size",
                             "a0", "b0"
                             )
  return(results_data)
  
} 
set.seed(1377)
#simcdp_DIP_csv(p0,target,ssize,n.stop,10000,cl)
#--------------------------------------------------------

# This is for the new DIP method, where p0 is equal to the scenarios.
simcdp_DIP_csv_2<-function(p0,target,ssize,n.stop,ntrial,cl,N){
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
  }
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
                             "Average sample size"
                             
                             
  )
  return(results_data)
  
}
# p0 = c(0.3,	0.36,	0.42,	0.45,	0.46)
# target = 0.25
# ssize = 30
# n.stop = 31
# cl = 0.95
# N = 20
# ntrial = 100
# simcdp_DIP_csv_2(p0,target,ssize,n.stop,ntrial,cl,N)
