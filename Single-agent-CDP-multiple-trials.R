

simcdp<-function(p0,target,a0,b0,ssize,n.stop,ntrial,cl){
    ndose=length(p0)
   
    d.select<-tox<-pts<-matrix(nrow=ntrial,ncol=ndose)
    samplesize<-rep(0,ntrial)
    nstop=0
    
    for(i in 1:ntrial){
      result<-cdp(p0,target,a0,b0,ssize,n.stop,cl)
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
	cat("Design specifications: \n");
    	cat("Prior distribution on DLT rate at each dose level:\n");
    	cat("Beta(",round(a0,1),",",round(b0,1),")","\n");
    	cat("Safety stopping rule:\n");
    	cat("Stop the trial if the Pr(DLT rate at the lowest study dose level > target | data) > ",cl,"\n");

	results_data = data.frame(rbind(p0,colMeans(d.select)*100,colMeans(tox),
	                                colMeans(pts),round(nstop/ntrial*100,2),
	                                round(mean(samplesize),2)))
	
	rownames(results_data) = c("scenario","MTD Selection percentage",
	                           "Average number of DLTs",
	                           "Average number of patients",
	                           "Percent stopped for safety",
	                           "Average sample size"
	                         
	)
	return(results_data)    

} 

#simcdp(p0,target,a0 = 2.07,b0 = 4.83,ssize,n.stop,10000,cl)
