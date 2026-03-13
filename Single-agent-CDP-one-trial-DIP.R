# p0: Vector of true DLT probabilities for each dose level.
# target: Target DLT rate (θ), representing the desired toxicity level.
# ssize: Total sample size for the trial.
# n.stop: Early stopping rule; the trial stops if the number of patients treated at a single dose level exceeds n.stop.
# cl: Safety stopping cutoff used to terminate the trial when excessive toxicity is observed.
# method: Specifies the DIP approach used to estimate toxicity probabilities. Options are "mean" or "mode".
# N: Maximum effective sample size (ESS) for the prior distribution.


cdp_DIP<-function(p0,target,ssize,n.stop,cl = 0.95,method,N){
    ndose = length(p0);   #number of dose levels
  y=n=dose.select=numeric(ndose);  #number of DLTs at each dose level
    stop=0; #indicate if trial stops early
    curr = 1
    i=1
    while(i <= ssize){
      
      a0 = 1 + target*(N - n)
      
      b0 =1 + (1-target)*(N - n)
      
      a0 = ifelse((N - n) <= 0,1,a0)
      b0 = ifelse((N - n) <= 0, 1,b0)
      
      y[curr] = y[curr] + rbinom(1,1,p0[curr]);
    
      n[curr] = n[curr] + 1;
      tried=which(n>0)
    
      safety=ifelse(n[1]>1,1 - pbeta(target, y[1] + a0, n[1] - y[1] + b0),0)
    
      if(safety>cl){ #check to see if lowest dose level is too toxic
        stop=1
        break
      }
      if(method=="mode"){
          u=(y[tried]+a0[tried] -1 )/(n[tried]+a0[tried]+b0[tried] -2)
      }else if(method=="mode"){
          u=(y[tried]+a0[tried])/(n[tried]+a0[tried]+b0[tried])
      }
      pipost=pava(u,w=n)
      lossvec=abs(pipost-target)  
      T=lossvec==min(lossvec)
      poss=which(T)
      if(sum(T)==1){
        sugglev=poss
      } else {
        if(all(pipost[poss]>target)){
          sugglev=min(poss)
        } else {
          sugglev=max(poss)
        }
      }
      if(length(tried)<ndose){
        if(pipost[sugglev]<target){
          curr=ifelse(n[sugglev+1]==0,sugglev+1,sugglev)					
        } else {
        curr=sugglev
        }
      } else {
      curr=sugglev
      }
    
      if(n[curr]>=n.stop){
        stop<-0
        break
      }
      
      i<-i+1
    }		
    if(stop==0){
      dose.select[curr]=dose.select[curr]+1;
    }
    return(list(dose.select=dose.select,tox.data=y,pt.allocation=n,stop=stop,a0 = a0,b0 = b0))
  }

# cdp_DIP(p0,target,ssize,n.stop,cl,method = "mean",N = 20)
# cdp_DIP(p0,target,ssize,n.stop,cl,method = "mode",N = 30)

