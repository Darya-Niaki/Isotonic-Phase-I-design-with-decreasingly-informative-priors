# you can not specify the values for a0 and b0. They are predetermind based on the p0 values.


cdp2<-function(p0,target,ssize,n.stop,cl){
  ndose = length(p0);   #number of dose levels
  y=n=dose.select=numeric(ndose);  #number of DLTs at each dose level
  stop=0; #indicate if trial stops early
  curr = 1
  i=1
  # This part is for a0 and b0
  x = sapply(p0, function(x){min(0.999,2*x)})
  b0 <- sapply(seq_along(p0), function(i){
    mu <- p0[i]
    xi <- x[i]
    
    f <- function(b){
      pbeta(xi, mu * b / (1 - mu), b) - ui
    }
    
    tryCatch({
      uniroot(f, c(0.0001, 100))$root
    }, error = function(e) {
      1
    })
  })
  
  # compute a0 for each mu
  a0 <- p0 * b0 / (1 - p0)
  
  while(i <= ssize){
    y[curr] = y[curr] + rbinom(1,1,p0[curr]);
    
    # is p0 a vector of probabilities? like the first scenario?
    
    n[curr] = n[curr] + 1;
    tried=which(n>0)
    
    safety=ifelse(n[1]>1,1 - pbeta(target, y[1] + a0[1], n[1] - y[1] + b0[1]),0)
    #I shoud see what this part does
    
    if(safety>cl){ #check to see if lowest dose level is too toxic
      stop=1
      break
    }
    
    u=(y[tried]+a0[tried])/(n[tried]+a0[tried]+b0[tried])
    
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
  return(list(dose.select=dose.select,tox.data=y,pt.allocation=n,stop=stop))
}
