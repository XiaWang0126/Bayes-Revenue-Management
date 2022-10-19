# Question 1 ----
mM=50          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM=1           # Price for Morning croissant
pL=1.5          # Price for Lunch croissant
capacity=50    # Capacity 
## a) FCFS: ----
ExpRevenue=rep(0,capacity+1)
for (i in 1:1){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
RevenueFCFS=ExpRevenue[1]
print(paste("Lower Bound for Expected Revenue (FCFS):", round(RevenueFCFS,1)))

## b) Protection level:----
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest))

## c) Optimal revenue with protection level: ----
OptimalExpRevenue=max(ExpRevenue)
print(paste("The daily revenue from the protection level is:", OptimalExpRevenue))
Improvement = round((OptimalExpRevenue- RevenueFCFS)/RevenueFCFS *100, 2)
print(paste("The percent of improvement compared with FCFS is:", Improvement, "%"))

## d) Allocation decision changes: ----
## Ceteris paribus assumption in each situation
### 1. When demand for Lunch croissant changes: ----
#### 1.1. Demand increases:
mM=50          # Mean Demand for Morning croissant, Poisson
mL2=30           # Mean Demand for Lunch croissant, Poisson
pM=1           # Price for Morning croissant
pL=1.5          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL2)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest2=Protectindexbest-1
OptimalExpRevenue2=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest2))
print(paste("The Revenue is:", OptimalExpRevenue2))

#### 1.2. Demand decreases:
mM=50          # Mean Demand for Morning croissant, Poisson
mL3=15           # Mean Demand for Lunch croissant, Poisson
pM=1           # Price for Morning croissant
pL=1.5          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL3)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest9=Protectindexbest-1
OptimalExpRevenue9=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest9))
print(paste("The Revenue is:", OptimalExpRevenue9))

### 2. When the price of Morning croissant changes:----
#### 2.1. Morning croissant price increases but is < Lunch croissant price:
mM=50          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM2=1.25           # Price for Morning croissant
pL=1.5          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM2*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest3=Protectindexbest-1
OptimalExpRevenue3=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest3))
print(paste("The Revenue is:", OptimalExpRevenue3))

#### 2.2. Morning croissant price increases and is = Lunch croissant price: 
mM=50          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM3=1.5          # Price for Morning croissant
pL=1.5          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM3*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest4=Protectindexbest-1
OptimalExpRevenue4=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest4))
print(paste("The Revenue is:", OptimalExpRevenue4))

#### 2.3. Morning croissant price increases and is > Lunch croissant:
mM=50          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM4=2          # Price for Morning croissant
pL=1.5          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM4*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest5=Protectindexbest-1
OptimalExpRevenue5=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest5))
print(paste("The Revenue is:", OptimalExpRevenue5))

#### 2.4. Morning croissant price decreases:
mM=50          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM5=0.75          # Price for Morning croissant
pL=1.5          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM5*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest6=Protectindexbest-1
OptimalExpRevenue6=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest6))
print(paste("The Revenue is:", OptimalExpRevenue6))


### 3. The price of Lunch croissant changes:----
#### 3.1. The price increases:
mM=50          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM=1          # Price for Morning croissant
pL2=1.75          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL2*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest7=Protectindexbest-1
OptimalExpRevenue7=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest7))
print(paste("The Revenue is:", OptimalExpRevenue7))

#### 3.2. The price decreases:
mM=50          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM=1          # Price for Morning croissant
pL3=1.25          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL3*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest7b=Protectindexbest-1
OptimalExpRevenue7b=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest7b))
print(paste("The Revenue is:", OptimalExpRevenue7b))

### 4. The number of croissant the restaurant receives everyday: ----
#### 4.1. The number of croissant increases:
mM=50          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM=1           # Price for Morning croissant
pL=1.5         # Price for Lunch croissant
capacity2=80    # Capacity 

ExpRevenue=rep(0,capacity2+1)
for (i in 1:(capacity2+1)){
  protect=i-1
  availforLowFare=capacity2-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity2-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest7=Protectindexbest-1
OptimalExpRevenue7=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest7))
print(paste("The Revenue is:", OptimalExpRevenue7))

#### 4.2. The number of croissant decreases:
mM=50          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM=1           # Price for Morning croissant
pL=1.5         # Price for Lunch croissant
capacity3=45    # Capacity 

ExpRevenue=rep(0,capacity3+1)
for (i in 1:(capacity3+1)){
  protect=i-1
  availforLowFare=capacity3-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity3-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest8=Protectindexbest-1
OptimalExpRevenue8=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest8))
print(paste("The Revenue is:", OptimalExpRevenue8))


### 5. When demand for Morning croissant changes: ----
#### 5.1. Demand increases:
mM3=40          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM=1           # Price for Morning croissant
pL=1.5          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM3)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest10b=Protectindexbest-1
OptimalExpRevenue10b=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest10b))
print(paste("The Revenue is:", OptimalExpRevenue10b))

#### 5.2. Demand decreases:
mM2=60          # Mean Demand for Morning croissant, Poisson
mL=20           # Mean Demand for Lunch croissant, Poisson
pM=1           # Price for Morning croissant
pL=1.5          # Price for Lunch croissant
capacity=50    # Capacity 

ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pM*soldLowFare+pL*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mM2)*dpois(dH,mL)
    }
  }
}
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest10=Protectindexbest-1
OptimalExpRevenue10=max(ExpRevenue)
print(paste("The Optimal Protection Level for Lunch croissant Demand:", ProtectBest10))
print(paste("The Revenue is:", OptimalExpRevenue10))

# Question 2: ----
## d) Dynamic programming algorithm:----

N1=100; # Leg 1 seat availability
N2=120; # Leg 2 seat availability
TT=300; # Length of time horizon

# Adding the probability of new product.
arrivalprob=c(1/5, 4/15, 1/6, 4/15,1/20);

# Adding the price of new product.
price=c(150,120,250,180,200);

# Creating empty arrays of correct dimensions.
# For the value function v(x1,x2,t):
v=array(rep( 0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1,N2+1,TT+1));
# To keep track of optimal decisions.
# The new product will be added at the end, as Product 5.
accept1=array(rep( 0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1,N2+1,TT+1));
accept2=array(rep( 0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1,N2+1,TT+1));
accept3=array(rep( 0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1,N2+1,TT+1));
accept4=array(rep( 0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1,N2+1,TT+1));
accept5=array(rep( 0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1,N2+1,TT+1));

totalarrivalprob=sum(arrivalprob);
noarrivalprob=1-totalarrivalprob;

# Terminal Values
for(i in 1:(N1+1)){
  for(j in 1:(N2+1)){
    v[i,j,1]=0; # All seats worthless at the end of horizon, i.e., t=1.
  }
}

# Dynamic Programming Algorithm

for(t in 2:(TT+1)){ #2:TT+1
  for(i in 1:(N1+1)){ #1:N1+1
    for(j in 1:(N2+1)){ #1:N2+1
      
      # For no arrivals:
      vforarrival0=v[i,j,t-1];
      
      # For Product 1 arrival:
      # default not accept unless able/profitable to accept
      vforarrival1=v[i,j,t-1];
      accept1[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival1=max(price[1]+v[i-1,j,t-1],v[i,j,t-1]);
        # Recording the decision in the accept1 variable:
        if(price[1]+v[i-1,j,t-1]>v[i,j,t-1]){
          accept1[i,j,t]=1;
        }
      }
      
      # For Product 2 arrival:
      # default not accept unless able/profitable to accept
      vforarrival2=v[i,j,t-1];
      accept2[i,j,t]=0;
      # If resource available:
      if(j>1){
        vforarrival2=max(price[2]+v[i,j-1,t-1],v[i,j,t-1]);
        # Recording the decision in the accept2 variable:
        if(price[2]+v[i,j-1,t-1]>v[i,j,t-1]){
          accept2[i,j,t]=1;
        }
      }
      
      # For Product 3 arrival:
      # default not accept unless able/profitable to accept
      vforarrival3=v[i,j,t-1];
      accept3[i,j,t]=0;
      # If resources available:
      if(i>1){
        if(j>1){
          vforarrival3=max(price[3]+v[i-1,j-1,t-1],v[i,j,t-1]);
          # Recording the decision in the accept3 variable:
          if(price[3]+v[i-1,j-1,t-1]>v[i,j,t-1]){
            accept3[i,j,t]=1;
          }
        }
      }
      
      # For Product 4 arrival:
      # default not accept unless able/profitable to accept
      vforarrival4=v[i,j,t-1];
      accept4[i,j,t]=0;
      # If resources available:
      if(i>1){
        if(j>1){
          vforarrival4=max(price[4]+v[i-1,j-1,t-1],v[i,j,t-1]);
          # Recording the decision in the accept4 variable:
          if(price[4]+v[i-1,j-1,t-1]>v[i,j,t-1]){
            accept4[i,j,t]=1;
          }
        }
      }
      
      # For New Product arrival (Product 5):
      # default not accept unless able/profitable to accept
      vforarrival5=v[i,j,t-1];
      accept5[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival5=max(price[1]+v[i-1,j,t-1],v[i,j,t-1]);
        # Recording the decision in the accept5 variable:
        if(price[5]+v[i-1,j,t-1]>v[i,j,t-1]){
          accept5[i,j,t]=1;
        }
      }
      
      # Obtaining the overall value function from its parts:
      v[i,j,t]=noarrivalprob*vforarrival0+
        arrivalprob[1]*vforarrival1+
        arrivalprob[2]*vforarrival2+
        arrivalprob[3]*vforarrival3+
        arrivalprob[4]*vforarrival4+
        arrivalprob[5]*vforarrival5;
    }
  }
}

