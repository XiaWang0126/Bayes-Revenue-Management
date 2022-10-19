############### Part 1: Data Collection and Preparation ############################

# Step 1: Simulate the number of each type of room and customer
# Simulate the number of each type of room
n_Ensuite <- 350
n_Studio <- 500

# Simulate the number of each type of customer
n_New <- 400
n_Current <- 300
n_Twin <- 150


# Step 2: Simulate the WTP of each type of customer for each type of room according to our market research
set.seed(688)
# 1. Setting WTP for new tenant according to the mean market price
# setting WTP according to the mean market price of the Ensuite 
# setting WTP according to the mean market price of the Studio
new.Ensuite <- round(rnorm(n=n_New, mean=300, sd=53),2)
new.Studio <- round(rnorm(n=n_New, mean=379, sd=64),2)

# 2. Assume: Current tenant WTP would be lower than new tenant
Current.Ensuite <- round(rnorm(n=n_Current, mean=300*0.95, sd=53),2) 
Current.Studio <- round(rnorm(n=n_Current, mean=379*0.95, sd=64),2)

# 3. Assume: Twin tenant WTP would be lower than current tenant
Twin.Ensuite <- round(rnorm(n=n_Twin, mean=300*0.92, sd=53),2)
Twin.Studio <- round(rnorm(n=n_Twin, mean=379*0.92, sd=64),2)


# 4. Combine the WTP of each customer into the dataframe
Ensuite <- c(new.Ensuite, Current.Ensuite, Twin.Ensuite)
Studio <- c(new.Studio, Current.Studio, Twin.Studio)


df.WTP <- data.frame(cbind(Ensuite, Studio))

# 5. Assign a label to each type of customer
label <- c(rep("New", n_New),
           rep("Current", n_Current), 
           rep("Twin", n_Twin))

df.WTP$label <- label

# Step 3: Simulate the base price for each type of room according to our market research
baseprice_Ensuite <- 180 # bed space cost for one Ensuite room
baseprice_Studio <- 239 # bed space cost for one Studio room

############### Part 2: Generate Price Points for each type of tenant ################

# Step 1: Setting an empty matrix 
# 1. Setting an empty matrix to save surplus data
new.surplusStudio<-rep(0,n_New)
new.surplusEnsuite<-rep(0,n_New)
current.surplusStudio<-rep(0,n_Current)
current.surplusEnsuite<-rep(0,n_Current)
twin.surplusStudio<-rep(0,n_Twin)
twin.surplusEnsuite<-rep(0,n_Twin)

# 2. Setting an empty matrix to save demand data

# We search prices from basePrice_Ensuite to maxPrice_Ensuite with an increment of 10.
# This gives us 28 price points for EnsuitePrice and similarly
# it will lead to 24 price points for StudioPrice 
# To model the demand in terms of prices.
# The total number of price combinations is 28*24=672
# We will keep track of this through an index variable called index.

new.demandEnsuite<-rep(0,28*24) 
new.demandStudio<-rep(0,28*24)

current.demandEnsuite<-rep(0,28*24)
current.demandStudio<-rep(0,28*24)

twin.demandEnsuite<-rep(0,28*24)
twin.demandStudio<-rep(0,28*24)

# Step 2: Find the max WTP for each type of room
maxprice_Ensuite <- as.integer(max(df.WTP$Ensuite)) # the max price for Ensuite 
maxprice_Studio <- as.integer(max(df.WTP$Studio)) # the max price for Studio



# Step 3: Generate Price Points for each type of customer
# 1. Generate the price points for new tenant
index=1
for (EnsuitePrice in seq(from = baseprice_Ensuite, to = maxprice_Ensuite, by = 10)){
  for (StudioPrice in seq(from = baseprice_Studio, to = maxprice_Studio, by = 16)){
    for (i in 1: n_New){
      new.surplusEnsuite[i]=df.WTP[i,1]-EnsuitePrice
      new.surplusStudio[i]=df.WTP[i,2]-StudioPrice
    }
    # the customer would choose Ensuite if their Ensuite surplus is larger than Studio, while their Ensuite surplus is non-negative
    new.demandEnsuite[index]=sum((new.surplusEnsuite>new.surplusStudio)*(new.surplusEnsuite>=0)) 
    # similarily, the customer would choose Studio if their Studio surplus is larger than Ensuite, while their Studio surplus is non-negative
    new.demandStudio[index]=sum((new.surplusStudio>=new.surplusEnsuite)*(new.surplusStudio>=0))
    index=index+1
  }
}

# 2. Create a table of new tenant data which we will use to run the two regressions for each type of room
newdata<-data.frame(matrix(nrow=28*24,ncol = 5))
colnames(newdata)=c("index","EnsuitePrice","StudioPrice","EnsuiteDemand", "StudioDemand")
index=1
for (EnsuitePrice in seq(from = baseprice_Ensuite, to = maxprice_Ensuite, by = 10)){
  for (StudioPrice in seq(from = baseprice_Studio, to = maxprice_Studio, by = 16)){
    newdata[index,1]=index
    newdata[index,2]=EnsuitePrice
    newdata[index,3]=StudioPrice
    newdata[index,4]=new.demandEnsuite[index]
    newdata[index,5]=new.demandStudio[index]
    index=index+1
  }
}


# 3. Generate the price points for current tenant
index=1
for (EnsuitePrice in seq(from = baseprice_Ensuite, to = maxprice_Ensuite, by = 10)){
  for (StudioPrice in seq(from = baseprice_Studio, to = maxprice_Studio, by = 16)){
    for (i in 1:n_Current){
      current.surplusEnsuite[i]=df.WTP[i+n_New,1]-EnsuitePrice
      current.surplusStudio[i]=df.WTP[i+n_New,2]-StudioPrice
    }
    current.demandEnsuite[index]=sum((current.surplusEnsuite> current.surplusStudio)*(current.surplusEnsuite>=0))
    current.demandStudio[index]=sum((current.surplusStudio>=current.surplusEnsuite)*(current.surplusStudio>=0))
    index=index+1
  }
}

# 4. Create a data table which we will use to run the two regressions for current tenant
currentdata<-data.frame(matrix(nrow=28*24,ncol = 5))
colnames(currentdata)=c("index","EnsuitePrice","StudioPrice","EnsuiteDemand", "StudioDemand")
index=1
for (EnsuitePrice in seq(from = baseprice_Ensuite, to = maxprice_Ensuite, by = 10)){
  for (StudioPrice in seq(from = baseprice_Studio, to = maxprice_Studio, by = 16)){
    currentdata[index,1]=index
    currentdata[index,2]=EnsuitePrice
    currentdata[index,3]=StudioPrice
    currentdata[index,4]=current.demandEnsuite[index]
    currentdata[index,5]=current.demandStudio[index]
    index=index+1
  }
}


# 5. Generate Price Points for twin tenant
index=1
for (EnsuitePrice in seq(from = baseprice_Ensuite, to = maxprice_Ensuite, by = 10)){
  for (StudioPrice in seq(from = baseprice_Studio, to = maxprice_Studio, by = 16)){
    for (i in 1:n_Twin){
      twin.surplusEnsuite[i]=df.WTP[i+n_New+n_Current,1]-EnsuitePrice
      twin.surplusStudio[i]=df.WTP[i+n_New+n_Current,2]-StudioPrice
    }
    twin.demandEnsuite[index]=sum((twin.surplusEnsuite> twin.surplusStudio)*(twin.surplusEnsuite>=0))
    twin.demandStudio[index]=sum((twin.surplusStudio>=twin.surplusEnsuite)*(twin.surplusStudio>=0))
    index=index+1
  }
}

# 6. Create a data table which we will use to run the two regressions for twin tenant
twindata<-data.frame(matrix(nrow=28*24,ncol = 5))
colnames(twindata)=c("index","EnsuitePrice","StudioPrice","EnsuiteDemand", "StudioDemand")
index=1
for (EnsuitePrice in seq(from = baseprice_Ensuite, to = maxprice_Ensuite, by = 10)){
  for (StudioPrice in seq(from = baseprice_Studio, to = maxprice_Studio, by = 16)){
    twindata[index,1]=index
    twindata[index,2]=EnsuitePrice
    twindata[index,3]=StudioPrice
    twindata[index,4]=twin.demandEnsuite[index]
    twindata[index,5]=twin.demandStudio[index]
    index=index+1
  }
}



############### Part 3: Visualisation and Regression #######################

# Step 1: Visualizing Revenue as a Function of Ensuite and Studio for each type of customer
newdata$revenue=newdata$EnsuitePrice*newdata$EnsuiteDemand+newdata$StudioPrice*newdata$StudioDemand
currentdata$revenue=currentdata$EnsuitePrice*currentdata$EnsuiteDemand+currentdata$StudioPrice*currentdata$StudioDemand
twindata$revenue=twindata$EnsuitePrice*twindata$EnsuiteDemand+twindata$StudioPrice*twindata$StudioDemand

# Plot the wireframe of each type of tenant
library(lattice)
wireframe(revenue ~ EnsuitePrice * StudioPrice, data=newdata)
wireframe(revenue ~ EnsuitePrice * StudioPrice, data=currentdata)
wireframe(revenue ~ EnsuitePrice * StudioPrice, data=twindata)

# Step 2: Run the regression for the new tenant customer
# 1. Regression for the dependent variable EnsuiteDemand for new tenant customer
fitnewEnsuite <-lm(EnsuiteDemand ~ EnsuitePrice + StudioPrice, data=newdata)
summary(fitnewEnsuite)

new_a1=coef(fitnewEnsuite)[1]
new_b11=coef(fitnewEnsuite)[2]
new_b12=coef(fitnewEnsuite)[3]

# 2. Regression for the dependent variable StudioDemand for new tenant customer

fitnewStudio <-lm(StudioDemand ~ EnsuitePrice + StudioPrice, data=newdata)
new_a2=coef(fitnewStudio)[1]
new_b21=coef(fitnewStudio)[2]
new_b22=coef(fitnewStudio)[3]

# 3. Get the Output text of the new tenant customer
library('stargazer')
stargazer(fitnewEnsuite, fitnewStudio, type="text")



# Step 3: Run the regression for the current tenant 
# 1. Regression for the dependent variable EnsuiteDemand for current tenant
fitcurrentEnsuite <-lm(EnsuiteDemand ~ EnsuitePrice + StudioPrice, data=currentdata)
summary(fitcurrentEnsuite)

current_a1=coef(fitcurrentEnsuite)[1]
current_b11=coef(fitcurrentEnsuite)[2]
current_b12=coef(fitcurrentEnsuite)[3]

# 2. Regression for the dependent variable StudioDemand for current tenant

fitcurrentStudio <-lm(StudioDemand ~ EnsuitePrice + StudioPrice, data=currentdata)
current_a2=coef(fitcurrentStudio)[1]
current_b21=coef(fitcurrentStudio)[2]
current_b22=coef(fitcurrentStudio)[3]

# 3. Get the Output text of the current customer
library('stargazer')
stargazer(fitcurrentEnsuite, fitcurrentStudio, type="text")



# Step 4: Run the regression for the twin tenant
# 1. Regression for the dependent variable EnsuiteDemand for twin tenant
fittwinEnsuite <-lm(EnsuiteDemand ~ EnsuitePrice + StudioPrice, data=twindata)
summary(fittwinEnsuite)

twin_a1=coef(fittwinEnsuite)[1]
twin_b11=coef(fittwinEnsuite)[2]
twin_b12=coef(fittwinEnsuite)[3]

# 2. Regression for the dependent variable StudioDemand for current customer

fittwinStudio <-lm(StudioDemand ~ EnsuitePrice + StudioPrice, data=twindata)
twin_a2=coef(fittwinStudio)[1]
twin_b21=coef(fittwinStudio)[2]
twin_b22=coef(fittwinStudio)[3]

# 3. Get the Output text of the current customer
library('stargazer')
stargazer(fittwinEnsuite, fittwinStudio, type="text")


# Step 5: Plot the demand and the price
wireframe(EnsuiteDemand ~ EnsuitePrice * StudioPrice, data=newdata)
wireframe(StudioDemand ~ EnsuitePrice * StudioPrice, data=newdata)
wireframe(EnsuiteDemand  ~ EnsuitePrice * StudioPrice, data=currentdata)
wireframe(StudioDemand   ~ EnsuitePrice * StudioPrice, data=currentdata)
wireframe(EnsuiteDemand   ~ EnsuitePrice * StudioPrice, data=twindata)
wireframe(StudioDemand   ~ EnsuitePrice * StudioPrice, data=twindata)

############### Part 4: Price Optimisation ################
# Step 1: Finding optimal revenue by optimization
library("nloptr")

# Define Objective Function 
eval_f <- function(x){
  
  # Differentiated Prices
  newEnsuitePrice=x[1]
  currentEnsuitePrice=x[2]
  twinEnsuitePrice=x[3]
  newStudioPrice=x[4]
  currentStudioPrice=x[5]
  twinStudioPrice=x[6]
  
  # Calculate the number of demand for each type of room and customer 
  # according to the intercept and coefficient from the regression model
  newEnsuiteDemand = as.integer(max(0,new_a1 + new_b11*newEnsuitePrice + new_b12 * newStudioPrice))
  currentEnsuiteDemand = as.integer(max(0, current_a1 + current_b11 * currentEnsuitePrice + current_b12 * currentStudioPrice))
  twinEnsuiteDemand = as.integer(max(0, twin_a1 +twin_b11 * twinEnsuitePrice + twin_b12 * twinStudioPrice))
  
  
  newStudioDemand = as.integer(max(0, new_a2 + new_b21 * newEnsuitePrice + new_b22 * newStudioPrice))
  currentStudioDemand = as.integer(max(0, current_a2 + current_b21 * currentEnsuitePrice + current_b22 * currentStudioPrice))
  twinStudioDemand = as.integer(max(0, twin_a2 + twin_b21 * twinEnsuitePrice + twin_b22 * twinStudioPrice))
  
  revenue = newEnsuiteDemand * newEnsuitePrice + 
    currentEnsuiteDemand * currentEnsuitePrice + 
    twinEnsuiteDemand * twinEnsuitePrice +
    newStudioDemand * newStudioPrice +
    currentStudioDemand * currentStudioPrice +
    twinStudioDemand * twinStudioPrice
    
  # Maximise the total revenue 
  objfunction=-revenue 
  return(objfunction)
}

# Define Constraint Function
eval_g_ineq <- function(x) {
  
  # Differerntiated Price
  newEnsuitePrice=x[1]
  currentEnsuitePrice=x[2]
  twinEnsuitePrice=x[3]
  newStudioPrice=x[4]
  currentStudioPrice=x[5]
  twinStudioPrice=x[6]
  
  # Calculate the number of demand for each type of room and customer 
  # according to the intercept and coefficient from the regression model
  newEnsuiteDemand = as.integer(max(0,new_a1 + new_b11*newEnsuitePrice + new_b12 * newStudioPrice))
  currentEnsuiteDemand = as.integer(max(0, current_a1 + current_b11 * currentEnsuitePrice + current_b12 * currentStudioPrice))
  twinEnsuiteDemand = as.integer(max(0, twin_a1 +twin_b11 * twinEnsuitePrice + twin_b12 * twinStudioPrice))
  
  
  newStudioDemand = as.integer(max(0, new_a2 + new_b21 * newEnsuitePrice + new_b22 * newStudioPrice))
  currentStudioDemand = as.integer(max(0, current_a2 + current_b21 * currentEnsuitePrice + current_b22 * currentStudioPrice))
  twinStudioDemand = as.integer(max(0, twin_a2 + twin_b21 * twinEnsuitePrice + twin_b22 * twinStudioPrice))
  
  # There are mainly four types of constraint: 
  # (1) price level associate with type of room
  # (2) total demand <= capacity ; 
  # (3) demand nonnegativity 
  # (4) each type of customer for room demand <=  its total number of customer

  # Price Constraints: Price difference between type of room is according to the promotion mentioned before
  constraint <- c(# (1) price constraint
                  x[5]-x[4]*0.95,  # price difference between currentStudioPrice and newStudioPrice is more than 5% 
                  x[6] -x[4]*0.92,  # price difference between twinStudioPrice and newStudioPrice is more than 8%
                  x[3]-x[2] + 10, # Ensuite price difference between current and twin should be more than 10 units
                  x[1] - x[6],  # we set twinStudioPrice is more than newEnsuitePrice
                  
                  x[2] - x[1]*0.95, # price difference between currentEnsuitePrice and newEnsuitePrice is more than 5% 
                  x[3] - x[1]*0.92, # price difference between twinEnsuitePrice and newEnsuitePrice is more than 8% 
                  x[6]-x[5]+10, # Studio price difference between current and twin should be more than 10 units
                  
                  # (2) Room Capacity Constraints
                  # Studio Capacity
                  newStudioDemand + currentStudioDemand + twinStudioDemand*2 - n_Studio,
                  # Ensuite Capacity
                  newEnsuiteDemand + currentEnsuiteDemand + twinEnsuiteDemand*2 - n_Ensuite,
                  
                  # (3) Demand Nonnegativity constraints
                  # Demand for each type of room
                  -newEnsuiteDemand,
                  -currentEnsuiteDemand,
                  -twinEnsuiteDemand,
                  -newStudioDemand,
                  -currentStudioDemand,
                  -twinStudioDemand,
                  
                  # (4) Each type of customer for room demand <= its total number of customer
                  # New tenant demand
                  newStudioDemand + newEnsuiteDemand - n_New,
                  # Current tenant demand
                  currentStudioDemand + currentEnsuiteDemand - n_Current,
                  # Twin tenant demand
                  twinStudioDemand + twinEnsuiteDemand -n_Twin)
  
  return(constraint)
}

# Setting initial values according to the base price of each type of room
x0 <- c(rep(baseprice_Ensuite, 3),rep(baseprice_Studio, 3))
# Setting lower and upper bounds of control based on our market research
lb <- c(rep(baseprice_Ensuite, 3),rep(baseprice_Studio, 3))
ub <- c(rep(as.numeric(maxprice_Ensuite), 3),rep(as.numeric(maxprice_Studio), 3))

opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)

# Get the optimal results of the solution
priceOpt<-result$solution
RevenueOpt<- -result$objective

# Print the price point of each type of room for each type of customer
print(paste("The Optimal New Tenant Ensuite Price is:",round(priceOpt[1],2)))
print(paste("The Optimal Current Tenant Ensuite Price is:",round(priceOpt[2],2)))
print(paste("The Optimal Twin Ensuite Price is:",round(priceOpt[3],2)))
print(paste("The Optimal New Studio Price is:",round(priceOpt[4],2)))
print(paste("The Optimal Current Studio Price is:",round(priceOpt[5],2)))
print(paste("The Optimal Twin Studio Price is:",round(priceOpt[6],2)))

# Print the optimal revenue result from the optimal prices 
print(paste('The Optimal Revenue is:', round(-result$objective,2)))


############### Part 5: Quantity Optimisation - Sequential Arrival #######################
# Our scenario: we predict the demand of Ensuite might be lower due to COVID issues.
# Step 1: Calculate the demand of each type of room according to the price set 

# 1. Get the demand of the Ensuite Room for each type of customer after adjusting price (decrease Ensuite price by 10%)
newEnsuiteDemandAdj = as.integer(max(0,new_a1 + new_b11*priceOpt[1]*0.90 + new_b12 * priceOpt[4]))
currentEnsuiteDemandAdj= as.integer(max(0, current_a1 + current_b11 *priceOpt[2]*0.90 + current_b12 * priceOpt[5]))
twinEnsuiteDemandAdj = as.integer(max(0, twin_a1 +twin_b11 * priceOpt[3]*0.90 + twin_b12 * priceOpt[6]))

# 2. Get the demand of the Studio Room for each type of customer after adjusting price (decrease Ensuite price by 10%)
newStudioDemandAdj = as.integer(max(0,new_a2+new_b21*priceOpt[1]*0.9+new_b22*priceOpt[4]))
currentStudioDemandAdj = as.integer(max(0,current_a2+current_b21*priceOpt[2]*0.9+current_b22*priceOpt[5]))
twinStudioDemandAdj = as.integer(max(0,twin_a2+twin_b21*priceOpt[3]*0.9+twin_b22*priceOpt[6]))

# Step 2: Multiple Tenant Classes with Sequential Arrival for Ensuite
# 1. Setting parameters
J<-3;     # number of fare classes/customers
Ensuiteprice<-c(priceOpt[1]*0.90, priceOpt[2]*0.90,priceOpt[3]*0.90);  # prices for each tenant class, from highest to lowest
# Expected demand for each tenant class according to the demand at the adjusted price
expd<-c(newEnsuiteDemandAdj,currentEnsuiteDemandAdj,twinEnsuiteDemandAdj);  
N<-n_Ensuite;   # Ensuite capacity

# 2. Create the empty matrix to record the results
Ensuitev<-matrix(0, nrow = (J+1), ncol = (N+1));  
Ensuiteybest<-matrix(0, nrow = (J+1), ncol = (N+1));


# 3. Dynamic Programming Recursion
for(i in 2:(J+1)){ 
  for(n in 1:(N+1)){
    x=n-1; # inventory level
    valuebest=-999;
    for(y in 0:x){ # protect for future stages
      avail=x-y; # available for this stage
      value=0; # to start computing the expected revenue
      for(d in 0:250){ 
        if (i == 4){ # for twin tenant arrival, we will sell two rooms
          sold = min(avail, d*2)
        }
        else{
          sold=min(avail,d)}
        # the expected value
        value=value+
          dpois(d, expd[i-1])*(Ensuiteprice[i-1]*sold+Ensuitev[i-1,n-sold])
      }
      if(value>valuebest){
        Ensuiteybest[i,n]=y;
        valuebest=value;
      }
    }
    Ensuitev[i,n]=valuebest;
  }
}

# 4. Get the results of the optimal protection limits for each type of customer for Ensuite Room 
EnsuiteOptimalProtectionLimits<-c(Ensuiteybest[3,155],Ensuiteybest[4,350])
print(paste('The Optimal Protection Level for New Ensuite Tenant is:',EnsuiteOptimalProtectionLimits[1]))
print(paste('The Optimal Protection Level for Current Ensuite Tenant is:',
            (EnsuiteOptimalProtectionLimits[2]-EnsuiteOptimalProtectionLimits[1])))
# Optimal protection level: we protect 154 for new tenant, 132 for current tenant

# 5. Get the result of the optimal total expected revenue for Ensuite
OptimalTotalExpectedRevenue=Ensuitev[J+1,N+1]
print(paste('The Optimal Total Expected Revenue for Ensuite is:', round(OptimalTotalExpectedRevenue,2)))



############### Part 6: Quantity Optimisation - Mixed Arrival #######################

# Step 1: Calculate arrival probability for each type of room and customer

# 1. Calculate total Ensuite arrival
totalEnsuite = newEnsuiteDemandAdj+ currentEnsuiteDemandAdj +twinEnsuiteDemandAdj

# 2. Calculate total Studio arrival
totalStudio = newStudioDemandAdj + currentStudioDemandAdj + twinStudioDemandAdj

# 3. Calculate total demand
totalDemand <- totalEnsuite + totalStudio

# 4. Calculate total customer
totalCustomer <- n_New + n_Current + n_Twin

# 5 Calculate total No arrival
totalNoArrival <- totalCustomer - totalDemand

# 6. Calculate the number of total no arrival customer for Ensuite
EnsuiteNoArrival <- totalNoArrival * (totalEnsuite/totalDemand)


# 7. Calculate the probability of customer arrival for Ensuite room
new_E_prob <- newEnsuiteDemandAdj / (totalEnsuite + EnsuiteNoArrival)
current_E_prob <- currentEnsuiteDemandAdj/ (totalEnsuite + EnsuiteNoArrival)
twin_E_prob <- twinEnsuiteDemandAdj / (totalEnsuite + EnsuiteNoArrival)
no_E_prob <- 1- new_E_prob - current_E_prob - twin_E_prob


# Step 2: Dynamic Programming with mixed arrivals
# 1. Setting paramters
N=n_Ensuite; # Ensuite room availability
TT= as.integer(totalEnsuite + EnsuiteNoArrival) # Length of time horizon
prob0=no_E_prob
prob1=new_E_prob
prob2=current_E_prob
prob3= twin_E_prob
price1= priceOpt[1]*0.90
price2 = priceOpt[2]*0.90
price3= priceOpt[3]*0.90

# 2. Create the empty matrix to record the results
Mixv=matrix(rep( 0, len=(N+1)*(TT+1)), nrow=N+1);
acceptCurrent=matrix(rep( 0, len=(N+1)*(TT+1)), nrow=N+1); # decision for current tenant (middle-fare)
acceptTwin = matrix(rep( 0, len=(N+1)*(TT+1)), nrow=N+1); # decision for twin tenant (low-fare)

# 3. Dynamic Programming Recursion
# Terminal Values
for(i in 1:(N+1)){
  Mixv[i,1]=0;
}

for(t in 2:(TT+1)){ #2:TT+1
  for(i in 1:(N+1)){ #1:N1+1
    
    # For no arrivals:
    vtogo0=Mixv[i,t-1];
    
    # For new tenant arrival:
    vtogo1=Mixv[i,t-1]; # default
    # If resource available:
    if(i>1){
      vtogo1=price1+Mixv[i-1,t-1];
    }
    
    # For current tenant arrival:
    vtogo2=Mixv[i,t-1];
    acceptCurrent[i,t]=0;
    # If resource available:
    if(i>1){
      vtogo2=max(price2+ Mixv[i-1,t-1], Mixv[i,t-1]);
      # Recording the decision in the acceptCurrent variable:
      if(price2+Mixv[i-1,t-1]>Mixv[i,t-1]){
        acceptCurrent[i,t]=1;
      }
    }
    
    # For twin tenant arrival:
    vtogo3=Mixv[i,t-1];
    acceptTwin[i,t]=0;
    # If resource available:
    if(i>2){
      vtogo3=max(price3*2 + Mixv[i-2,t-1],Mixv[i,t-1]); # we will sell 2 rooms to twin tenant
      # Recording the decision in the acceptTwin variable:
      if(price3*2 +Mixv[i-2,t-1]>Mixv[i,t-1]){
        acceptTwin[i,t]=1;
      }
    }
    
    # Obtaining the overall value function from its parts:
    Mixv[i,t]=prob0*vtogo0+prob1*vtogo1+prob2*vtogo2+ prob3*vtogo3
  }
}

# 4. Get the result of the optimal total expected revenue
MixOptimalTotalExpectedRevenue=Mixv[(N+1),(TT+1)]
print(paste('The Optimal Total Expected Revenue for Mixed Arrival is:', round(MixOptimalTotalExpectedRevenue,2)))


# Step 3. Visualizing the Optimal Policy Structure
# 1. Visualise the optimal policy for current tenant
acceptanceCurrent<-t(acceptCurrent[2:(N+1),2:(TT+1)]); # transpose of accept current (horizontal:time)
xaxis<-1:TT
yaxis<-1:N
filled.contour(xaxis,yaxis,acceptanceCurrent,xaxt="n",yaxt="n",
               key.axes = axis(4, seq(0, 1, by = 1)), nlevels = 2,
               color.palette = rainbow,
               plot.title =title(main='Optimal Current Tenant Policy Structure',
                                 xlab="Remaining Time", 
                                 ylab="Remaining Number of Ensuite Rooms" ))


# 2. Visualise the optimal policy for twin tenant
acceptanceTwin<-t(acceptTwin[2:(N+1),2:(TT+1)]); # transpose of accept twin (horizontal:time)
xaxis<-1:TT
yaxis<-1:N
filled.contour(xaxis,yaxis,acceptanceTwin,xaxt="n",yaxt="n",
               key.axes = axis(4, seq(0, 1, by = 1)), nlevels = 2,
               color.palette = rainbow,
               plot.title =title(main='Optimal Twin Tenants Policy Structure',
                                 xlab="Remaining Time", 
                                 ylab="Remaining Number of Ensuite Rooms"))
