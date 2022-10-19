################## Question 1 ----
library(nloptr)
library(stargazer)
library(lattice)

# Read the data
df=read.csv("CongestionPricing.csv",header = TRUE)

# Histograms of WTPs
par(mfrow=c(1,2))
library(Hmisc)
hist.data.frame(df[2:3])

### Section a) Single congestion price charge ----
################ Step 1: know the demand at each price level
# Compute maximum willingness to pay for each surveyed driver and enter it as a new column to the dataframe
N=nrow(df)
for (i in 1:N){
  df$maxWTP[i]=max(df[i,2:3])
}

# The upperbound price will be the maximum price of the maximum WTP
maxprice=max(df$maxWTP)

# Defining empty array variables we will be introducing to record demand and revenue level
demand=rep(NA,maxprice)
revenue=rep(NA,maxprice)

######### Step 2: Compute the demand and revenue at each price level to identify the maximum
# Find how many people buy at each price level
for (p in 1:maxprice){
  # The customers that have WTP grater or equal to the price in each iteration, 
  # will be assigned True values and False otherwise.
  # Within the array of demand, the True values will be treated as 1 and False as 0,
  # sum them up to know the number of demand at each price.
  # Once known the demand, we can also obtain the revenue at each price, by multiplying demand and price.
  demand[p]=sum(df$maxWTP>=p)
  revenue[p]=p*demand[p]
}

# Identifying the Best Price that gives the maximum revenue within the surveyed drivers.
revenueBest=max(revenue)
priceBest=which(revenue == revenueBest)
print(paste("If a single price is to be charged across peak and non-peak hours, the optimal price is:",priceBest))
# The price that would maximize the revenue is 8.

############## Step 3: Compute the general demand and revenue with the price obtained
# The result obtained in previous step is in terms of the surveyed drivers,
# who represent the rest of drivers in at the population level.
# In this step we will compute the total demand, the total revenue obtained and the total level of emission.

# We need to know the number of cars entering to the charging zone with the price=8
survey_cars = sum(df$maxWTP >= 8) # the demand of drivers within survey with WTP >= 8
total_cars = 192000
proportion = survey_cars/N # the percetage of entering surveyed cars that represent to the total cars
# We need to multiply the proportion of the surveyed drivers that will enter with
# the total number of cars to know the total number of cars entering to the charging zone
# For common sense, we will set the total number of cars as integer,
# since half cars do not exist, for instance.
entering_cars = as.integer(proportion * total_cars) 

# The total revenue with single charging price=8: 
max_revenue = priceBest * entering_cars
print(paste("The maximum revenue from single charging price is:", max_revenue))

# The total level of emission
# The level of emission depends on the average speed of the cars
# The average speed of the cars is computed by: 
# Average speed = 30 - 0.0625* number of cars in thousands
ave_speed = 30 - 0.0625*(entering_cars/1000) # we need to divide the number of cars by 1000
print(paste("The average speed of the cars is:", ave_speed))

# Once we have the average speed of the cars, we can compute the level of emission per car
# The formula to compute the emission per car is: 
# Emissions per car (g/km) = 617.5 – 16.7 * (Average Speed) if average speed < 25
emission = 617.5 - 16.7 * ave_speed
print(paste("The emission per car is:", emission))
# We can know the total emission level by multiplying the emission per car 
# to the number of cars willing to enter to the charging zone at price=8
total_emission = emission * entering_cars
print(paste("The total emission level at charging price=8 is:", total_emission, "g/km"))


### Section b) Peak price charging ----
################ Step 1: Know the nonpeak consumer surplus with the congestion charge for nonpeak hours
######################## and the consumer surplus of peak hours at different price level

# Compute the difference between maximum willingness to pay for each surveyed driver for the nonpeak hour
# and the actual charged price to know the demand
basePrice= 7 #Nonpeak price
N=nrow(df)
demandNonPeak<-rep(0,maxprice)
demandPeak<-rep(0,maxprice)
revenue2<-rep(0,maxprice)

# New column that computes the difference between the max WTP and the 
# nonpeak price charge called maxsurplusNonPeak
maxsurplusNonPeak<-rep(0,N)

for (i in 1:N){
  maxsurplusNonPeak[i]=max(df[i,3]-basePrice)
  df$maxsurplusNonPeak[i]=max(df[i,3]-basePrice)
  
}

# Compute the consumer surplus of peak hours at different price levels
# and record the outputs in a matrix.
surplusPeak<-matrix(0,N,maxprice)

# Each column of the matrix represent the price level of peak hour charge
for (p in 1:maxprice){
  for (i in 1:N){
    surplusPeak[i,p]=df[i,2]-p
  }
}

################# Step 2: Compare the consumer surplus between peak and nonpeak to know the demand
########################  of peak and nonpeak at different prices hence, identify the maximum revenue

# In each p, the drivers will chose the option that gives them higher surplus 
for (p in 1:maxprice){
  demandNonPeak[p]=sum((maxsurplusNonPeak>surplusPeak[,p])*(maxsurplusNonPeak>=0))
  demandPeak[p]=sum((surplusPeak[,p]>=maxsurplusNonPeak)*(surplusPeak[,p]>=0))
  revenue2[p]=basePrice*demandNonPeak[p]+p*demandPeak[p]
}

revenueBest2=max(revenue2[basePrice:maxprice]) # the maximum revenue
priceBest2=which(revenue2 == revenueBest2) # Optimal peak price

########### Step 3: Know the general demand of peak and nonpeak, the total revenue obtained 
################## and the total emission level between the two time periods. 

# The general number of cars entering to the charging zone in peak and nonpeak hours
demand_peak = demandPeak[priceBest2]
demand_nonpeak = demandNonPeak[priceBest2]
total_cars = 192000
# the demand percentage of surveyed cars that represent to the total cars
proportion_peak = demand_peak/N 
proportion_nonpeak = demand_nonpeak/N 
# We need to multiply the proportion of each period with the total number of cars
# to know the number of cars entering to the charging zone in each period
entering_cars_peak = as.integer(proportion_peak * total_cars) # the number of cars should be integer 
entering_cars_nonpeak = as.integer(proportion_nonpeak * total_cars)

# Once we know the demand, we can compute the total revenue obtained by multiplying 
# the demand of each period by the different charging prices
max_revenue2 = priceBest2 * entering_cars_peak + basePrice * entering_cars_nonpeak
print(paste("The maximum revenue from different charging price is:", max_revenue2))

# To compute the emission level, we need to know the average speed during peak and nonpeak hours
# The average speed of the cars
ave_speed_peak = 30 - 0.0625*(entering_cars_peak/1000)
ave_speed_nonpeak = 30 - 0.0625*(entering_cars_nonpeak/1000)

# Emission level per car in each period
emission_peak = 617.5 - 16.7 * ave_speed_peak
emission_nonpeak = 235 - 1.4 *  ave_speed_nonpeak

# We can know the total emission level by multiplying the emission per car 
# to the number of cars willing to enter to the charging zone at each period and sum them up
total_emission_peak = emission_peak * entering_cars_peak
total_emission_nonpeak = emission_nonpeak * entering_cars_nonpeak
total_emission2 = total_emission_peak + total_emission_nonpeak # the total emission generated


#### Section c) Minimizing emission level ----
# In this case, we need to minimize the emission level and at the same time, 
# obtain a revenue of at least 1.1 millions with different charge price. 
# Nonpeak charge= 7, find the optimal Peak charge.
# We will use nonlinear programming to find the solution to the optimization problem.


############### Step 1: Define the objective function of the problem

# This function will return the value of the objective function: minimum emission level
# when the revenue is larger than 1.1 millions.
eval_f <- function(x){
  basePrice= 7 # the nonpeak price
  peakPrice= x # we need to find the optimal peak price
  # both demand will be the same as the previous section as nonpeak price has not changed
  NonPeakDemand= demandNonPeak[x] 
  PeakDemand=demandPeak[x]
  # the total number of cars entering to the zone in each period
  entering_cars_peak4 = (PeakDemand/N) * total_cars # number of cars as integer
  entering_cars_nonpeak4 = (NonPeakDemand/N) * total_cars
  # the average speed from peak and nonpeak hours
  ave_speed_peak4 <- 30 - 0.0625*(entering_cars_peak4/1000)
  ave_speed_nonpeak4 <- 30 - 0.0625*(entering_cars_nonpeak4/1000)
  # set conditions to compute the total emission according to the average speed of cars in each period
  # for peak hours
  if (ave_speed_peak4 < 25){
    total_emission_peak4 <- (617.5 - 16.7 * ave_speed_peak4) *entering_cars_peak4
  } else{
    total_emission_peak4 <- (235 - 1.4 * ave_speed_peak4) * entering_cars_peak4
  }
  # for nonpeak hours
  if (ave_speed_nonpeak4 < 25){
    total_emission_nonpeak4 <- (617.5 - 16.7 * ave_speed_nonpeak4)* entering_cars_nonpeak4
  } else{
    total_emission_nonpeak4 <- (235 - 1.4 * ave_speed_nonpeak4) * entering_cars_nonpeak4
  }
  # the total emission level
  emission4 = total_emission_peak4 + total_emission_nonpeak4 
  # the objective function
  objfunction=emission4 # it's positive since we are minimizing
  return(objfunction)
}

################## Step 2: Define the constraints function of the problem

# This function will evaluate all the constraints that should hold in the solution of the problem
# In this case, the main constraints are the non-negative value for the total number of cars entering in each period
# and the revenue constraint: at leat 1.1 millions.
eval_g_ineq <- function(x) {
  # The first part is hold the same
  basePrice=7
  peakPrice= x
  NonPeakDemand= demandNonPeak[x]
  PeakDemand=demandPeak[x]
  entering_cars_peak4 = as.integer((PeakDemand/N) * total_cars) # the number of cars as integer
  entering_cars_nonpeak4 = as.integer((NonPeakDemand/N) * total_cars)
  # Compute the total revenue obtained
  revenue_optimal= peakPrice * entering_cars_peak4 + basePrice * entering_cars_nonpeak4
  # the constraints that should be hold
  # the structure of the constraints should follow <= inequality
  constraint <- c(-entering_cars_peak4, # non-negative value
                  -entering_cars_nonpeak4, # non-negative value
                  1100000-revenue_optimal # we want the optimal revenue >= 1.1 millions
  )
  return(constraint)
}

############### Step 3: Apply the functions defined to get the optimal result

# initial values
x0 <- 1
# lower and upper bounds of control
lb <- 1
ub <- maxprice
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts) # nonlinear programming

# Get the Optimal peak price charge
priceOpt<-result$solution
# Get the minimum emission level that holds all the constraints
emissionOpt<- result$objective
# Know the revenue obtained
revenueOpt <- -(eval_g_ineq(priceOpt)[3]-1100000)

# Print the results
print(paste0("The Optimal emission level is: ",emissionOpt, " the Optimal price is ",
             priceOpt, " and the revenue obtained is ", revenueOpt))


