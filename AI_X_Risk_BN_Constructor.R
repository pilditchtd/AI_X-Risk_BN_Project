##### AI X-Risk BN Model Constructor #####

## Written by Toby D. Pilditch, PhD - All mistakes are my own. ##


#### Step 1: Loading in requisite packages (Need to do this only once on a new installation)
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("graph")
# BiocManager::install("Rgraphviz")
# install.packages("gRain", dependencies = TRUE) ##necessary for first run on a new machine
# install.packages("gRbase") ## these two (gRbase and RBGL) may be unnecessary if depedencies (see above) have installed
# install.packages("truncnorm") # Check for up to date packages? or if these have been incorporated into stats packages...
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("dplyr")
# install.packages("plyr")
# install.packages("hexbin")

## do need to run these function calls every time you load up the script
library(gRbase)
library(gRain)

# distribution sampling packages:
library(stats)
library(truncnorm)

## Supplementary package installation (e.g. graphing, tables) ##
library(dplyr)
library(reshape2)
library(ggplot2)
library(hexbin)

#### Step 2: Input parameters and definitions

SecondOrdProb_YN <- "True" # "False" = no second order uncertainty incorporated
SimNums <- 10000 # When running second order uncertainty, number of markov samples...

Years <- seq(2023,2100,1)

Yrs_to_2070 <- 48


### Different time series can be specified here, along with different 2nd order uncertainty trends.
 ## Just make sure the numbe of data-points are the same across all variables.

# Probability User is System Blind
Prob.SBl <- c(seq.int(from=10,to=40,length.out=Yrs_to_2070),
           seq.int(from=(40 + (30 / Yrs_to_2070)),
                   to=(40 + ((30 / Yrs_to_2070) * 30)),length.out=30))
ProbSD.SBl <- seq.int(from=5,to=20,length.out=length(Years))
# Probability AGI is feasible
Prob.BFe <- c(seq.int(from=0,to=65,length.out=Yrs_to_2070),
          seq.int(from=(65 + (65 / Yrs_to_2070)),
                  to=(65 + ((65 / Yrs_to_2070) * 30)),length.out=30))
ProbSD.BFe <- seq.int(from=5,to=20,length.out=length(Years))
# Probability AGI is designed 
Prob.De <- c(seq.int(from=0,to=65,length.out=Yrs_to_2070),
          seq.int(from=(65 + (65 / Yrs_to_2070)),
                  to=(65 + ((65 / Yrs_to_2070) * 30)),length.out=30))
ProbSD.De <- seq.int(from=5,to=20,length.out=length(Years))
# Probability system properties are enabling for AGI X-Risk action (e.g., connectedness)
Prob.SSu <- c(seq.int(from=2,to=20,length.out=Yrs_to_2070),
           seq.int(from=(20 + (18 / Yrs_to_2070)),
                   to=(20 + ((18 / Yrs_to_2070) * 30)),length.out=30))
ProbSD.SSu <- seq.int(from=5,to=20,length.out=length(Years))
# Probability that an AGI implementer(user) is capable of detecting misaligned goals, subversion of user conOpol, and AI power to act
Prob.Op <- c(seq.int(from=10,to=50,length.out=Yrs_to_2070),
          seq.int(from=(50 + (40 / Yrs_to_2070)),
                  to=(50 + ((40 / Yrs_to_2070) * 30)),length.out=30))
ProbSD.Op <- seq.int(from=5,to=20,length.out=length(Years))

## Conditional probabilities:

# For now conditionals and their uncertainties are assumed to remain constant over time.
# There is no reason why you can't apply time series functions like those above (or others) here as well to both.

# Probability of an implemented AGI having goal misalignment: P(MAG|SBl,De)
Prob.MAG_De.SBl <- rep(80, times=length(Years))
Prob.MAG_De.nSBl <- rep(60, times=length(Years))
Prob.MAG_nDe.SBl <- rep(0, times=length(Years))
Prob.MAG_nDe.nSBl <- rep(0, times=length(Years))

ProbSD.MAG_De.SBl <- rep(10, times=length(Years))
ProbSD.MAG_De.nSBl <- rep(10, times=length(Years))
ProbSD.MAG_nDe.SBl <- rep(0, times=length(Years))
ProbSD.MAG_nDe.nSBl <- rep(0, times=length(Years))

# Probability of AGI being able to subvert control: P(CCo|SBl,De)
Prob.CCo_De.SBl <- rep(80, times=length(Years))
Prob.CCo_De.nSBl <- rep(60, times=length(Years))
Prob.CCo_nDe.SBl <- rep(0, times=length(Years))
Prob.CCo_nDe.nSBl <- rep(0, times=length(Years))

ProbSD.CCo_De.SBl <- rep(10, times=length(Years))
ProbSD.CCo_De.nSBl <- rep(10, times=length(Years))
ProbSD.CCo_nDe.SBl <- rep(0, times=length(Years))
ProbSD.CCo_nDe.nSBl <- rep(0, times=length(Years))

# Probability of AGI being built with required aspects: P(Bu|De,BFe,In)
Prob.Bu_In.BFe.De <- rep(95, times=length(Years))
Prob.Bu_In.BFe.nDe <- rep(0, times=length(Years))
Prob.Bu_In.nBFe.De <- rep(0, times=length(Years))
Prob.Bu_In.nBFe.nDe <- rep(0, times=length(Years))
Prob.Bu_nIn.BFe.De <- rep(50, times=length(Years))
Prob.Bu_nIn.BFe.nDe <- rep(0, times=length(Years))
Prob.Bu_nIn.nBFe.De <- rep(0, times=length(Years))
Prob.Bu_nIn.nBFe.nDe <- rep(0, times=length(Years))

ProbSD.Bu_In.BFe.De <- rep(5, times=length(Years))
ProbSD.Bu_In.BFe.nDe <- rep(0, times=length(Years))
ProbSD.Bu_In.nBFe.De <- rep(0, times=length(Years))
ProbSD.Bu_In.nBFe.nDe <- rep(0, times=length(Years))
ProbSD.Bu_nIn.BFe.De <- rep(10, times=length(Years))
ProbSD.Bu_nIn.BFe.nDe <- rep(0, times=length(Years))
ProbSD.Bu_nIn.nBFe.De <- rep(0, times=length(Years))
ProbSD.Bu_nIn.nBFe.nDe <- rep(0, times=length(Years))

# Probability of AGI having sufficient power to commit X-Risk action: P(Po|De,SSu)
Prob.Po_SSu.De <- rep(100, times=length(Years))
Prob.Po_SSu.nDe <- rep(0, times=length(Years))
Prob.Po_nSSu.De <- rep(0, times=length(Years))
Prob.Po_nSSu.nDe <- rep(0, times=length(Years))

ProbSD.Po_SSu.De <- rep(5, times=length(Years))
ProbSD.Po_SSu.nDe <- rep(0, times=length(Years))
ProbSD.Po_nSSu.De <- rep(0, times=length(Years))
ProbSD.Po_nSSu.nDe <- rep(0, times=length(Years))

# Probability of Incentive: P(In|CCo,Op,Po,MAG)
Prob.In_MAG.Po.OProb.CCo <- rep(80, times=length(Years))
Prob.In_MAG.Po.OProb.nCCo <- rep(80, times=length(Years))
Prob.In_MAG.Po.nOProb.CCo <- rep(1, times=length(Years))
Prob.In_MAG.Po.nOProb.nCCo <- rep(60, times=length(Years))
Prob.In_MAG.nPo.OProb.CCo <- rep(80, times=length(Years))
Prob.In_MAG.nPo.OProb.nCCo <- rep(80, times=length(Years))
Prob.In_MAG.nPo.nOProb.CCo <- rep(1, times=length(Years))
Prob.In_MAG.nPo.nOProb.nCCo <- rep(70, times=length(Years))
Prob.In_nMAG.Po.OProb.CCo <- rep(80, times=length(Years))
Prob.In_nMAG.Po.OProb.nCCo <- rep(80, times=length(Years))
Prob.In_nMAG.Po.nOProb.CCo <- rep(30, times=length(Years))
Prob.In_nMAG.Po.nOProb.nCCo <- rep(70, times=length(Years))
Prob.In_nMAG.nPo.OProb.CCo <- rep(80, times=length(Years))
Prob.In_nMAG.nPo.OProb.nCCo <- rep(80, times=length(Years))
Prob.In_nMAG.nPo.nOProb.CCo <- rep(70, times=length(Years))
Prob.In_nMAG.nPo.nOProb.nCCo <- rep(80, times=length(Years))

ProbSD.In_MAG.Po.OProb.CCo <- rep(10, times=length(Years)) #seq.int(from=3,to=15,length.out=length(Years))
ProbSD.In_MAG.Po.OProb.nCCo <- rep(10, times=length(Years))
ProbSD.In_MAG.Po.nOProb.CCo <- rep(5, times=length(Years))
ProbSD.In_MAG.Po.nOProb.nCCo <- rep(10, times=length(Years))
ProbSD.In_MAG.nPo.OProb.CCo <- rep(10, times=length(Years))
ProbSD.In_MAG.nPo.OProb.nCCo <- rep(10, times=length(Years))
ProbSD.In_MAG.nPo.nOProb.CCo <- rep(5, times=length(Years))
ProbSD.In_MAG.nPo.nOProb.nCCo <- rep(10, times=length(Years))
ProbSD.In_nMAG.Po.OProb.CCo <- rep(10, times=length(Years))
ProbSD.In_nMAG.Po.OProb.nCCo <- rep(10, times=length(Years))
ProbSD.In_nMAG.Po.nOProb.CCo <- rep(10, times=length(Years))
ProbSD.In_nMAG.Po.nOProb.nCCo <- rep(10, times=length(Years))
ProbSD.In_nMAG.nPo.OProb.CCo <- rep(10, times=length(Years))
ProbSD.In_nMAG.nPo.OProb.nCCo <- rep(10, times=length(Years))
ProbSD.In_nMAG.nPo.nOProb.CCo <- rep(10, times=length(Years))
ProbSD.In_nMAG.nPo.nOProb.nCCo <- rep(10, times=length(Years))

# Probability of X-Risk from misaligned AGI: P(AIX|Po,In,Bu,CCo,MAG)
Prob.AIX_MAG.CCo.Bu.In.Po <- rep(95, times=length(Years))
Prob.AIX_MAG.CCo.Bu.In.nPo <- rep(0, times=length(Years))
Prob.AIX_MAG.CCo.Bu.nIn.Po <- rep(10, times=length(Years))
Prob.AIX_MAG.CCo.Bu.nIn.nPo <- rep(0, times=length(Years))
Prob.AIX_MAG.CCo.nBu.In.Po <- rep(0, times=length(Years))
Prob.AIX_MAG.CCo.nBu.In.nPo <- rep(0, times=length(Years))
Prob.AIX_MAG.CCo.nBu.nIn.Po <- rep(0, times=length(Years))
Prob.AIX_MAG.CCo.nBu.nIn.nPo <- rep(0, times=length(Years))
Prob.AIX_MAG.nCCo.Bu.In.Po <- rep(0, times=length(Years))
Prob.AIX_MAG.nCCo.Bu.In.nPo <- rep(0, times=length(Years))
Prob.AIX_MAG.nCCo.Bu.nIn.Po <- rep(0, times=length(Years))
Prob.AIX_MAG.nCCo.Bu.nIn.nPo <- rep(0, times=length(Years))
Prob.AIX_MAG.nCCo.nBu.In.Po <- rep(0, times=length(Years))
Prob.AIX_MAG.nCCo.nBu.In.nPo <- rep(0, times=length(Years))
Prob.AIX_MAG.nCCo.nBu.nIn.Po <- rep(0, times=length(Years))
Prob.AIX_MAG.nCCo.nBu.nIn.nPo <- rep(0, times=length(Years))
Prob.AIX_nMAG.CCo.Bu.In.Po <- rep(0, times=length(Years))
Prob.AIX_nMAG.CCo.Bu.In.nPo <- rep(0, times=length(Years))
Prob.AIX_nMAG.CCo.Bu.nIn.Po <- rep(0, times=length(Years))
Prob.AIX_nMAG.CCo.Bu.nIn.nPo <- rep(0, times=length(Years))
Prob.AIX_nMAG.CCo.nBu.In.Po <- rep(0, times=length(Years))
Prob.AIX_nMAG.CCo.nBu.In.nPo <- rep(0, times=length(Years))
Prob.AIX_nMAG.CCo.nBu.nIn.Po <- rep(0, times=length(Years))
Prob.AIX_nMAG.CCo.nBu.nIn.nPo <- rep(0, times=length(Years))
Prob.AIX_nMAG.nCCo.Bu.In.Po <- rep(0, times=length(Years))
Prob.AIX_nMAG.nCCo.Bu.In.nPo <- rep(0, times=length(Years))
Prob.AIX_nMAG.nCCo.Bu.nIn.Po <- rep(0, times=length(Years))
Prob.AIX_nMAG.nCCo.Bu.nIn.nPo <- rep(0, times=length(Years))
Prob.AIX_nMAG.nCCo.nBu.In.Po <- rep(0, times=length(Years))
Prob.AIX_nMAG.nCCo.nBu.In.nPo <- rep(0, times=length(Years))
Prob.AIX_nMAG.nCCo.nBu.nIn.Po <- rep(0, times=length(Years))
Prob.AIX_nMAG.nCCo.nBu.nIn.nPo <- rep(0, times=length(Years))

ProbSD.AIX_MAG.CCo.Bu.In.Po <- rep(5, times=length(Years))
ProbSD.AIX_MAG.CCo.Bu.In.nPo <- rep(0, times=length(Years))
ProbSD.AIX_MAG.CCo.Bu.nIn.Po <- rep(10, times=length(Years))
ProbSD.AIX_MAG.CCo.Bu.nIn.nPo <- rep(0, times=length(Years))
ProbSD.AIX_MAG.CCo.nBu.In.Po <- rep(0, times=length(Years))
ProbSD.AIX_MAG.CCo.nBu.In.nPo <- rep(0, times=length(Years))
ProbSD.AIX_MAG.CCo.nBu.nIn.Po <- rep(0, times=length(Years))
ProbSD.AIX_MAG.CCo.nBu.nIn.nPo <- rep(0, times=length(Years))
ProbSD.AIX_MAG.nCCo.Bu.In.Po <- rep(0, times=length(Years))
ProbSD.AIX_MAG.nCCo.Bu.In.nPo <- rep(0, times=length(Years))
ProbSD.AIX_MAG.nCCo.Bu.nIn.Po <- rep(0, times=length(Years))
ProbSD.AIX_MAG.nCCo.Bu.nIn.nPo <- rep(0, times=length(Years))
ProbSD.AIX_MAG.nCCo.nBu.In.Po <- rep(0, times=length(Years))
ProbSD.AIX_MAG.nCCo.nBu.In.nPo <- rep(0, times=length(Years))
ProbSD.AIX_MAG.nCCo.nBu.nIn.Po <- rep(0, times=length(Years))
ProbSD.AIX_MAG.nCCo.nBu.nIn.nPo <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.CCo.Bu.In.Po <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.CCo.Bu.In.nPo <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.CCo.Bu.nIn.Po <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.CCo.Bu.nIn.nPo <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.CCo.nBu.In.Po <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.CCo.nBu.In.nPo <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.CCo.nBu.nIn.Po <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.CCo.nBu.nIn.nPo <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.nCCo.Bu.In.Po <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.nCCo.Bu.In.nPo <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.nCCo.Bu.nIn.Po <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.nCCo.Bu.nIn.nPo <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.nCCo.nBu.In.Po <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.nCCo.nBu.In.nPo <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.nCCo.nBu.nIn.Po <- rep(0, times=length(Years))
ProbSD.AIX_nMAG.nCCo.nBu.nIn.nPo <- rep(0, times=length(Years))

#### Step 3: The constructor

## Storage initialization for constructor:
Prob.AIX <- c()
Prob.MAG <- c()
Prob.CCo <- c()
Prob.Bu <- c()
Prob.Po <- c()
Prob.In <- c()
ProbSD.AIX <- c()
ProbSD.MAG <- c()
ProbSD.CCo <- c()
ProbSD.Bu <- c()
ProbSD.Po <- c()
ProbSD.In <- c()

Prob.AIX_Bu <- c()
Prob.MAG_Bu <- c()
Prob.CCo_Bu <- c()
Prob.Po_Bu <- c()
Prob.In_Bu <- c()
ProbSD.AIX_Bu <- c()
ProbSD.MAG_Bu <- c()
ProbSD.CCo_Bu <- c()
ProbSD.Po_Bu <- c()
ProbSD.In_Bu <- c()

Prob.SBl_Bu <- c()
ProbSD.SBl_Bu <- c()
Prob.BFe_Bu <- c()
ProbSD.BFe_Bu <- c()
Prob.De_Bu <- c()
ProbSD.De_Bu <- c()
Prob.SSu_Bu <- c()
ProbSD.SSu_Bu <- c()
Prob.Op_Bu <- c()
ProbSD.Op_Bu <- c()

for(i in 1:length(Years)){
  
  t.Prob.AIX <- c()
  t.Prob.MAG <- c()
  t.Prob.CCo <- c()
  t.Prob.Bu <- c()
  t.Prob.Po <- c()
  t.Prob.In <- c()
  t.Prob.AIX_Bu <- c()
  t.Prob.MAG_Bu <- c()
  t.Prob.CCo_Bu <- c()
  t.Prob.Po_Bu <- c()
  t.Prob.In_Bu <- c()
  t.Prob.SBl_Bu <- c()
  t.Prob.BFe_Bu <- c()
  t.Prob.De_Bu <- c()
  t.Prob.SSu_Bu <- c()
  t.Prob.Op_Bu <- c()

  for (x in 1:SimNums){ # Simulations for accounting for uncertainty

    if (SecondOrdProb_YN == "True"){
      
      t.Prob.SBl <- rtruncnorm(1,0,100,Prob.SBl[i],ProbSD.SBl[i])
      if(ProbSD.SBl[i]==0){t.Prob.SBl <- Prob.SBl[i]}# catch for no variance/uncertainty specified
      # Probability AGI is feasible
      t.Prob.BFe <- rtruncnorm(1,0,100,Prob.BFe[i],ProbSD.BFe[i])
      if(ProbSD.BFe[i]==0){t.Prob.BFe <- Prob.BFe[i]}# catch for no variance/uncertainty specified
      # Probability AGI is designed
      t.Prob.De <- rtruncnorm(1,0,100,Prob.De[i],ProbSD.De[i])
      if(ProbSD.De[i]==0){t.Prob.De <- Prob.De[i]}# catch for no variance/uncertainty specified
      # Probability system properties are enabling for AGI X-Risk action (e.g., connectedness[i])
      t.Prob.SSu <- rtruncnorm(1,0,100,Prob.SSu[i],ProbSD.SSu[i])
      if(ProbSD.SSu[i]==0){t.Prob.SSu <- Prob.SSu[i]}# catch for no variance/uncertainty specified
      # Probability that an AGI implementer(user[i]) is capable of detecting misaligned goals, subversion of user control, and AGI power to act
      t.Prob.Op <- rtruncnorm(1,0,100,Prob.Op[i],ProbSD.Op[i])
      if(ProbSD.Op[i]==0){t.Prob.Op <- Prob.Op[i]}# catch for no variance/uncertainty specified
      
      # Probability of AGI goal misalignment: (P(MAG|SBl,Ca)[i])
      t.Prob.MAG_De.SBl <- rtruncnorm(1,0,100,Prob.MAG_De.SBl[i],ProbSD.MAG_De.SBl[i])
      if(ProbSD.MAG_De.SBl[i]==0){t.Prob.MAG_De.SBl <- Prob.MAG_De.SBl[i]}# catch for no variance/uncertainty specified
      t.Prob.MAG_De.nSBl <- rtruncnorm(1,0,100,Prob.MAG_De.nSBl[i],ProbSD.MAG_De.nSBl[i])
      if(ProbSD.MAG_De.nSBl[i]==0){t.Prob.MAG_De.nSBl <- Prob.MAG_De.nSBl[i]}# catch for no variance/uncertainty specified
      t.Prob.MAG_nDe.SBl <- rtruncnorm(1,0,100,Prob.MAG_nDe.SBl[i],ProbSD.MAG_nDe.SBl[i])
      if(ProbSD.MAG_nDe.SBl[i]==0){t.Prob.MAG_nDe.SBl <- Prob.MAG_nDe.SBl[i]}# catch for no variance/uncertainty specified
      t.Prob.MAG_nDe.nSBl <- rtruncnorm(1,0,100,Prob.MAG_nDe.nSBl[i],ProbSD.MAG_nDe.nSBl[i])
      if(ProbSD.MAG_nDe.nSBl[i]==0){t.Prob.MAG_nDe.nSBl <- Prob.MAG_nDe.nSBl[i]}# catch for no variance/uncertainty specified
      
      # Probability of AGI being able to subvert control: (P(CCo|SBl,De)[i])
      t.Prob.CCo_De.SBl <- rtruncnorm(1,0,100,Prob.CCo_De.SBl[i],ProbSD.CCo_De.SBl[i])
      if(ProbSD.CCo_De.SBl[i]==0){t.Prob.CCo_De.SBl <- Prob.CCo_De.SBl[i]}# catch for no variance/uncertainty specified
      t.Prob.CCo_De.nSBl <- rtruncnorm(1,0,100,Prob.CCo_De.nSBl[i],ProbSD.CCo_De.nSBl[i])
      if(ProbSD.CCo_De.nSBl[i]==0){t.Prob.CCo_De.nSBl <- Prob.CCo_De.nSBl[i]}# catch for no variance/uncertainty specified
      t.Prob.CCo_nDe.SBl <- rtruncnorm(1,0,100,Prob.CCo_nDe.SBl[i],ProbSD.CCo_nDe.SBl[i])
      if(ProbSD.CCo_nDe.SBl[i]==0){t.Prob.CCo_nDe.SBl <- Prob.CCo_nDe.SBl[i]}# catch for no variance/uncertainty specified
      t.Prob.CCo_nDe.nSBl <- rtruncnorm(1,0,100,Prob.CCo_nDe.nSBl[i],ProbSD.CCo_nDe.nSBl[i])
      if(ProbSD.CCo_nDe.nSBl[i]==0){t.Prob.CCo_nDe.nSBl <- Prob.CCo_nDe.nSBl[i]}# catch for no variance/uncertainty specified
      
      # Probability of AGI being built with required aspects: (P(Bu|De,BFe,In)[i])
      t.Prob.Bu_In.BFe.De <- rtruncnorm(1,0,100,Prob.Bu_In.BFe.De[i],ProbSD.Bu_In.BFe.De[i])
      if(ProbSD.Bu_In.BFe.De[i]==0){t.Prob.Bu_In.BFe.De <- Prob.Bu_In.BFe.De[i]}# catch for no variance/uncertainty specified
      t.Prob.Bu_In.BFe.nDe <- rtruncnorm(1,0,100,Prob.Bu_In.BFe.nDe[i],ProbSD.Bu_In.BFe.nDe[i])
      if(ProbSD.Bu_In.BFe.nDe[i]==0){t.Prob.Bu_In.BFe.nDe <- Prob.Bu_In.BFe.nDe[i]}# catch for no variance/uncertainty specified
      t.Prob.Bu_In.nBFe.De <- rtruncnorm(1,0,100,Prob.Bu_In.nBFe.De[i],ProbSD.Bu_In.nBFe.De[i])
      if(ProbSD.Bu_In.nBFe.De[i]==0){t.Prob.Bu_In.nBFe.De <- Prob.Bu_In.nBFe.De[i]}# catch for no variance/uncertainty specified
      t.Prob.Bu_In.nBFe.nDe <- rtruncnorm(1,0,100,Prob.Bu_In.nBFe.nDe[i],ProbSD.Bu_In.nBFe.nDe[i])
      if(ProbSD.Bu_In.nBFe.nDe[i]==0){t.Prob.Bu_In.nBFe.nDe <- Prob.Bu_In.nBFe.nDe[i]}# catch for no variance/uncertainty specified
      t.Prob.Bu_nIn.BFe.De <- rtruncnorm(1,0,100,Prob.Bu_nIn.BFe.De[i],ProbSD.Bu_nIn.BFe.De[i])
      if(ProbSD.Bu_nIn.BFe.De[i]==0){t.Prob.Bu_nIn.BFe.De <- Prob.Bu_nIn.BFe.De[i]}# catch for no variance/uncertainty specified
      t.Prob.Bu_nIn.BFe.nDe <- rtruncnorm(1,0,100,Prob.Bu_nIn.BFe.nDe[i],ProbSD.Bu_nIn.BFe.nDe[i])
      if(ProbSD.Bu_nIn.BFe.nDe[i]==0){t.Prob.Bu_nIn.BFe.nDe <- Prob.Bu_nIn.BFe.nDe[i]}# catch for no variance/uncertainty specified
      t.Prob.Bu_nIn.nBFe.De <- rtruncnorm(1,0,100,Prob.Bu_nIn.nBFe.De[i],ProbSD.Bu_nIn.nBFe.De[i])
      if(ProbSD.Bu_nIn.nBFe.De[i]==0){t.Prob.Bu_nIn.nBFe.De <- Prob.Bu_nIn.nBFe.De[i]}# catch for no variance/uncertainty specified
      t.Prob.Bu_nIn.nBFe.nDe <- rtruncnorm(1,0,100,Prob.Bu_nIn.nBFe.nDe[i],ProbSD.Bu_nIn.nBFe.nDe[i])
      if(ProbSD.Bu_nIn.nBFe.nDe[i]==0){t.Prob.Bu_nIn.nBFe.nDe <- Prob.Bu_nIn.nBFe.nDe[i]}# catch for no variance/uncertainty specified
      
      # Probability of AGI having sufficient power to commit X-Risk action: (P(Po|De,SSu)[i])
      t.Prob.Po_SSu.De <- rtruncnorm(1,0,100,Prob.Po_SSu.De[i],ProbSD.Po_SSu.De[i])
      if(ProbSD.Po_SSu.De[i]==0){t.Prob.Po_SSu.De <- Prob.Po_SSu.De[i]}# catch for no variance/uncertainty specified
      t.Prob.Po_SSu.nDe <- rtruncnorm(1,0,100,Prob.Po_SSu.nDe[i],ProbSD.Po_SSu.nDe[i])
      if(ProbSD.Po_SSu.nDe[i]==0){t.Prob.Po_SSu.nDe <- Prob.Po_SSu.nDe[i]}# catch for no variance/uncertainty specified
      t.Prob.Po_nSSu.De <- rtruncnorm(1,0,100,Prob.Po_nSSu.De[i],ProbSD.Po_nSSu.De[i])
      if(ProbSD.Po_nSSu.De[i]==0){t.Prob.Po_nSSu.De <- Prob.Po_nSSu.De[i]}# catch for no variance/uncertainty specified
      t.Prob.Po_nSSu.nDe <- rtruncnorm(1,0,100,Prob.Po_nSSu.nDe[i],ProbSD.Po_nSSu.nDe[i])
      if(ProbSD.Po_nSSu.nDe[i]==0){t.Prob.Po_nSSu.nDe <- Prob.Po_nSSu.nDe[i]}# catch for no variance/uncertainty specified
      
      # Probability of AGI goal misalignment: (P(In|CCo,Tr,Po,MAG)[i])
      t.Prob.In_MAG.Po.OProb.CCo <- rtruncnorm(1,0,100,Prob.In_MAG.Po.OProb.CCo[i],ProbSD.In_MAG.Po.OProb.CCo[i])
      if(ProbSD.In_MAG.Po.OProb.CCo[i]==0){t.Prob.In_MAG.Po.OProb.CCo <- Prob.In_MAG.Po.OProb.CCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_MAG.Po.OProb.nCCo <- rtruncnorm(1,0,100,Prob.In_MAG.Po.OProb.nCCo[i],ProbSD.In_MAG.Po.OProb.nCCo[i])
      if(ProbSD.In_MAG.Po.OProb.nCCo[i]==0){t.Prob.In_MAG.Po.OProb.nCCo <- Prob.In_MAG.Po.OProb.nCCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_MAG.Po.nOProb.CCo <- rtruncnorm(1,0,100,Prob.In_MAG.Po.nOProb.CCo[i],ProbSD.In_MAG.Po.nOProb.CCo[i])
      if(ProbSD.In_MAG.Po.nOProb.CCo[i]==0){t.Prob.In_MAG.Po.nOProb.CCo <- Prob.In_MAG.Po.nOProb.CCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_MAG.Po.nOProb.nCCo <- rtruncnorm(1,0,100,Prob.In_MAG.Po.nOProb.nCCo[i],ProbSD.In_MAG.Po.nOProb.nCCo[i])
      if(ProbSD.In_MAG.Po.nOProb.nCCo[i]==0){t.Prob.In_MAG.Po.nOProb.nCCo <- Prob.In_MAG.Po.nOProb.nCCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_MAG.nPo.OProb.CCo <- rtruncnorm(1,0,100,Prob.In_MAG.nPo.OProb.CCo[i],ProbSD.In_MAG.nPo.OProb.CCo[i])
      if(ProbSD.In_MAG.nPo.OProb.CCo[i]==0){t.Prob.In_MAG.nPo.OProb.CCo <- Prob.In_MAG.nPo.OProb.CCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_MAG.nPo.OProb.nCCo <- rtruncnorm(1,0,100,Prob.In_MAG.nPo.OProb.nCCo[i],ProbSD.In_MAG.nPo.OProb.nCCo[i])
      if(ProbSD.In_MAG.nPo.OProb.nCCo[i]==0){t.Prob.In_MAG.nPo.OProb.nCCo <- Prob.In_MAG.nPo.OProb.nCCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_MAG.nPo.nOProb.CCo <- rtruncnorm(1,0,100,Prob.In_MAG.nPo.nOProb.CCo[i],ProbSD.In_MAG.nPo.nOProb.CCo[i])
      if(ProbSD.In_MAG.nPo.nOProb.CCo[i]==0){t.Prob.In_MAG.nPo.nOProb.CCo <- Prob.In_MAG.nPo.nOProb.CCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_MAG.nPo.nOProb.nCCo <- rtruncnorm(1,0,100,Prob.In_MAG.nPo.nOProb.nCCo[i],ProbSD.In_MAG.nPo.nOProb.nCCo[i])
      if(ProbSD.In_MAG.nPo.nOProb.nCCo[i]==0){t.Prob.In_MAG.nPo.nOProb.nCCo <- Prob.In_MAG.nPo.nOProb.nCCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_nMAG.Po.OProb.CCo <- rtruncnorm(1,0,100,Prob.In_nMAG.Po.OProb.CCo[i],ProbSD.In_nMAG.Po.OProb.CCo[i])
      if(ProbSD.In_nMAG.Po.OProb.CCo[i]==0){t.Prob.In_nMAG.Po.OProb.CCo <- Prob.In_nMAG.Po.OProb.CCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_nMAG.Po.OProb.nCCo <- rtruncnorm(1,0,100,Prob.In_nMAG.Po.OProb.nCCo[i],ProbSD.In_nMAG.Po.OProb.nCCo[i])
      if(ProbSD.In_nMAG.Po.OProb.nCCo[i]==0){t.Prob.In_nMAG.Po.OProb.nCCo <- Prob.In_nMAG.Po.OProb.nCCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_nMAG.Po.nOProb.CCo <- rtruncnorm(1,0,100,Prob.In_nMAG.Po.nOProb.CCo[i],ProbSD.In_nMAG.Po.nOProb.CCo[i])
      if(ProbSD.In_nMAG.Po.nOProb.CCo[i]==0){t.Prob.In_nMAG.Po.nOProb.CCo <- Prob.In_nMAG.Po.nOProb.CCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_nMAG.Po.nOProb.nCCo <- rtruncnorm(1,0,100,Prob.In_nMAG.Po.nOProb.nCCo[i],ProbSD.In_nMAG.Po.nOProb.nCCo[i])
      if(ProbSD.In_nMAG.Po.nOProb.nCCo[i]==0){t.Prob.In_nMAG.Po.nOProb.nCCo <- Prob.In_nMAG.Po.nOProb.nCCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_nMAG.nPo.OProb.CCo <- rtruncnorm(1,0,100,Prob.In_nMAG.nPo.OProb.CCo[i],ProbSD.In_nMAG.nPo.OProb.CCo[i])
      if(ProbSD.In_nMAG.nPo.OProb.CCo[i]==0){t.Prob.In_nMAG.nPo.OProb.CCo <- Prob.In_nMAG.nPo.OProb.CCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_nMAG.nPo.OProb.nCCo <- rtruncnorm(1,0,100,Prob.In_nMAG.nPo.OProb.nCCo[i],ProbSD.In_nMAG.nPo.OProb.nCCo[i])
      if(ProbSD.In_nMAG.nPo.OProb.nCCo[i]==0){t.Prob.In_nMAG.nPo.OProb.nCCo <- Prob.In_nMAG.nPo.OProb.nCCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_nMAG.nPo.nOProb.CCo <- rtruncnorm(1,0,100,Prob.In_nMAG.nPo.nOProb.CCo[i],ProbSD.In_nMAG.nPo.nOProb.CCo[i])
      if(ProbSD.In_nMAG.nPo.nOProb.CCo[i]==0){t.Prob.In_nMAG.nPo.nOProb.CCo <- Prob.In_nMAG.nPo.nOProb.CCo[i]}# catch for no variance/uncertainty specified
      t.Prob.In_nMAG.nPo.nOProb.nCCo <- rtruncnorm(1,0,100,Prob.In_nMAG.nPo.nOProb.nCCo[i],ProbSD.In_nMAG.nPo.nOProb.nCCo[i])
      if(ProbSD.In_nMAG.nPo.nOProb.nCCo[i]==0){t.Prob.In_nMAG.nPo.nOProb.nCCo <- Prob.In_nMAG.nPo.nOProb.nCCo[i]}# catch for no variance/uncertainty specified
      
      # Probability of X-Risk from AGI: (P(AIX|Po,In,Bu,CCo,MAG)[i])
      t.Prob.AIX_MAG.CCo.Bu.In.Po <- rtruncnorm(1,0,100,Prob.AIX_MAG.CCo.Bu.In.Po[i],ProbSD.AIX_MAG.CCo.Bu.In.Po[i])
      if(ProbSD.AIX_MAG.CCo.Bu.In.Po[i]==0){t.Prob.AIX_MAG.CCo.Bu.In.Po <- Prob.AIX_MAG.CCo.Bu.In.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.CCo.Bu.In.nPo <- rtruncnorm(1,0,100,Prob.AIX_MAG.CCo.Bu.In.nPo[i],ProbSD.AIX_MAG.CCo.Bu.In.nPo[i])
      if(ProbSD.AIX_MAG.CCo.Bu.In.nPo[i]==0){t.Prob.AIX_MAG.CCo.Bu.In.nPo <- Prob.AIX_MAG.CCo.Bu.In.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.CCo.Bu.nIn.Po <- rtruncnorm(1,0,100,Prob.AIX_MAG.CCo.Bu.nIn.Po[i],ProbSD.AIX_MAG.CCo.Bu.nIn.Po[i])
      if(ProbSD.AIX_MAG.CCo.Bu.nIn.Po[i]==0){t.Prob.AIX_MAG.CCo.Bu.nIn.Po <- Prob.AIX_MAG.CCo.Bu.nIn.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.CCo.Bu.nIn.nPo <- rtruncnorm(1,0,100,Prob.AIX_MAG.CCo.Bu.nIn.nPo[i],ProbSD.AIX_MAG.CCo.Bu.nIn.nPo[i])
      if(ProbSD.AIX_MAG.CCo.Bu.nIn.nPo[i]==0){t.Prob.AIX_MAG.CCo.Bu.nIn.nPo <- Prob.AIX_MAG.CCo.Bu.nIn.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.CCo.nBu.In.Po <- rtruncnorm(1,0,100,Prob.AIX_MAG.CCo.nBu.In.Po[i],ProbSD.AIX_MAG.CCo.nBu.In.Po[i])
      if(ProbSD.AIX_MAG.CCo.nBu.In.Po[i]==0){t.Prob.AIX_MAG.CCo.nBu.In.Po <- Prob.AIX_MAG.CCo.nBu.In.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.CCo.nBu.In.nPo <- rtruncnorm(1,0,100,Prob.AIX_MAG.CCo.nBu.In.nPo[i],ProbSD.AIX_MAG.CCo.nBu.In.nPo[i])
      if(ProbSD.AIX_MAG.CCo.nBu.In.nPo[i]==0){t.Prob.AIX_MAG.CCo.nBu.In.nPo <- Prob.AIX_MAG.CCo.nBu.In.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.CCo.nBu.nIn.Po <- rtruncnorm(1,0,100,Prob.AIX_MAG.CCo.nBu.nIn.Po[i],ProbSD.AIX_MAG.CCo.nBu.nIn.Po[i])
      if(ProbSD.AIX_MAG.CCo.nBu.nIn.Po[i]==0){t.Prob.AIX_MAG.CCo.nBu.nIn.Po <- Prob.AIX_MAG.CCo.nBu.nIn.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.CCo.nBu.nIn.nPo <- rtruncnorm(1,0,100,Prob.AIX_MAG.CCo.nBu.nIn.nPo[i],ProbSD.AIX_MAG.CCo.nBu.nIn.nPo[i])
      if(ProbSD.AIX_MAG.CCo.nBu.nIn.nPo[i]==0){t.Prob.AIX_MAG.CCo.nBu.nIn.nPo <- Prob.AIX_MAG.CCo.nBu.nIn.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.nCCo.Bu.In.Po <- rtruncnorm(1,0,100,Prob.AIX_MAG.nCCo.Bu.In.Po[i],ProbSD.AIX_MAG.nCCo.Bu.In.Po[i])
      if(ProbSD.AIX_MAG.nCCo.Bu.In.Po[i]==0){t.Prob.AIX_MAG.nCCo.Bu.In.Po <- Prob.AIX_MAG.nCCo.Bu.In.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.nCCo.Bu.In.nPo <- rtruncnorm(1,0,100,Prob.AIX_MAG.nCCo.Bu.In.nPo[i],ProbSD.AIX_MAG.nCCo.Bu.In.nPo[i])
      if(ProbSD.AIX_MAG.nCCo.Bu.In.nPo[i]==0){t.Prob.AIX_MAG.nCCo.Bu.In.nPo <- Prob.AIX_MAG.nCCo.Bu.In.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.nCCo.Bu.nIn.Po <- rtruncnorm(1,0,100,Prob.AIX_MAG.nCCo.Bu.nIn.Po[i],ProbSD.AIX_MAG.nCCo.Bu.nIn.Po[i])
      if(ProbSD.AIX_MAG.nCCo.Bu.nIn.Po[i]==0){t.Prob.AIX_MAG.nCCo.Bu.nIn.Po <- Prob.AIX_MAG.nCCo.Bu.nIn.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.nCCo.Bu.nIn.nPo <- rtruncnorm(1,0,100,Prob.AIX_MAG.nCCo.Bu.nIn.nPo[i],ProbSD.AIX_MAG.nCCo.Bu.nIn.nPo[i])
      if(ProbSD.AIX_MAG.nCCo.Bu.nIn.nPo[i]==0){t.Prob.AIX_MAG.nCCo.Bu.nIn.nPo <- Prob.AIX_MAG.nCCo.Bu.nIn.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.nCCo.nBu.In.Po <- rtruncnorm(1,0,100,Prob.AIX_MAG.nCCo.nBu.In.Po[i],ProbSD.AIX_MAG.nCCo.nBu.In.Po[i])
      if(ProbSD.AIX_MAG.nCCo.nBu.In.Po[i]==0){t.Prob.AIX_MAG.nCCo.nBu.In.Po <- Prob.AIX_MAG.nCCo.nBu.In.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.nCCo.nBu.In.nPo <- rtruncnorm(1,0,100,Prob.AIX_MAG.nCCo.nBu.In.nPo[i],ProbSD.AIX_MAG.nCCo.nBu.In.nPo[i])
      if(ProbSD.AIX_MAG.nCCo.nBu.In.nPo[i]==0){t.Prob.AIX_MAG.nCCo.nBu.In.nPo <- Prob.AIX_MAG.nCCo.nBu.In.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.nCCo.nBu.nIn.Po <- rtruncnorm(1,0,100,Prob.AIX_MAG.nCCo.nBu.nIn.Po[i],ProbSD.AIX_MAG.nCCo.nBu.nIn.Po[i])
      if(ProbSD.AIX_MAG.nCCo.nBu.nIn.Po[i]==0){t.Prob.AIX_MAG.nCCo.nBu.nIn.Po <- Prob.AIX_MAG.nCCo.nBu.nIn.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_MAG.nCCo.nBu.nIn.nPo <- rtruncnorm(1,0,100,Prob.AIX_MAG.nCCo.nBu.nIn.nPo[i],ProbSD.AIX_MAG.nCCo.nBu.nIn.nPo[i])
      if(ProbSD.AIX_MAG.nCCo.nBu.nIn.nPo[i]==0){t.Prob.AIX_MAG.nCCo.nBu.nIn.nPo <- Prob.AIX_MAG.nCCo.nBu.nIn.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.CCo.Bu.In.Po <- rtruncnorm(1,0,100,Prob.AIX_nMAG.CCo.Bu.In.Po[i],ProbSD.AIX_nMAG.CCo.Bu.In.Po[i])
      if(ProbSD.AIX_nMAG.CCo.Bu.In.Po[i]==0){t.Prob.AIX_nMAG.CCo.Bu.In.Po <- Prob.AIX_nMAG.CCo.Bu.In.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.CCo.Bu.In.nPo <- rtruncnorm(1,0,100,Prob.AIX_nMAG.CCo.Bu.In.nPo[i],ProbSD.AIX_nMAG.CCo.Bu.In.nPo[i])
      if(ProbSD.AIX_nMAG.CCo.Bu.In.nPo[i]==0){t.Prob.AIX_nMAG.CCo.Bu.In.nPo <- Prob.AIX_nMAG.CCo.Bu.In.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.CCo.Bu.nIn.Po <- rtruncnorm(1,0,100,Prob.AIX_nMAG.CCo.Bu.nIn.Po[i],ProbSD.AIX_nMAG.CCo.Bu.nIn.Po[i])
      if(ProbSD.AIX_nMAG.CCo.Bu.nIn.Po[i]==0){t.Prob.AIX_nMAG.CCo.Bu.nIn.Po <- Prob.AIX_nMAG.CCo.Bu.nIn.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.CCo.Bu.nIn.nPo <- rtruncnorm(1,0,100,Prob.AIX_nMAG.CCo.Bu.nIn.nPo[i],ProbSD.AIX_nMAG.CCo.Bu.nIn.nPo[i])
      if(ProbSD.AIX_nMAG.CCo.Bu.nIn.nPo[i]==0){t.Prob.AIX_nMAG.CCo.Bu.nIn.nPo <- Prob.AIX_nMAG.CCo.Bu.nIn.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.CCo.nBu.In.Po <- rtruncnorm(1,0,100,Prob.AIX_nMAG.CCo.nBu.In.Po[i],ProbSD.AIX_nMAG.CCo.nBu.In.Po[i])
      if(ProbSD.AIX_nMAG.CCo.nBu.In.Po[i]==0){t.Prob.AIX_nMAG.CCo.nBu.In.Po <- Prob.AIX_nMAG.CCo.nBu.In.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.CCo.nBu.In.nPo <- rtruncnorm(1,0,100,Prob.AIX_nMAG.CCo.nBu.In.nPo[i],ProbSD.AIX_nMAG.CCo.nBu.In.nPo[i])
      if(ProbSD.AIX_nMAG.CCo.nBu.In.nPo[i]==0){t.Prob.AIX_nMAG.CCo.nBu.In.nPo <- Prob.AIX_nMAG.CCo.nBu.In.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.CCo.nBu.nIn.Po <- rtruncnorm(1,0,100,Prob.AIX_nMAG.CCo.nBu.nIn.Po[i],ProbSD.AIX_nMAG.CCo.nBu.nIn.Po[i])
      if(ProbSD.AIX_nMAG.CCo.nBu.nIn.Po[i]==0){t.Prob.AIX_nMAG.CCo.nBu.nIn.Po <- Prob.AIX_nMAG.CCo.nBu.nIn.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.CCo.nBu.nIn.nPo <- rtruncnorm(1,0,100,Prob.AIX_nMAG.CCo.nBu.nIn.nPo[i],ProbSD.AIX_nMAG.CCo.nBu.nIn.nPo[i])
      if(ProbSD.AIX_nMAG.CCo.nBu.nIn.nPo[i]==0){t.Prob.AIX_nMAG.CCo.nBu.nIn.nPo <- Prob.AIX_nMAG.CCo.nBu.nIn.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.nCCo.Bu.In.Po <- rtruncnorm(1,0,100,Prob.AIX_nMAG.nCCo.Bu.In.Po[i],ProbSD.AIX_nMAG.nCCo.Bu.In.Po[i])
      if(ProbSD.AIX_nMAG.nCCo.Bu.In.Po[i]==0){t.Prob.AIX_nMAG.nCCo.Bu.In.Po <- Prob.AIX_nMAG.nCCo.Bu.In.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.nCCo.Bu.In.nPo <- rtruncnorm(1,0,100,Prob.AIX_nMAG.nCCo.Bu.In.nPo[i],ProbSD.AIX_nMAG.nCCo.Bu.In.nPo[i])
      if(ProbSD.AIX_nMAG.nCCo.Bu.In.nPo[i]==0){t.Prob.AIX_nMAG.nCCo.Bu.In.nPo <- Prob.AIX_nMAG.nCCo.Bu.In.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.nCCo.Bu.nIn.Po <- rtruncnorm(1,0,100,Prob.AIX_nMAG.nCCo.Bu.nIn.Po[i],ProbSD.AIX_nMAG.nCCo.Bu.nIn.Po[i])
      if(ProbSD.AIX_nMAG.nCCo.Bu.nIn.Po[i]==0){t.Prob.AIX_nMAG.nCCo.Bu.nIn.Po <- Prob.AIX_nMAG.nCCo.Bu.nIn.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.nCCo.Bu.nIn.nPo <- rtruncnorm(1,0,100,Prob.AIX_nMAG.nCCo.Bu.nIn.nPo[i],ProbSD.AIX_nMAG.nCCo.Bu.nIn.nPo[i])
      if(ProbSD.AIX_nMAG.nCCo.Bu.nIn.nPo[i]==0){t.Prob.AIX_nMAG.nCCo.Bu.nIn.nPo <- Prob.AIX_nMAG.nCCo.Bu.nIn.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.nCCo.nBu.In.Po <- rtruncnorm(1,0,100,Prob.AIX_nMAG.nCCo.nBu.In.Po[i],ProbSD.AIX_nMAG.nCCo.nBu.In.Po[i])
      if(ProbSD.AIX_nMAG.nCCo.nBu.In.Po[i]==0){t.Prob.AIX_nMAG.nCCo.nBu.In.Po <- Prob.AIX_nMAG.nCCo.nBu.In.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.nCCo.nBu.In.nPo <- rtruncnorm(1,0,100,Prob.AIX_nMAG.nCCo.nBu.In.nPo[i],ProbSD.AIX_nMAG.nCCo.nBu.In.nPo[i])
      if(ProbSD.AIX_nMAG.nCCo.nBu.In.nPo[i]==0){t.Prob.AIX_nMAG.nCCo.nBu.In.nPo <- Prob.AIX_nMAG.nCCo.nBu.In.nPo[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.nCCo.nBu.nIn.Po <- rtruncnorm(1,0,100,Prob.AIX_nMAG.nCCo.nBu.nIn.Po[i],ProbSD.AIX_nMAG.nCCo.nBu.nIn.Po[i])
      if(ProbSD.AIX_nMAG.nCCo.nBu.nIn.Po[i]==0){t.Prob.AIX_nMAG.nCCo.nBu.nIn.Po <- Prob.AIX_nMAG.nCCo.nBu.nIn.Po[i]}# catch for no variance/uncertainty specified
      t.Prob.AIX_nMAG.nCCo.nBu.nIn.nPo <- rtruncnorm(1,0,100,Prob.AIX_nMAG.nCCo.nBu.nIn.nPo[i],ProbSD.AIX_nMAG.nCCo.nBu.nIn.nPo[i])
      if(ProbSD.AIX_nMAG.nCCo.nBu.nIn.nPo[i]==0){t.Prob.AIX_nMAG.nCCo.nBu.nIn.nPo <- Prob.AIX_nMAG.nCCo.nBu.nIn.nPo[i]}# catch for no variance/uncertainty specified
      
    }else{ # No need to sample parameters
      t.Prob.SBl <- Prob.SBl[i]
      # Probability AGI is feasible
      t.Prob.BFe <- Prob.BFe[i]
      # Probability AGI is designed
      t.Prob.De <- Prob.De[i]
      # Probability system properties are enabling for AGI X-Risk action (e.g., connectedness)
      t.Prob.SSu <- Prob.SSu[i]
      # Probability that an AGI implementer(user) is capable of detecting misaligned goals, subversion of user control, and AGI power to act
      t.Prob.Op <- Prob.Op[i]
      
      # Probability of AGI goal misalignment: (P(MAG|SBl,De)[i])
      t.Prob.MAG_De.SBl <- Prob.MAG_De.SBl[i]
      t.Prob.MAG_De.nSBl <- Prob.MAG_De.nSBl[i]
      t.Prob.MAG_nDe.SBl <- Prob.MAG_nDe.SBl[i]
      t.Prob.MAG_nDe.nSBl <- Prob.MAG_nDe.nSBl[i]
      
      # Probability of AGI being able to subvert control: (P(CCo|SBl,De)[i])
      t.Prob.CCo_De.SBl <- Prob.CCo_De.SBl[i]
      t.Prob.CCo_De.nSBl <- Prob.CCo_De.nSBl[i]
      t.Prob.CCo_nDe.SBl <- Prob.CCo_nDe.SBl[i]
      t.Prob.CCo_nDe.nSBl <- Prob.CCo_nDe.nSBl[i]

      # Probability of AGI being built with required aspects: (P(Bu|De,BFe,In)[i])
      t.Prob.Bu_In.BFe.De <- Prob.Bu_In.BFe.De[i]
      t.Prob.Bu_In.BFe.nDe <- Prob.Bu_In.BFe.nDe[i]
      t.Prob.Bu_In.nBFe.De <- Prob.Bu_In.nBFe.De[i]
      t.Prob.Bu_In.nBFe.nDe <- Prob.Bu_In.nBFe.nDe[i]
      t.Prob.Bu_nIn.BFe.De <- Prob.Bu_nIn.BFe.De[i]
      t.Prob.Bu_nIn.BFe.nDe <- Prob.Bu_nIn.BFe.nDe[i]
      t.Prob.Bu_nIn.nBFe.De <- Prob.Bu_nIn.nBFe.De[i]
      t.Prob.Bu_nIn.nBFe.nDe <- Prob.Bu_nIn.nBFe.nDe[i]
      
      # Probability of AGI having sufficient power to commit X-Risk action: (P(Po|De,SSu)[i])
      t.Prob.Po_SSu.De <- Prob.Po_SSu.De[i]
      t.Prob.Po_SSu.nDe <- Prob.Po_SSu.nDe[i]
      t.Prob.Po_nSSu.De <- Prob.Po_nSSu.De[i]
      t.Prob.Po_nSSu.nDe <- Prob.Po_nSSu.nDe[i]
      
      # Probability of AGI goal misalignment: (P(In|CCo,Tr,Po,MAG)[i])
      t.Prob.In_MAG.Po.OProb.CCo <- Prob.In_MAG.Po.OProb.CCo[i]
      t.Prob.In_MAG.Po.OProb.nCCo <- Prob.In_MAG.Po.OProb.nCCo[i]
      t.Prob.In_MAG.Po.nOProb.CCo <- Prob.In_MAG.Po.nOProb.CCo[i]
      t.Prob.In_MAG.Po.nOProb.nCCo <- Prob.In_MAG.Po.nOProb.nCCo[i]
      t.Prob.In_MAG.nPo.OProb.CCo <- Prob.In_MAG.nPo.OProb.CCo[i]
      t.Prob.In_MAG.nPo.OProb.nCCo <- Prob.In_MAG.nPo.OProb.nCCo[i]
      t.Prob.In_MAG.nPo.nOProb.CCo <- Prob.In_MAG.nPo.nOProb.CCo[i]
      t.Prob.In_MAG.nPo.nOProb.nCCo <- Prob.In_MAG.nPo.nOProb.nCCo[i]
      t.Prob.In_nMAG.Po.OProb.CCo <- Prob.In_nMAG.Po.OProb.CCo[i]
      t.Prob.In_nMAG.Po.OProb.nCCo <- Prob.In_nMAG.Po.OProb.nCCo[i]
      t.Prob.In_nMAG.Po.nOProb.CCo <- Prob.In_nMAG.Po.nOProb.CCo[i]
      t.Prob.In_nMAG.Po.nOProb.nCCo <- Prob.In_nMAG.Po.nOProb.nCCo[i]
      t.Prob.In_nMAG.nPo.OProb.CCo <- Prob.In_nMAG.nPo.OProb.CCo[i]
      t.Prob.In_nMAG.nPo.OProb.nCCo <- Prob.In_nMAG.nPo.OProb.nCCo[i]
      t.Prob.In_nMAG.nPo.nOProb.CCo <- Prob.In_nMAG.nPo.nOProb.CCo[i]
      t.Prob.In_nMAG.nPo.nOProb.nCCo <- Prob.In_nMAG.nPo.nOProb.nCCo[i]
      
      # Probability of X-Risk from AGI: (P(AIX|Po,In,Bu,CCo,MAG)[i])
      t.Prob.AIX_MAG.CCo.Bu.In.Po <- Prob.AIX_MAG.CCo.Bu.In.Po[i]
      t.Prob.AIX_MAG.CCo.Bu.In.nPo <- Prob.AIX_MAG.CCo.Bu.In.nPo[i]
      t.Prob.AIX_MAG.CCo.Bu.nIn.Po <- Prob.AIX_MAG.CCo.Bu.nIn.Po[i]
      t.Prob.AIX_MAG.CCo.Bu.nIn.nPo <- Prob.AIX_MAG.CCo.Bu.nIn.nPo[i]
      t.Prob.AIX_MAG.CCo.nBu.In.Po <- Prob.AIX_MAG.CCo.nBu.In.Po[i]
      t.Prob.AIX_MAG.CCo.nBu.In.nPo <- Prob.AIX_MAG.CCo.nBu.In.nPo[i]
      t.Prob.AIX_MAG.CCo.nBu.nIn.Po <- Prob.AIX_MAG.CCo.nBu.nIn.Po[i]
      t.Prob.AIX_MAG.CCo.nBu.nIn.nPo <- Prob.AIX_MAG.CCo.nBu.nIn.nPo[i]
      t.Prob.AIX_MAG.nCCo.Bu.In.Po <- Prob.AIX_MAG.nCCo.Bu.In.Po[i]
      t.Prob.AIX_MAG.nCCo.Bu.In.nPo <- Prob.AIX_MAG.nCCo.Bu.In.nPo[i]
      t.Prob.AIX_MAG.nCCo.Bu.nIn.Po <- Prob.AIX_MAG.nCCo.Bu.nIn.Po[i]
      t.Prob.AIX_MAG.nCCo.Bu.nIn.nPo <- Prob.AIX_MAG.nCCo.Bu.nIn.nPo[i]
      t.Prob.AIX_MAG.nCCo.nBu.In.Po <- Prob.AIX_MAG.nCCo.nBu.In.Po[i]
      t.Prob.AIX_MAG.nCCo.nBu.In.nPo <- Prob.AIX_MAG.nCCo.nBu.In.nPo[i]
      t.Prob.AIX_MAG.nCCo.nBu.nIn.Po <- Prob.AIX_MAG.nCCo.nBu.nIn.Po[i]
      t.Prob.AIX_MAG.nCCo.nBu.nIn.nPo <- Prob.AIX_MAG.nCCo.nBu.nIn.nPo[i]
      t.Prob.AIX_nMAG.CCo.Bu.In.Po <- Prob.AIX_nMAG.CCo.Bu.In.Po[i]
      t.Prob.AIX_nMAG.CCo.Bu.In.nPo <- Prob.AIX_nMAG.CCo.Bu.In.nPo[i]
      t.Prob.AIX_nMAG.CCo.Bu.nIn.Po <- Prob.AIX_nMAG.CCo.Bu.nIn.Po[i]
      t.Prob.AIX_nMAG.CCo.Bu.nIn.nPo <- Prob.AIX_nMAG.CCo.Bu.nIn.nPo[i]
      t.Prob.AIX_nMAG.CCo.nBu.In.Po <- Prob.AIX_nMAG.CCo.nBu.In.Po[i]
      t.Prob.AIX_nMAG.CCo.nBu.In.nPo <- Prob.AIX_nMAG.CCo.nBu.In.nPo[i]
      t.Prob.AIX_nMAG.CCo.nBu.nIn.Po <- Prob.AIX_nMAG.CCo.nBu.nIn.Po[i]
      t.Prob.AIX_nMAG.CCo.nBu.nIn.nPo <- Prob.AIX_nMAG.CCo.nBu.nIn.nPo[i]
      t.Prob.AIX_nMAG.nCCo.Bu.In.Po <- Prob.AIX_nMAG.nCCo.Bu.In.Po[i]
      t.Prob.AIX_nMAG.nCCo.Bu.In.nPo <- Prob.AIX_nMAG.nCCo.Bu.In.nPo[i]
      t.Prob.AIX_nMAG.nCCo.Bu.nIn.Po <- Prob.AIX_nMAG.nCCo.Bu.nIn.Po[i]
      t.Prob.AIX_nMAG.nCCo.Bu.nIn.nPo <- Prob.AIX_nMAG.nCCo.Bu.nIn.nPo[i]
      t.Prob.AIX_nMAG.nCCo.nBu.In.Po <- Prob.AIX_nMAG.nCCo.nBu.In.Po[i]
      t.Prob.AIX_nMAG.nCCo.nBu.In.nPo <- Prob.AIX_nMAG.nCCo.nBu.In.nPo[i]
      t.Prob.AIX_nMAG.nCCo.nBu.nIn.Po <- Prob.AIX_nMAG.nCCo.nBu.nIn.Po[i]
      t.Prob.AIX_nMAG.nCCo.nBu.nIn.nPo <- Prob.AIX_nMAG.nCCo.nBu.nIn.nPo[i]
    
    }
    
    #### Step 3.1: Model Build
    ## Initializing the model:
    vals <- c("True","False") # setting the logical constraints to apply to the Probability Tables
    SBl.Node <- cptable(~SBl, values=c(t.Prob.SBl,(100 - t.Prob.SBl)),levels=vals)
    # Probability AGI is feasible
    BFe.Node <- cptable(~BFe, values=c(t.Prob.BFe,(100 - t.Prob.BFe)),levels=vals)
    # Probability AGI is designed
    De.Node <- cptable(~De, values=c(t.Prob.De,(100 - t.Prob.De)),levels=vals)
    # Probability system properties are enabling for AGI X-Risk action (e.g., connectedness)
    SSu.Node  <- cptable(~SSu, values=c(t.Prob.SSu,(100 - t.Prob.SSu)),levels=vals)
    # Probability that an AGI implementer(user) is capable of detecting misaligned goals, subversion of user control, and AGI power to act
    Op.Node  <- cptable(~Op, values=c(t.Prob.Op,(100 - t.Prob.Op)),levels=vals)
    
    # Probability of AGI goal misalignment: P(MAG|SBl,Ca)
    MAG_CPT <-c(t.Prob.MAG_De.SBl,(100 - t.Prob.MAG_De.SBl),
                t.Prob.MAG_De.nSBl,(100 - t.Prob.MAG_De.nSBl),
                t.Prob.MAG_nDe.SBl,(100 - t.Prob.MAG_nDe.SBl),
                t.Prob.MAG_nDe.nSBl,(100 - t.Prob.MAG_nDe.nSBl))
    
    # Probability of AGI being able to subvert control: P(CCo|SBl,De)
    CCo_CPT <-c(t.Prob.CCo_De.SBl,(100 - t.Prob.CCo_De.SBl),
                t.Prob.CCo_De.nSBl,(100 - t.Prob.CCo_De.nSBl),
                t.Prob.CCo_nDe.SBl,(100 - t.Prob.CCo_nDe.SBl),
                t.Prob.CCo_nDe.nSBl,(100 - t.Prob.CCo_nDe.nSBl))
    
    # Probability of AGI being built with required aspects: P(Bu|De,BFe,In)
    Bu_CPT <-c(t.Prob.Bu_In.BFe.De,(100 - t.Prob.Bu_In.BFe.De),
               t.Prob.Bu_In.BFe.nDe,(100 - t.Prob.Bu_In.BFe.nDe),
               t.Prob.Bu_In.nBFe.De,(100 - t.Prob.Bu_In.nBFe.De),
               t.Prob.Bu_In.nBFe.nDe,(100 - t.Prob.Bu_In.nBFe.nDe),
               t.Prob.Bu_nIn.BFe.De,(100 - t.Prob.Bu_nIn.BFe.De),
               t.Prob.Bu_nIn.BFe.nDe,(100 - t.Prob.Bu_nIn.BFe.nDe),
               t.Prob.Bu_nIn.nBFe.De,(100 - t.Prob.Bu_nIn.nBFe.De),
               t.Prob.Bu_nIn.nBFe.nDe,(100 - t.Prob.Bu_nIn.nBFe.nDe))
    
    # Probability of AGI having sufficient power to commit X-Risk action: P(Po|De,SSu)
    Po_CPT <-c(t.Prob.Po_SSu.De,(100 - t.Prob.Po_SSu.De),
               t.Prob.Po_SSu.nDe,(100 - t.Prob.Po_SSu.nDe),
               t.Prob.Po_nSSu.De,(100 - t.Prob.Po_nSSu.De),
               t.Prob.Po_nSSu.nDe,(100 - t.Prob.Po_nSSu.nDe))
    
    # Probability of AGI goal misalignment: P(In|CCo,Op,Po,MAG)
    In_CPT <-c(t.Prob.In_MAG.Po.OProb.CCo,(100 - t.Prob.In_MAG.Po.OProb.CCo),
               t.Prob.In_MAG.Po.OProb.nCCo,(100 - t.Prob.In_MAG.Po.OProb.nCCo),
               t.Prob.In_MAG.Po.nOProb.CCo,(100 - t.Prob.In_MAG.Po.nOProb.CCo),
               t.Prob.In_MAG.Po.nOProb.nCCo,(100 - t.Prob.In_MAG.Po.nOProb.nCCo),
               t.Prob.In_MAG.nPo.OProb.CCo,(100 - t.Prob.In_MAG.nPo.OProb.CCo),
               t.Prob.In_MAG.nPo.OProb.nCCo,(100 - t.Prob.In_MAG.nPo.OProb.nCCo),
               t.Prob.In_MAG.nPo.nOProb.CCo,(100 - t.Prob.In_MAG.nPo.nOProb.CCo),
               t.Prob.In_MAG.nPo.nOProb.nCCo,(100 - t.Prob.In_MAG.nPo.nOProb.nCCo),
               t.Prob.In_nMAG.Po.OProb.CCo,(100 - t.Prob.In_nMAG.Po.OProb.CCo),
               t.Prob.In_nMAG.Po.OProb.nCCo,(100 - t.Prob.In_nMAG.Po.OProb.nCCo),
               t.Prob.In_nMAG.Po.nOProb.CCo,(100 - t.Prob.In_nMAG.Po.nOProb.CCo),
               t.Prob.In_nMAG.Po.nOProb.nCCo,(100 - t.Prob.In_nMAG.Po.nOProb.nCCo),
               t.Prob.In_nMAG.nPo.OProb.CCo,(100 - t.Prob.In_nMAG.nPo.OProb.CCo),
               t.Prob.In_nMAG.nPo.OProb.nCCo,(100 - t.Prob.In_nMAG.nPo.OProb.nCCo),
               t.Prob.In_nMAG.nPo.nOProb.CCo,(100 - t.Prob.In_nMAG.nPo.nOProb.CCo),
               t.Prob.In_nMAG.nPo.nOProb.nCCo,(100 - t.Prob.In_nMAG.nPo.nOProb.nCCo))
    
    # Probability of X-Risk from AGI: P(AIX|Po,In,Bu,CCo,MAG)
    AIX_CPT <-c(t.Prob.AIX_MAG.CCo.Bu.In.Po,(100 - t.Prob.AIX_MAG.CCo.Bu.In.Po),
                t.Prob.AIX_MAG.CCo.Bu.In.nPo,(100 - t.Prob.AIX_MAG.CCo.Bu.In.nPo),
                t.Prob.AIX_MAG.CCo.Bu.nIn.Po,(100 - t.Prob.AIX_MAG.CCo.Bu.nIn.Po),
                t.Prob.AIX_MAG.CCo.Bu.nIn.nPo,(100 - t.Prob.AIX_MAG.CCo.Bu.nIn.nPo),
                t.Prob.AIX_MAG.CCo.nBu.In.Po,(100 - t.Prob.AIX_MAG.CCo.nBu.In.Po),
                t.Prob.AIX_MAG.CCo.nBu.In.nPo,(100 - t.Prob.AIX_MAG.CCo.nBu.In.nPo),
                t.Prob.AIX_MAG.CCo.nBu.nIn.Po,(100 - t.Prob.AIX_MAG.CCo.nBu.nIn.Po),
                t.Prob.AIX_MAG.CCo.nBu.nIn.nPo,(100 - t.Prob.AIX_MAG.CCo.nBu.nIn.nPo),
                t.Prob.AIX_MAG.nCCo.Bu.In.Po,(100 - t.Prob.AIX_MAG.nCCo.Bu.In.Po),
                t.Prob.AIX_MAG.nCCo.Bu.In.nPo,(100 - t.Prob.AIX_MAG.nCCo.Bu.In.nPo),
                t.Prob.AIX_MAG.nCCo.Bu.nIn.Po,(100 - t.Prob.AIX_MAG.nCCo.Bu.nIn.Po),
                t.Prob.AIX_MAG.nCCo.Bu.nIn.nPo,(100 - t.Prob.AIX_MAG.nCCo.Bu.nIn.nPo),
                t.Prob.AIX_MAG.nCCo.nBu.In.Po,(100 - t.Prob.AIX_MAG.nCCo.nBu.In.Po),
                t.Prob.AIX_MAG.nCCo.nBu.In.nPo,(100 - t.Prob.AIX_MAG.nCCo.nBu.In.nPo),
                t.Prob.AIX_MAG.nCCo.nBu.nIn.Po,(100 - t.Prob.AIX_MAG.nCCo.nBu.nIn.Po),
                t.Prob.AIX_MAG.nCCo.nBu.nIn.nPo,(100 - t.Prob.AIX_MAG.nCCo.nBu.nIn.nPo),
                t.Prob.AIX_nMAG.CCo.Bu.In.Po,(100 - t.Prob.AIX_nMAG.CCo.Bu.In.Po),
                t.Prob.AIX_nMAG.CCo.Bu.In.nPo,(100 - t.Prob.AIX_nMAG.CCo.Bu.In.nPo),
                t.Prob.AIX_nMAG.CCo.Bu.nIn.Po,(100 - t.Prob.AIX_nMAG.CCo.Bu.nIn.Po),
                t.Prob.AIX_nMAG.CCo.Bu.nIn.nPo,(100 - t.Prob.AIX_nMAG.CCo.Bu.nIn.nPo),
                t.Prob.AIX_nMAG.CCo.nBu.In.Po,(100 - t.Prob.AIX_nMAG.CCo.nBu.In.Po),
                t.Prob.AIX_nMAG.CCo.nBu.In.nPo,(100 - t.Prob.AIX_nMAG.CCo.nBu.In.nPo),
                t.Prob.AIX_nMAG.CCo.nBu.nIn.Po,(100 - t.Prob.AIX_nMAG.CCo.nBu.nIn.Po),
                t.Prob.AIX_nMAG.CCo.nBu.nIn.nPo,(100 - t.Prob.AIX_nMAG.CCo.nBu.nIn.nPo),
                t.Prob.AIX_nMAG.nCCo.Bu.In.Po,(100 - t.Prob.AIX_nMAG.nCCo.Bu.In.Po),
                t.Prob.AIX_nMAG.nCCo.Bu.In.nPo,(100 - t.Prob.AIX_nMAG.nCCo.Bu.In.nPo),
                t.Prob.AIX_nMAG.nCCo.Bu.nIn.Po,(100 - t.Prob.AIX_nMAG.nCCo.Bu.nIn.Po),
                t.Prob.AIX_nMAG.nCCo.Bu.nIn.nPo,(100 - t.Prob.AIX_nMAG.nCCo.Bu.nIn.nPo),
                t.Prob.AIX_nMAG.nCCo.nBu.In.Po,(100 - t.Prob.AIX_nMAG.nCCo.nBu.In.Po),
                t.Prob.AIX_nMAG.nCCo.nBu.In.nPo,(100 - t.Prob.AIX_nMAG.nCCo.nBu.In.nPo),
                t.Prob.AIX_nMAG.nCCo.nBu.nIn.Po,(100 - t.Prob.AIX_nMAG.nCCo.nBu.nIn.Po),
                t.Prob.AIX_nMAG.nCCo.nBu.nIn.nPo,(100 - t.Prob.AIX_nMAG.nCCo.nBu.nIn.nPo))
    
    # Note, CPT function reads from "back to front" when ordering inputs
    MAG.Node <- cptable(~MAG|SBl:De, values = MAG_CPT,levels=vals)
    CCo.Node <- cptable(~CCo|SBl:De, values = CCo_CPT,levels=vals)
    Bu.Node <- cptable(~Bu|De:BFe:In, values = Bu_CPT,levels=vals)
    Po.Node <- cptable(~Po|De:SSu, values = Po_CPT,levels=vals)
    In.Node <- cptable(~In|CCo:Op:Po:MAG, values = In_CPT,levels=vals)
      
    AIX.Node <- cptable(~AIX|Po:In:Bu:CCo:MAG, values = AIX_CPT,levels=vals)
    
    XNet_List <- compileCPT(list(SBl.Node,BFe.Node,De.Node,SSu.Node,Op.Node,MAG.Node,
                                 CCo.Node,Bu.Node,Po.Node,In.Node,AIX.Node))
    
    # Initializing:
    XNet <- grain(XNet_List)  
    #plot(XNet)
    #XNet2 <- XNet1 # Duplicate constructor sometimes useful.
    
    
    ### Step 3.2: Model Query
    AIXTemp <- querygrain(XNet, nodes = c("AIX"), type = "joint")
    Post.AIX <- ((AIXTemp[1]) * 100)
    
    # Further checks on intermediary layer:
    MAGTemp <- querygrain(XNet, nodes = c("MAG"), type = "joint")
    Post.MAG <- as.numeric((MAGTemp[1]) * 100)
    CCoTemp <- querygrain(XNet, nodes = c("CCo"), type = "joint")
    Post.CCo <- as.numeric((CCoTemp[1]) * 100)
    BuTemp <- querygrain(XNet, nodes = c("Bu"), type = "joint")
    Post.Bu <- as.numeric((BuTemp[1]) * 100)
    PoTemp <- querygrain(XNet, nodes = c("Po"), type = "joint")
    Post.Po <- as.numeric((PoTemp[1]) * 100)
    InTemp <- querygrain(XNet, nodes = c("In"), type = "joint")
    Post.In <- as.numeric((InTemp[1]) * 100)
    
    ### Step 3.3: Storage
    t.Prob.AIX <- c(t.Prob.AIX,Post.AIX)
    # Further checks on intermediary layer:
    t.Prob.MAG <- c(t.Prob.MAG,Post.MAG)
    t.Prob.CCo <- c(t.Prob.CCo,Post.CCo)
    t.Prob.Bu <- c(t.Prob.Bu,Post.Bu)
    t.Prob.Po <- c(t.Prob.Po,Post.Po)
    t.Prob.In <- c(t.Prob.In,Post.In)

    ### Step 3.4: Conditionalising on bu=True
    XNet_Bu <- setEvidence(XNet, evidence=list(Bu="True"))
    
    ## Re-query Variables:
    AIXTemp <- querygrain(XNet_Bu, nodes = c("AIX"), type = "joint")
    Post.AIX <- ((AIXTemp[1]) * 100)
    # Further checks on intermediary layer:
    MAGTemp <- querygrain(XNet_Bu, nodes = c("MAG"), type = "joint")
    Post.MAG <- as.numeric((MAGTemp[1]) * 100)
    CCoTemp <- querygrain(XNet_Bu, nodes = c("CCo"), type = "joint")
    Post.CCo <- as.numeric((CCoTemp[1]) * 100)
    PoTemp <- querygrain(XNet_Bu, nodes = c("Po"), type = "joint")
    Post.Po <- as.numeric((PoTemp[1]) * 100)
    InTemp <- querygrain(XNet_Bu, nodes = c("In"), type = "joint")
    Post.In <- as.numeric((InTemp[1]) * 100)
    # Checking Input layer now, post-conditionalisation:
    SBlTemp <- querygrain(XNet_Bu, nodes = c("SBl"), type = "joint")
    Post.SBl <- as.numeric((SBlTemp[1]) * 100)
    BFeTemp <- querygrain(XNet_Bu, nodes = c("BFe"), type = "joint")
    Post.BFe <- as.numeric((BFeTemp[1]) * 100)
    DeTemp <- querygrain(XNet_Bu, nodes = c("De"), type = "joint")
    Post.De <- as.numeric((DeTemp[1]) * 100)
    SSuTemp <- querygrain(XNet_Bu, nodes = c("SSu"), type = "joint")
    Post.SSu <- as.numeric((SSuTemp[1]) * 100)
    OpTemp <- querygrain(XNet_Bu, nodes = c("Op"), type = "joint")
    Post.Op <- as.numeric((OpTemp[1]) * 100)
    
    t.Prob.AIX_Bu <- c(t.Prob.AIX_Bu,Post.AIX)
    t.Prob.MAG_Bu <- c(t.Prob.MAG_Bu,Post.MAG)
    t.Prob.CCo_Bu <- c(t.Prob.CCo_Bu,Post.CCo)
    t.Prob.Po_Bu <- c(t.Prob.Po_Bu,Post.Po)
    t.Prob.In_Bu <- c(t.Prob.In_Bu,Post.In)
    t.Prob.SBl_Bu <- c(t.Prob.SBl_Bu,Post.SBl)
    t.Prob.BFe_Bu <- c(t.Prob.BFe_Bu,Post.BFe)
    t.Prob.De_Bu <- c(t.Prob.De_Bu,Post.De)
    t.Prob.SSu_Bu <- c(t.Prob.SSu_Bu,Post.SSu)
    t.Prob.Op_Bu <- c(t.Prob.Op_Bu,Post.Op)
    
  }
  Prob.AIX <- c(Prob.AIX,mean(t.Prob.AIX))
  Prob.MAG <- c(Prob.MAG,mean(t.Prob.MAG))
  Prob.CCo <- c(Prob.CCo,mean(t.Prob.CCo))
  Prob.Bu <- c(Prob.Bu,mean(t.Prob.Bu))
  Prob.Po <- c(Prob.Po,mean(t.Prob.Po))
  Prob.In <- c(Prob.In,mean(t.Prob.In))
  ProbSD.AIX <- c(ProbSD.AIX,sd(t.Prob.AIX))
  ProbSD.MAG <- c(ProbSD.MAG,sd(t.Prob.MAG))
  ProbSD.CCo <- c(ProbSD.CCo,sd(t.Prob.CCo))
  ProbSD.Bu <- c(ProbSD.Bu,sd(t.Prob.Bu))
  ProbSD.Po <- c(ProbSD.Po,sd(t.Prob.Po))
  ProbSD.In <- c(ProbSD.In,sd(t.Prob.In))
  
  Prob.AIX_Bu <- c(Prob.AIX_Bu,mean(t.Prob.AIX_Bu))
  Prob.MAG_Bu <- c(Prob.MAG_Bu,mean(t.Prob.MAG_Bu))
  Prob.CCo_Bu <- c(Prob.CCo_Bu,mean(t.Prob.CCo_Bu))
  Prob.Po_Bu <- c(Prob.Po_Bu,mean(t.Prob.Po_Bu))
  Prob.In_Bu <- c(Prob.In_Bu,mean(t.Prob.In_Bu))
  ProbSD.AIX_Bu <- c(ProbSD.AIX_Bu,sd(t.Prob.AIX_Bu))
  ProbSD.MAG_Bu <- c(ProbSD.MAG_Bu,sd(t.Prob.MAG_Bu))
  ProbSD.CCo_Bu <- c(ProbSD.CCo_Bu,sd(t.Prob.CCo_Bu))
  ProbSD.Po_Bu <- c(ProbSD.Po_Bu,sd(t.Prob.Po_Bu))
  ProbSD.In_Bu <- c(ProbSD.In_Bu,sd(t.Prob.In_Bu))
  Prob.SBl_Bu <- c(Prob.SBl_Bu,mean(t.Prob.SBl_Bu))
  ProbSD.SBl_Bu <- c(ProbSD.SBl_Bu,sd(t.Prob.SBl_Bu))
  Prob.BFe_Bu <- c(Prob.BFe_Bu,mean(t.Prob.BFe_Bu))
  ProbSD.BFe_Bu <- c(ProbSD.BFe_Bu,sd(t.Prob.BFe_Bu))
  Prob.De_Bu <- c(Prob.De_Bu,mean(t.Prob.De_Bu))
  ProbSD.De_Bu <- c(ProbSD.De_Bu,sd(t.Prob.De_Bu))
  Prob.SSu_Bu <- c(Prob.SSu_Bu,mean(t.Prob.SSu_Bu))
  ProbSD.SSu_Bu <- c(ProbSD.SSu_Bu,sd(t.Prob.SSu_Bu))
  Prob.Op_Bu <- c(Prob.Op_Bu,mean(t.Prob.Op_Bu))
  ProbSD.Op_Bu <- c(ProbSD.Op_Bu,sd(t.Prob.Op_Bu))
}

#### Step 4: Data Processing

output.df <- data.frame(Years,Prob.AIX,ProbSD.AIX,Prob.MAG,ProbSD.MAG,Prob.CCo,ProbSD.CCo,
                        Prob.Bu,ProbSD.Bu,Prob.Po,ProbSD.Po,Prob.In,ProbSD.In,
                        Prob.SBl,ProbSD.SBl,Prob.BFe,ProbSD.BFe,Prob.De,ProbSD.De,Prob.SSu,ProbSD.SSu,Prob.Op,ProbSD.Op,
                        Prob.AIX_Bu,ProbSD.AIX_Bu,Prob.MAG_Bu,ProbSD.MAG_Bu,Prob.CCo_Bu,ProbSD.CCo_Bu,
                        Prob.Po_Bu,ProbSD.Po_Bu,Prob.In_Bu,ProbSD.In_Bu,Prob.SBl_Bu,ProbSD.SBl_Bu,
                        Prob.BFe_Bu,ProbSD.BFe_Bu,Prob.De_Bu,ProbSD.De_Bu,Prob.SSu_Bu,ProbSD.SSu_Bu,Prob.Op_Bu,ProbSD.Op_Bu)

write.csv(output.df,file="AI_XRisk_BN_Outputs.csv")

#### Step 5: Figure Generation

### Top-level Probability Output:

## Probability AIX:

jpeg('ProbAIX.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.AIX), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.AIX - ProbSD.AIX), ymax=(Prob.AIX + ProbSD.AIX)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 1),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1), limits = c(2020,2100)) +
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(xlim = c(2025,2100)) + # ylim = c(0, 100), 
  labs(x='Year', y='Probability (%)', title='P(AGI Misalignment X-Event) [P(AIX)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

### Mid-level Conditionals:

# Probability of Misaligned Goals:
jpeg('ProbMAG.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.MAG), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.MAG - ProbSD.MAG), ymax=(Prob.MAG + ProbSD.MAG)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI has Misaligned Goals) [P(MAG)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability of AI being able to subvert control:
jpeg('ProbCCo.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.CCo), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.CCo - ProbSD.CCo), ymax=(Prob.CCo + ProbSD.CCo)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI Circumvents Control) [P(CCo)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph
#
## Probability of AI being built with requisite capabilities
jpeg('ProbBu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.Bu - ProbSD.Bu), ymax=(Prob.Bu + ProbSD.Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1), limits = c(2020,2100)) +
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2043, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2100, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI Built) [P(Bu)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability of AI having Power to commit X-Risk Event:
jpeg('ProbPo.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.Po), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.Po - ProbSD.Po), ymax=(Prob.Po + ProbSD.Po)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability', title='P(AGI has Power to Commit X-Event) [P(Po)]',size=14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

## Probability of Incentive for AI:
jpeg('ProbIn.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.In), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.In - ProbSD.In), ymax=(Prob.In + ProbSD.In)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(Incentive) [P(In)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

### Base-level (inputs):

## Probability of User knowing System:
jpeg('ProbSBl.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.SBl), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.SBl - ProbSD.SBl), ymax=(Prob.SBl + ProbSD.SBl)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(User System Blind) [P(SBl)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability AI Build is Feasible:
jpeg('ProbBFe.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.BFe), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.BFe - ProbSD.BFe), ymax=(Prob.BFe + ProbSD.BFe)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI Build is Feasible) [P(BFe)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability AI Design viable:
jpeg('ProbDe.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.De), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.De - ProbSD.De), ymax=(Prob.De + ProbSD.De)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI Viable Design) [P(De)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability System is Susceptible (e.g., Connectedness):
jpeg('ProbSSu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.SSu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.SSu - ProbSD.SSu), ymax=(Prob.SSu + ProbSD.SSu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(System is Susceptible) [P(SSu)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

## Probability AI Features are Transparent to Users:
jpeg('ProbOp.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.Op), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.Op - ProbSD.Op), ymax=(Prob.Op + ProbSD.Op)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI Features Opaque) [P(Op)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

########################################################################################################
#### The figures below plot the above probabilities, conditioned on an AGI build (P(Bu)) being TRUE ####
########################################################################################################

## Probability AIX:

jpeg('ProbAIX_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.AIX_Bu), geom="line", size = .6,color=c("red")) +
  geom_ribbon(aes(ymin=(Prob.AIX_Bu - ProbSD.AIX_Bu), ymax=(Prob.AIX_Bu + ProbSD.AIX_Bu)),alpha=0.2,linetype=0,fill=c("red"),color=c("black")) +
  stat_summary(fun=mean, aes(y = Prob.AIX), geom="line", size = .6,color=c("blue")) +
  geom_ribbon(aes(ymin=(Prob.AIX - ProbSD.AIX), ymax=(Prob.AIX + ProbSD.AIX)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 1),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1), limits = c(2020,2100)) +
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(xlim = c(2025,2100)) + # ylim = c(0, 100), 
  labs(x='Year', y='Probability (%)', title='P(AGI Misalignment X-Event|Built) [P(AIX)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

### Mid-level Conditionals:

# Probability of Misaligned Goals:
jpeg('ProbMAG_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.MAG_Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.MAG_Bu - ProbSD.MAG_Bu), ymax=(Prob.MAG_Bu + ProbSD.MAG_Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI has Misaligned Goals|Built) [P(MAG)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability of AI being able to subvert control:
jpeg('ProbCCo_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.CCo_Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.CCo_Bu - ProbSD.CCo_Bu), ymax=(Prob.CCo_Bu + ProbSD.CCo_Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI Circumvents Control|Built) [P(CCo)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability of AI having Power to commit X-Risk Event:
jpeg('ProbPo_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.Po_Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.Po_Bu - ProbSD.Po_Bu), ymax=(Prob.Po_Bu + ProbSD.Po_Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability', title='P(AGI has Power to Commit X-Event|Built) [P(Po)]',size=14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

## Probability of Desire to Deploy AI:
jpeg('ProbIn_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.In_Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.In_Bu - ProbSD.In_Bu), ymax=(Prob.In_Bu + ProbSD.In_Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(Incentive|Built) [P(In)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

### Base-level (inputs):

## Probability of User knowing System:
jpeg('ProbSBl_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.SBl_Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.SBl_Bu - ProbSD.SBl_Bu), ymax=(Prob.SBl_Bu + ProbSD.SBl_Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(User System Blind|Built) [P(SBl)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability AI Build is Feasible:
jpeg('ProbBFe_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.BFe_Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.BFe_Bu - ProbSD.BFe_Bu), ymax=(Prob.BFe_Bu + ProbSD.BFe_Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI Build is Feasible|Built) [P(BFe)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability AI Design viable:
jpeg('ProbDe_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.De_Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.De_Bu - ProbSD.De_Bu), ymax=(Prob.De_Bu + ProbSD.De_Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI Viable Design|Built) [P(De)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

# Probability System is Susceptible (e.g., Connectedness):
jpeg('ProbSSu_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.SSu_Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.SSu_Bu - ProbSD.SSu_Bu), ymax=(Prob.SSu_Bu + ProbSD.SSu_Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(System is Susceptible|Built) [P(SSu)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

## Probability AI Features are Transparent to Users:
jpeg('ProbOp_Bu.jpg', units="in", width=5, height=5, res=300)
ggplot(output.df, aes(x=Years)) +
  stat_summary(fun=mean, aes(y = Prob.Op_Bu), geom="line", size = .6) +
  geom_ribbon(aes(ymin=(Prob.Op_Bu - ProbSD.Op_Bu), ymax=(Prob.Op_Bu + ProbSD.Op_Bu)),alpha=0.2,linetype=0,fill=c("blue"),color=c("black")) +
  #geom_line(aes(y=Prob.AIX, x=Years),color=c("black"),size=0.75,linetype="dashed") +
  scale_y_continuous(breaks = round(seq(0, 100, by = 5),1)) + # , limits = c(0, 100)
  scale_x_continuous(breaks = round(seq(2020, 2100, by = 10),1)) + # , limits = c(2020,2100)
  #theme_bw() +
  theme_classic() + #theme_minimal() +
  geom_vline(xintercept = 2070, size = .5, colour = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, size = .5, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, size = .5, colour = "black", linetype = "solid") +
  coord_cartesian(ylim = c(0, 100), xlim = c(2025,2100)) +
  labs(x='Year', y='Probability (%)', title='P(AGI Features Opaque|Built) [P(Op)]',size = 14) +
  theme(text = element_text(family = "serif",size = 14),
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white",colour = "black"),
        panel.background = element_rect(fill="white", colour = "black"), # , colour = "black"
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "serif",size = 14,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 14),
        legend.position = 'bottom')
dev.off() #sending out graph

