# Make the OM objects, all based on the FLStock objects:
# FLBiol for each stock
# FLCatch for each stock in each FLFishery
# 2 x FLFishery (BT2 and TR1)
# FLFisheries to contain the FLFishery objects

rm(list=ls())
#library("devtools");
#options(devtools.install.args = "--no-multiarch")   
#install_github("iagomosqueira/FLasher")
#install_github("flr/FLCore")
#install_github("iagomosqueira/FLFishery")
library(FLasher)

# Load the FLStock objects
load("../data/biology_and_selectivity_basic_data.Rdata")

#------------------------------------------------------
# Make FLBiol objects for each stock
# Based on the FLStock objects and the fitted SRR model
ple.biol <- as(ple, "FLBiol")
ple.biol@desc  <- ple.biol@name <- "ple" 
ple.biol@rec@model <- srple1@model
ple.biol@rec@params <- srple1@params

sol.biol <- as(sol, "FLBiol")
sol.biol@desc  <- sol.biol@name <- "sol"
sol.biol@rec@model <- srsol1@model
sol.biol@rec@params <- srsol1@params

tur.biol <- as(tur, "FLBiol")
tur.biol@desc  <- tur.biol@name <- "tur"
tur.biol@rec@model <- srtur1@model
tur.biol@rec@params <- srtur1@params

bll.biol <- as(bll, "FLBiol")
bll.biol@desc  <- bll.biol@name <- "bll"
bll.biol@rec@model <- srtur1@model
bll.biol@rec@params <- srtur1@params

biols <- FLBiols(ple=ple.biol,sol=sol.biol,tur=tur.biol, bll=bll.biol)

#------------------------------------------------------
# Make the FLCatch objects - 2 for each stock

CBT2ple <- CTR1ple <- as(ple,"FLCatch")
CBT2sol <- CTR1sol <- as(sol,"FLCatch")
CBT2tur <- CTR1tur <- as(tur,"FLCatch")
CBT2bll <- CTR1bll <- as(bll,"FLCatch")

# Each FLCatch must have the appopriate selectivity
# Also landings.n and discards.n - these are overwritten in the projections but are used to calculate the discards ratio
# get catch sels from results od set selectivities ...

CBT2ple@catch.sel[]  <- selple80
CTR1ple@catch.sel[]  <- selple100
CBT2ple@landings.n[] <- Dple
CBT2ple@discards.n[] <- 1- Dple
CTR1ple@landings.n[] <- Dple
CTR1ple@discards.n[] <- 1- Dple

CBT2sol@catch.sel[] <- selsol80
CTR1sol@catch.sel[] <- selsol100
CBT2sol@landings.n[] <- Dsol
CBT2sol@discards.n[] <- 1- Dsol
CTR1sol@landings.n[] <- Dsol
CTR1sol@discards.n[] <- 1- Dsol

CBT2tur@catch.sel[] <- seltur80
CTR1tur@catch.sel[] <- seltur100
CBT2tur@landings.n[] <- Dtur
CBT2tur@discards.n[] <- 1- Dtur
CTR1tur@landings.n[] <- Dtur
CTR1tur@discards.n[] <- 1- Dtur

CBT2bll@catch.sel[] <- selbll80
CTR1bll@catch.sel[] <- selbll100
CBT2bll@landings.n[] <- Dbll
CBT2bll@discards.n[] <- 1- Dbll
CTR1bll@landings.n[] <- Dbll
CTR1bll@discards.n[] <- 1- Dbll

# Set an initial guess at catchability (the alpha parameter - beta should always be 0)
catch.q(CBT2ple) <- catch.q(CTR1ple) <-
catch.q(CBT2sol) <- catch.q(CTR1sol) <-
catch.q(CBT2tur) <- catch.q(CTR1tur) <- 
catch.q(CBT2bll) <- catch.q(CTR1bll) <-  c(1,0) 

# Make the FLFishery objects
# Set the effort to 1 - overwritten later
BT2 <- FLFishery(BT2ple= CBT2ple,BT2sol= CBT2sol,BT2tur= CBT2tur,BT2bll= CBT2bll,desc="BT2")
BT2@effort[]  <- 1
TR1 <- FLFishery(TR1ple= CTR1ple,TR1sol= CTR1sol,TR1tur= CTR1tur,TR1bll= CTR1bll,desc="TR1")
TR1@effort[]  <- 1

# Make the FLFisheries
fisheries <- FLFisheries(BT2=BT2, TR1=TR1)

FCB <- matrix(c(rep(c(1,2), each=4), rep(c(1:4),4)),ncol=3,byrow=F)

# Done!
# Could also add in the BRP objects to get other initial abundances 
save(ple, sol, tur, bll, biols, fisheries, FCB, srple1, srsol1, srtur1, file="../data/OM.Rdata")

#--------------------------------------------------------
#year <- 2;
#ctarget1 <- 100
#ctarget2 <- 200
#ctrldf <- data.frame(year=rep(year, each=2),quant="catch", fishery=c(1,2),catch=c(1,1), value=c(ctarget1,ctarget2))
#
#ctrl <- fwdControl(ctrldf)
#ctrl@FCB <- FCB
#test <- fwd(object=biols, fishery=fisheries, control=ctrl)
#test[["flag"]]  # is optimizer successful
#
#BT2out <- test[["fisheries"]][["BT2"]]
#TR1out <- test[["fisheries"]][["TR1"]]
#
#catch(BT2out[[1]])
#catch(BT2out[[2]])
#effort(BT2out)

