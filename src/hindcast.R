# Attempt to perform a hindcast based on landings of place and sole, and effort of BT2 and TR1
rm(list=ls())
library(FLasher)
library(ggplot2)
# Other funcs (e.g. to estimate F)
source("funcs.R")

# Attempt to estimate Q - the catchability parameter - for each FLCatch
# Internally: F = Effort * Selectivty * Q
# Effort is not age structured and is at the FLFishery level. The same effort is applied to all FLCatches in that FLFishery
# Selectivity is age structured and is at the FLCatch level
# Q is not age structured. It can be structured by year if wanted. It is at the FLCatch level.

# Here we assume that all other parameters (selectivity, weights, etc) are correct but that Q is unknown.
# So we estimate Q from the observed catch and effort data

# Two attempted ways of estimating Q.
# 1. Drive model with observed effort and compare predicted landings to observed landings.
# 2. Drive model with observed landings (on one of the stocks) and compare predicted landings to observed landings.

# Focus on 

# Get observed landings and effort
source("1 aggregate landings and effort data from STECF.R")

# Years
years <- 2004:2015
lanbygearyear <- subset(lanbygearyear, year %in% years)
effbygearyear <- subset(effbygearyear, year %in% years)

# Take a look at the effort
unique(effbygearyear$regulated.gear)
ggplot(effbygearyear, aes(x=year, y=Effective.Effort)) + geom_line(aes(colour=regulated.gear))

# And the landings
summary(lanbygearyear)
ggplot(lanbygearyear, aes(x=year, y=Landings)) + geom_line(aes(linetype=species, colour=regulated.gear))
ggplot(lanbygearyear, aes(x=year, y=Landings)) + geom_line(aes(colour=regulated.gear)) + facet_wrap(~species, scales="free")

# Ignore file 2 - LH to objects

# Load objects
load("../data/OM.Rdata") # From script 3

#---------------------------------------------------
# Final conditioning


# Initial conditions:
# Based on stock assessment
# Or abundance at MSY

# Need to give some effort to fisheries (effort multiplier is then found)
# Fix this in script 3

# Need to load effort in 2004 in fisheries objects as this us used to updateN in first year of projection
fisheries[["BT2"]]@effort[] <- subset(effbygearyear, regulated.gear=="BT2" & year %in% 2004:2015)$Effective.Effort
fisheries[["TR1"]]@effort[] <- subset(effbygearyear, regulated.gear=="TR1" & year %in% 2004:2015)$Effective.Effort

# Better guess at Q - using final F / effort
fisheries[["TR1"]][[1]]@catch.q["alpha",] <- 0.1 / 800000 
fisheries[["BT2"]][[1]]@catch.q["alpha",] <- 0.1 / 300000
fisheries[["TR1"]][[2]]@catch.q["alpha",] <- 0.02 / 800000
fisheries[["BT2"]][[2]]@catch.q["alpha",] <- 0.2 / 300000  # F / effort

#---------------------------------------------------

# Option 1
# Project with effort - look at landings
proj_years <- 2005:2015

# Observed landings for plaice and sole
bt2_ple_lan_obs <- subset(lanbygearyear, regulated.gear=="BT2" & species=="PLE" & year %in% proj_years)$Landings
bt2_sol_lan_obs <- subset(lanbygearyear, regulated.gear=="BT2" & species=="SOL" & year %in% proj_years)$Landings
tr1_ple_lan_obs <- subset(lanbygearyear, regulated.gear=="TR1" & species=="PLE" & year %in% proj_years)$Landings
tr1_sol_lan_obs <- subset(lanbygearyear, regulated.gear=="TR1" & species=="SOL" & year %in% proj_years)$Landings

# Observed effort for BT2 and TR1
bt2_effort_obs <- subset(effbygearyear, regulated.gear=="BT2" & year %in% proj_years)$Effective.Effort
tr1_effort_obs <- subset(effbygearyear, regulated.gear=="TR1" & year %in% proj_years)$Effective.Effort

# Make effort control - HACK
eff_ctrldf <- data.frame(year=rep(proj_years, each=2), quant="effort", fishery=c(1,2), value=NA)
eff_ctrldf[eff_ctrldf$fishery==1,"value"] <- bt2_effort_obs
eff_ctrldf[eff_ctrldf$fishery==2,"value"] <- tr1_effort_obs
eff_ctrl <- fwdControl(eff_ctrldf)
eff_ctrl@FCB <- FCB

# Test run
run <- fwd(object=biols, fishery=fisheries, control=eff_ctrl)

flfs <- run[["fisheries"]]

# Are efforts OK
flfs[["BT2"]]@effort
bt2_effort_obs
flfs[["TR1"]]@effort
tr1_effort_obs


# Name accessor is broken : ple , sol , tur, brill
landings(flfs[["BT2"]][[1]])
subset(lanbygearyear, regulated.gear=="BT2" & species=="PLE" & year %in% 2004:2015)$Landings
landings(flfs[["TR1"]][[1]])
subset(lanbygearyear, regulated.gear=="TR1" & species=="PLE" & year %in% 2004:2015)$Landings

landings(flfs[["BT2"]][[2]])
subset(lanbygearyear, regulated.gear=="BT2" & species=="SOL" & year %in% 2004:2015)$Landings
landings(flfs[["TR1"]][[2]])
subset(lanbygearyear, regulated.gear=="TR1" & species=="SOL" & year %in% 2004:2015)$Landings



#-----------------------------------------------------
# Try to estimate Qs based on years 2010-2015

fisheries_temp <- fisheries

fit_years <- 2010:2015

# Observed landings for plaice and sole in the fit years
# Observerd
olan_bt2_ple <- FLQuant(subset(lanbygearyear, regulated.gear=="BT2" & species=="PLE" & year %in% 2004:2015)$Landings, dimnames=list(age="all", year=2004:2015))
olan_tr1_ple <- FLQuant(subset(lanbygearyear, regulated.gear=="TR1" & species=="PLE" & year %in% 2004:2015)$Landings, dimnames=list(age="all", year=2004:2015))
olan_bt2_sol <- FLQuant(subset(lanbygearyear, regulated.gear=="BT2" & species=="SOL" & year %in% 2004:2015)$Landings, dimnames=list(age="all", year=2004:2015))
olan_tr1_sol <- FLQuant(subset(lanbygearyear, regulated.gear=="TR1" & species=="SOL" & year %in% 2004:2015)$Landings, dimnames=list(age="all", year=2004:2015))

evalQ <- function(log_qs){
    qs <- exp(log_qs)
    cat("Guess at q:", qs, "\n")
    # Update qs
    catch.q(fisheries_temp[["BT2"]][[1]])["alpha",] <- qs[1]
    catch.q(fisheries_temp[["BT2"]][[2]])["alpha",] <- qs[2]
    catch.q(fisheries_temp[["TR1"]][[1]])["alpha",] <- qs[3]
    catch.q(fisheries_temp[["TR1"]][[2]])["alpha",] <- qs[4]
    # Project
    run <- fwd(object=biols, fishery=fisheries_temp, control=eff_ctrl)
    # Get predicted catches
    lhat11 <- landings(run[["fisheries"]][["BT2"]][[1]])[,ac(fit_years)] 
    lhat12 <- landings(run[["fisheries"]][["BT2"]][[2]])[,ac(fit_years)]
    lhat21 <- landings(run[["fisheries"]][["TR1"]][[1]])[,ac(fit_years)]
    lhat22 <- landings(run[["fisheries"]][["TR1"]][[2]])[,ac(fit_years)]
    # Create error term
    #error11 <- sum(abs(c(log(lhat11 / olan_bt2_ple[,ac(fit_years)])))) 
    #error12 <- sum(abs(c(log(lhat12 / olan_bt2_sol[,ac(fit_years)])))) 
    #error21 <- sum(abs(c(log(lhat21 / olan_tr1_ple[,ac(fit_years)]))))
    #error22 <- sum(abs(c(log(lhat22 / olan_tr1_sol[,ac(fit_years)]))))
    error11 <- sum(abs(c((lhat11 - olan_bt2_ple[,ac(fit_years)])))) 
    error12 <- sum(abs(c((lhat12 - olan_bt2_sol[,ac(fit_years)])))) 
    error21 <- sum(abs(c((lhat21 - olan_tr1_ple[,ac(fit_years)]))))
    error22 <- sum(abs(c((lhat22 - olan_tr1_sol[,ac(fit_years)]))))
    error <- error11+error12+error21+error22
    cat("error: ", error, "\n")
    return(error)
}
# Initial
logqinit <-  log(c(c(fisheries[[1]][[1]]@catch.q["alpha",]), c(fisheries[[1]][[2]]@catch.q["alpha",]), c(fisheries[[2]][[1]]@catch.q["alpha",]), c(fisheries[[1]][[2]]@catch.q["alpha",])))
toptim <- optim(logqinit, evalQ, control=list(trace=1))
finalq <- exp(toptim$par)

# finalq1 <- finalq # log prop error
finalq2 <- finalq # abs difference error


fisheries_temp2 <- fisheries_temp

# Test fit
catch.q(fisheries_temp[["BT2"]][[1]])["alpha",] <- finalq1[1]
catch.q(fisheries_temp[["BT2"]][[2]])["alpha",] <- finalq1[2]
catch.q(fisheries_temp[["TR1"]][[1]])["alpha",] <- finalq1[3]
catch.q(fisheries_temp[["TR1"]][[2]])["alpha",] <- finalq1[4]
run1 <- fwd(object=biols, fishery=fisheries_temp, control=eff_ctrl)

catch.q(fisheries_temp2[["BT2"]][[1]])["alpha",] <- finalq2[1]
catch.q(fisheries_temp2[["BT2"]][[2]])["alpha",] <- finalq2[2]
catch.q(fisheries_temp2[["TR1"]][[1]])["alpha",] <- finalq2[3]
catch.q(fisheries_temp2[["TR1"]][[2]])["alpha",] <- finalq2[4]
run2 <- fwd(object=biols, fishery=fisheries_temp2, control=eff_ctrl)



flfs1 <- run1[["fisheries"]]
flfs2 <- run2[["fisheries"]]

# Are efforts OK
flfs[["BT2"]]@effort
bt2_effort_obs
flfs[["TR1"]]@effort
tr1_effort_obs


# Name accessor is broken : ple , sol , tur, brill
# Plaice
landings(flfs[["BT2"]][[1]])
olan_bt2_ple
landings(flfs[["TR1"]][[1]])
olan_tr1_ple

landings(flfs[["BT2"]][[2]])
olan_bt2_sol
landings(flfs[["TR1"]][[2]])
olan_tr1_sol

pdat1 <- rbind(
cbind(metric="hat", stock="ple", fishery="bt2",as.data.frame(landings(flfs1[["BT2"]][[1]]))) ,
cbind(metric="obs", stock="ple", fishery="bt2",as.data.frame(olan_bt2_ple)),
cbind(metric="hat", stock="ple", fishery="tr1",as.data.frame(landings(flfs1[["TR1"]][[1]]))),
cbind(metric="obs", stock="ple", fishery="tr1",as.data.frame(olan_tr1_ple)),
cbind(metric="hat", stock="sol", fishery="bt2",as.data.frame(landings(flfs1[["BT2"]][[2]]))),
cbind(metric="obs", stock="sol", fishery="bt2",as.data.frame(olan_bt2_sol)),
cbind(metric="hat", stock="sol", fishery="tr1",as.data.frame(landings(flfs1[["TR1"]][[2]]))),
cbind(metric="obs", stock="sol", fishery="tr1",as.data.frame(olan_tr1_sol)))

pdat2 <- rbind(
cbind(metric="hat", stock="ple", fishery="bt2",as.data.frame(landings(flfs2[["BT2"]][[1]]))) ,
cbind(metric="obs", stock="ple", fishery="bt2",as.data.frame(olan_bt2_ple)),
cbind(metric="hat", stock="ple", fishery="tr1",as.data.frame(landings(flfs2[["TR1"]][[1]]))),
cbind(metric="obs", stock="ple", fishery="tr1",as.data.frame(olan_tr1_ple)),
cbind(metric="hat", stock="sol", fishery="bt2",as.data.frame(landings(flfs2[["BT2"]][[2]]))),
cbind(metric="obs", stock="sol", fishery="bt2",as.data.frame(olan_bt2_sol)),
cbind(metric="hat", stock="sol", fishery="tr1",as.data.frame(landings(flfs2[["TR1"]][[2]]))),
cbind(metric="obs", stock="sol", fishery="tr1",as.data.frame(olan_tr1_sol)))


pdat <- rbind(cbind(fit="prop", pdat1), cbind(fit="diff", pdat2))
pdat <- subset(pdat, year > 2004)

ggplot(pdat, aes(x=year, y=data)) + geom_line(aes(linetype=fishery, colour=metric))+ facet_grid(stock~fit, scales="free")


# Reconstruct error term

sum(abs(log((landings(flfs[["BT2"]][[1]]) / olan_bt2_ple)[,ac(fit_years)]))) 
sum(abs(log((landings(flfs[["TR1"]][[1]]) / olan_tr1_ple)[,ac(fit_years)])))
sum(abs(log((landings(flfs[["BT2"]][[2]]) / olan_bt2_sol)[,ac(fit_years)])))
sum(abs(log((landings(flfs[["TR1"]][[2]]) / olan_tr1_sol)[,ac(fit_years)])))



