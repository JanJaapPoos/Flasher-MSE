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

# Focus on plaice and sole

#------------------------------------------------------
# Observed effort and landings data

# Get observed landings and effort
source("1 aggregate landings and effort data from STECF.R")

# Hindcast years including initial year
years <- 2004:2015
lanbygearyear <- subset(lanbygearyear, year %in% years)
effbygearyear <- subset(effbygearyear, year %in% years)

# Take a look at the effort
#unique(effbygearyear$regulated.gear)
ggplot(effbygearyear, aes(x=year, y=Effective.Effort)) + geom_line(aes(colour=regulated.gear))

# And the landings
#summary(lanbygearyear)
#ggplot(lanbygearyear, aes(x=year, y=Landings)) + geom_line(aes(colour=regulated.gear)) + facet_wrap(~species, scales="free")
# Brill looks wrong in final year

# Build FLQuant objects of the observed landings and effort

# Observed landings for plaice and sole
obslan_bt2_ple <- FLQuant(subset(lanbygearyear, regulated.gear=="BT2" & species=="PLE" & year %in% 2004:2015)$Landings, dimnames=list(age="all", year=years))
obslan_tr1_ple <- FLQuant(subset(lanbygearyear, regulated.gear=="TR1" & species=="PLE" & year %in% 2004:2015)$Landings, dimnames=list(age="all", year=years))
obslan_bt2_sol <- FLQuant(subset(lanbygearyear, regulated.gear=="BT2" & species=="SOL" & year %in% 2004:2015)$Landings, dimnames=list(age="all", year=years))
obslan_tr1_sol <- FLQuant(subset(lanbygearyear, regulated.gear=="TR1" & species=="SOL" & year %in% 2004:2015)$Landings, dimnames=list(age="all", year=years))

# Observed effort for BT2 and TR1
obseff_bt2 <- FLQuant(subset(effbygearyear, regulated.gear=="BT2" & year %in% years)$Effective.Effort, dimnames=list(age="all", year=years))
obseff_tr1 <- FLQuant(subset(effbygearyear, regulated.gear=="TR1" & year %in% years)$Effective.Effort, dimnames=list(age="all", year=years))

#---------------------------------------------------
# Final conditioning of OM
# Load OM objects generated from script 3
load("../data/OM.Rdata") # From script 3

# Initial abundance in stocks already set at Fmsy equib level (see script 1)

# Set recruitment to be fixed at the estimated recruitment, i.e. no model
biols[["ple"]]@rec@model <- ~rec
biols[["ple"]]@rec@params <- FLPar(c(rec(srple1)[,ac(years)]), dimnames=list(params="rec",year=years, iter=1))

biols[["sol"]]@rec@model <- ~rec
biols[["sol"]]@rec@params <- FLPar(c(rec(srsol1)[,ac(years)]), dimnames=list(params="rec",year=years, iter=1))

biols[["tur"]]@rec@model <- ~rec
biols[["tur"]]@rec@params <- FLPar(c(rec(srtur1)[,ac(years)]), dimnames=list(params="rec",year=years, iter=1))

biols[["bll"]]@rec@model <- ~rec
biols[["bll"]]@rec@params <- FLPar(c(rec(srtur1)[,ac(years)]), dimnames=list(params="rec",year=years, iter=1))

# Insert observed effort into fisheries objects
# In the projections the effort multiplier is found in each year to hit the target
# The initial effort should therefore be in the right ball park
# The observed effort is a good (perfect) guess at the required effort
# Effort in 2004 is used to update N in 2005, even though first projection target is in 2005
fisheries[["BT2"]]@effort <- obseff_bt2
fisheries[["TR1"]]@effort <- obseff_tr1

# Better guess at Q - using final partial F / final effort
# Final total F of plaice is abut 0.2
# Initial guess is that the total F of plaice is split equally between the fisheries (0.1 for BT2 and TR1 each)
# Initial guess is that the total F of sole is almost all due to BT2 (0.2 on BT2)
# F = Sel * Q * effort
# Sel maximum is 1
# Q ~ F / effort
fisheries[["BT2"]][[1]]@catch.q["alpha",] <- 0.1 / obseff_bt2[,ac(2015)]
fisheries[["TR1"]][[1]]@catch.q["alpha",] <- 0.1 / obseff_tr1[,ac(2015)]
fisheries[["BT2"]][[2]]@catch.q["alpha",] <- 0.2 / obseff_bt2[,ac(2015)]
fisheries[["TR1"]][[2]]@catch.q["alpha",] <- 0.02 / obseff_tr1[,ac(2015)]

#---------------------------------------------------
# Test - just to see if the projection will run
# Drive projection with observed effort

proj_years <- 2005:2015

# Make effort control
eff_ctrldf <- data.frame(year=rep(proj_years, each=2), quant="effort", fishery=c(1,2), value=NA)
eff_ctrldf[eff_ctrldf$fishery==1,"value"] <- c(obseff_bt2[,ac(proj_years)])
eff_ctrldf[eff_ctrldf$fishery==2,"value"] <- c(obseff_tr1[,ac(proj_years)])
eff_ctrl <- fwdControl(eff_ctrldf)
eff_ctrl@FCB <- FCB

# Test run
run <- fwd(object=biols, fishery=fisheries, control=eff_ctrl)
# Extract fishery output
flfs <- run[["fisheries"]]

# Are efforts OK - i.e. did we hit the observed effort?
flfs[["BT2"]]@effort
obseff_bt2
flfs[["TR1"]]@effort
obseff_tr1

# Look at landings - any good?
# Name accessor is broken. Order of stocks is ple , sol , tur, brill

# Plaice
landings(flfs[["BT2"]][[1]])[,ac(proj_years)]
obslan_bt2_ple[,ac(proj_years)]
landings(flfs[["TR1"]][[1]])[,ac(proj_years)]
obslan_tr1_ple[,ac(proj_years)]

# Sole
landings(flfs[["BT2"]][[2]])[,ac(proj_years)]
obslan_bt2_sol[,ac(proj_years)]
landings(flfs[["TR1"]][[2]])[,ac(proj_years)]
obslan_tr1_sol[,ac(proj_years)]

#-----------------------------------------------------
# Try to estimate Qs
# Use error in observed and predicted plaice and sole landings in years 2010-2015
# Error term based on:
# 1. proportional difference = abs(log(hat / obs))
# 2. absolute difference = abs(hat - obs)

fit_years <- 2010:2015

# Copy to mess about with 
fisheries_temp <- fisheries

# Function for optim - based on log Qs - stop negative values
evalQ <- function(log_qs, error_type){
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
    if (error_type == "prop"){ 
        error11 <- sum(abs(c(log(lhat11/ obslan_bt2_ple[,ac(fit_years)])))) 
        error12 <- sum(abs(c(log(lhat12/ obslan_bt2_sol[,ac(fit_years)])))) 
        error21 <- sum(abs(c(log(lhat21/ obslan_tr1_ple[,ac(fit_years)]))))
        error22 <- sum(abs(c(log(lhat22/ obslan_tr1_sol[,ac(fit_years)]))))
    }
    if (error_type == "diff"){ 
        error11 <- sum(abs(c((lhat11 - obslan_bt2_ple[,ac(fit_years)])))) 
        error12 <- sum(abs(c((lhat12 - obslan_bt2_sol[,ac(fit_years)])))) 
        error21 <- sum(abs(c((lhat21 - obslan_tr1_ple[,ac(fit_years)]))))
        error22 <- sum(abs(c((lhat22 - obslan_tr1_sol[,ac(fit_years)]))))
    }
    # Sum catch errors to build single error term
    error <- error11+error12+error21+error22
    cat("error: ", error, "\n")
    return(error)
}

# Initial value of Q on log scale
logqinit <-  log(c(c(fisheries[[1]][[1]]@catch.q["alpha",]), c(fisheries[[1]][[2]]@catch.q["alpha",]), c(fisheries[[2]][[1]]@catch.q["alpha",]), c(fisheries[[1]][[2]]@catch.q["alpha",])))
# Run optim - simple Nelder Mead with max 500 iters
toptim_prop <- optim(logqinit, evalQ, control=list(trace=1), error_type="prop")
toptim_diff <- optim(logqinit, evalQ, control=list(trace=1), error_type="diff")
finalq_prop <- exp(toptim_prop$par)
finalq_diff <- exp(toptim_diff$par)

# Project with final values of Q
catch.q(fisheries_temp[["BT2"]][[1]])["alpha",] <- finalq_prop[1]
catch.q(fisheries_temp[["BT2"]][[2]])["alpha",] <- finalq_prop[2]
catch.q(fisheries_temp[["TR1"]][[1]])["alpha",] <- finalq_prop[3]
catch.q(fisheries_temp[["TR1"]][[2]])["alpha",] <- finalq_prop[4]
run_prop <- fwd(object=biols, fishery=fisheries_temp, control=eff_ctrl)

catch.q(fisheries_temp[["BT2"]][[1]])["alpha",] <- finalq_diff[1]
catch.q(fisheries_temp[["BT2"]][[2]])["alpha",] <- finalq_diff[2]
catch.q(fisheries_temp[["TR1"]][[1]])["alpha",] <- finalq_diff[3]
catch.q(fisheries_temp[["TR1"]][[2]])["alpha",] <- finalq_diff[4]
run_diff <- fwd(object=biols, fishery=fisheries_temp, control=eff_ctrl)

flfs_prop <- run_prop[["fisheries"]]
flfs_diff <- run_diff[["fisheries"]]
biols_prop <- run_prop[["biols"]]
biols_diff <- run_diff[["biols"]]

# Build a big data.frame of results - pretty horrible - dump into funcs.R?
# Observed, estimated with prop error, estimated with diff error

pdat <- rbind(
cbind(metric="prop_hat", stock="Ple", fishery="BT2",as.data.frame(landings(flfs_prop[["BT2"]][[1]]))) ,
cbind(metric="diff_hat", stock="Ple", fishery="BT2",as.data.frame(landings(flfs_diff[["BT2"]][[1]]))) ,
cbind(metric="obs", stock="Ple", fishery="BT2",as.data.frame(obslan_bt2_ple)),
cbind(metric="prop_hat", stock="Ple", fishery="TR1",as.data.frame(landings(flfs_prop[["TR1"]][[1]]))),
cbind(metric="diff_hat", stock="Ple", fishery="TR1",as.data.frame(landings(flfs_diff[["TR1"]][[1]]))),
cbind(metric="obs", stock="Ple", fishery="TR1",as.data.frame(obslan_tr1_ple)),
cbind(metric="prop_hat", stock="Sol", fishery="BT2",as.data.frame(landings(flfs_prop[["BT2"]][[2]]))),
cbind(metric="diff_hat", stock="Sol", fishery="BT2",as.data.frame(landings(flfs_diff[["BT2"]][[2]]))),
cbind(metric="obs", stock="Sol", fishery="BT2",as.data.frame(obslan_bt2_sol)),
cbind(metric="prop_hat", stock="Sol", fishery="TR1",as.data.frame(landings(flfs_prop[["TR1"]][[2]]))),
cbind(metric="diff_hat", stock="Sol", fishery="TR1",as.data.frame(landings(flfs_diff[["TR1"]][[2]]))),
cbind(metric="obs", stock="Sol", fishery="TR1",as.data.frame(obslan_tr1_sol)))

# Cut off first year and unwanted columns
pdat <- subset(pdat, year > 2004)
pdat <- pdat[,c("metric", "stock", "fishery", "year", "data")]

# View the fits
# Landings
p <- ggplot(pdat, aes(x=year, y=data)) + geom_line(aes(colour=metric))+ facet_grid(stock~fishery, scales="free")
#p <- ggplot(pdat, aes(x=year, y=data)) + geom_line(aes(colour=metric))+ facet_wrap(~fishery + stock, scales="free", ncol=1)
p + xlab("Year") + ylab("Landings")

# Difference between the fits is marginal - prop on sole TR1 is better

# Biomass
tsb <- get_tsb(run_prop[["biols"]][c("ple","sol")])
ggplot(subset(as.data.frame(tsb), year>2004), aes(x=year, y=data)) + geom_line() + facet_wrap(~qname, scales="free", ncol=1)

ssb <- get_ssb(run_prop[["biols"]][c("ple","sol")])
ggplot(subset(as.data.frame(ssb), year>2004), aes(x=year, y=data)) + geom_line() + facet_wrap(~qname, scales="free", ncol=1)

# F - looks bad
pf_ple_bt2 <- get_f(run_prop, fn=1, cn=1, bn=1, age_range = range(ple)[c("minfbar","maxfbar")])
pf_ple_tr1 <- get_f(run_prop, fn=2, cn=1, bn=1, age_range = range(ple)[c("minfbar","maxfbar")])
f_ple <- pf_ple_bt2 + pf_ple_tr1

pf_sol_bt2 <- get_f(run_prop, fn=1, cn=2, bn=2, age_range = range(sol)[c("minfbar","maxfbar")])
pf_sol_tr1 <- get_f(run_prop, fn=2, cn=2, bn=2, age_range = range(sol)[c("minfbar","maxfbar")])
f_sol <- pf_sol_bt2 + pf_sol_tr1


#---------------------------------------------
# Not enough info for fitting,
# Stock abundance high + q low gives same catches as stock abundance low + q high
# But Fs are different
# Need some additional info on either F or abundance

# Tune to assessment results too
load("../data/biology_and_selectivity_basic_data.Rdata")
# Plaice
ple_ass <- ass.stockOrigDisc
# Sole
sol_ass <- ass.stock

# Update initial N in 2004 with assessment
# And tune to estimated F
n(biols[["ple"]])[,"2004"] <- stock.n(ple_ass)[,"2004"]
n(biols[["sol"]])[,"2004"] <- stock.n(sol_ass)[,"2004"]

fit_years <- 2010:2015

# Copy to mess about with 
fisheries_temp <- fisheries

# Function for optim - based on log Qs - stop negative values
evalQ <- function(log_qs, error_type){
    qs <- exp(log_qs)
    cat("Guess at q:", qs, "\n")
    # Update qs
    catch.q(fisheries_temp[["BT2"]][[1]])["alpha",] <- qs[1]
    catch.q(fisheries_temp[["BT2"]][[2]])["alpha",] <- qs[2]
    catch.q(fisheries_temp[["TR1"]][[1]])["alpha",] <- qs[3]
    catch.q(fisheries_temp[["TR1"]][[2]])["alpha",] <- qs[4]
    # Project
    run <- fwd(object=biols, fishery=fisheries_temp, control=eff_ctrl)
    # Get predicted landings
    lhat11 <- landings(run[["fisheries"]][["BT2"]][[1]]) 
    lhat12 <- landings(run[["fisheries"]][["BT2"]][[2]])
    lhat21 <- landings(run[["fisheries"]][["TR1"]][[1]])
    lhat22 <- landings(run[["fisheries"]][["TR1"]][[2]])
    # Get predicted F
    pfhat111 <- get_f(run, 1, 1, 1, age_range=c(2,6))
    pfhat211 <- get_f(run, 2, 1, 1, age_range=c(2,6))
    fhatple <- pfhat111 + pfhat211
    pfhat122 <- get_f(run, 1, 2, 2, age_range=c(2,6))
    pfhat222 <- get_f(run, 2, 2, 2, age_range=c(2,6))
    fhatsol <- pfhat122 + pfhat222
    # Create error term
    if (error_type == "prop"){ 
        error_lan11 <- sum(abs(c(log((lhat11/ obslan_bt2_ple)[,ac(fit_years)])))) 
        error_lan12 <- sum(abs(c(log((lhat12/ obslan_bt2_sol)[,ac(fit_years)])))) 
        error_lan21 <- sum(abs(c(log((lhat21/ obslan_tr1_ple)[,ac(fit_years)]))))
        error_lan22 <- sum(abs(c(log((lhat22/ obslan_tr1_sol)[,ac(fit_years)]))))
        error_fple <- sum(abs(c(log(fhatple[,ac(fit_years)] / fbar(ple_ass)[,ac(fit_years)]))))
        error_fsol <- sum(abs(c(log(fhatsol[,ac(fit_years)] / fbar(sol_ass)[,ac(fit_years)]))))
    }
    if (error_type == "diff"){ 
        error_lan11 <- sum(abs(c(((lhat11 - obslan_bt2_ple)[,ac(fit_years)])))) 
        error_lan12 <- sum(abs(c(((lhat12 - obslan_bt2_sol)[,ac(fit_years)])))) 
        error_lan21 <- sum(abs(c(((lhat21 - obslan_tr1_ple)[,ac(fit_years)]))))
        error_lan22 <- sum(abs(c(((lhat22 - obslan_tr1_sol)[,ac(fit_years)]))))
        error_fple <- sum(abs(c(fhatple[,ac(fit_years)] - fbar(ple_ass)[,ac(fit_years)])))
        error_fsol <- sum(abs(c(fhatsol[,ac(fit_years)] - fbar(sol_ass)[,ac(fit_years)])))
    }
    # Sum catch errors to build single error term
    error <- error_lan11 + error_lan12 + error_lan21 + error_lan22 + error_fple + error_fsol
    cat("error: ", error, "\n")
    return(error)
}

# Initial value of Q on log scale
logqinit <-  log(c(c(fisheries[[1]][[1]]@catch.q["alpha",]), c(fisheries[[1]][[2]]@catch.q["alpha",]), c(fisheries[[2]][[1]]@catch.q["alpha",]), c(fisheries[[1]][[2]]@catch.q["alpha",])))
# Run optim - simple Nelder Mead with max 500 iters
toptim_prop <- optim(logqinit, evalQ, control=list(trace=1), error_type="prop")
finalq_prop <- exp(toptim_prop$par)

# Project with final values of Q
catch.q(fisheries_temp[["BT2"]][[1]])["alpha",] <- finalq_prop[1]
catch.q(fisheries_temp[["BT2"]][[2]])["alpha",] <- finalq_prop[2]
catch.q(fisheries_temp[["TR1"]][[1]])["alpha",] <- finalq_prop[3]
catch.q(fisheries_temp[["TR1"]][[2]])["alpha",] <- finalq_prop[4]
run_prop <- fwd(object=biols, fishery=fisheries_temp, control=eff_ctrl)

flfs_prop <- run_prop[["fisheries"]]
biols_prop <- run_prop[["biols"]]

# Build a big data.frame of results - pretty horrible - dump into funcs.R?
# Observed, estimated with prop error, estimated with diff error

pdat <- rbind(
cbind(metric="prop_hat", stock="Ple", fishery="BT2",as.data.frame(landings(flfs_prop[["BT2"]][[1]]))) ,
cbind(metric="obs", stock="Ple", fishery="BT2",as.data.frame(obslan_bt2_ple)),
cbind(metric="prop_hat", stock="Ple", fishery="TR1",as.data.frame(landings(flfs_prop[["TR1"]][[1]]))),
cbind(metric="obs", stock="Ple", fishery="TR1",as.data.frame(obslan_tr1_ple)),
cbind(metric="prop_hat", stock="Sol", fishery="BT2",as.data.frame(landings(flfs_prop[["BT2"]][[2]]))),
cbind(metric="obs", stock="Sol", fishery="BT2",as.data.frame(obslan_bt2_sol)),
cbind(metric="prop_hat", stock="Sol", fishery="TR1",as.data.frame(landings(flfs_prop[["TR1"]][[2]]))),
cbind(metric="obs", stock="Sol", fishery="TR1",as.data.frame(obslan_tr1_sol)))

# Cut off first year and unwanted columns
pdat <- subset(pdat, year > 2004)
pdat <- pdat[,c("metric", "stock", "fishery", "year", "data")]

# View the fits
# Landings
p <- ggplot(pdat, aes(x=year, y=data)) + geom_line(aes(colour=metric))+ facet_grid(stock~fishery, scales="free")
#p <- ggplot(pdat, aes(x=year, y=data)) + geom_line(aes(colour=metric))+ facet_wrap(~fishery + stock, scales="free", ncol=1)
p + xlab("Year") + ylab("Landings")

# Difference between the fits is marginal - prop on sole TR1 is better

ggplot(effbygearyear, aes(x=year, y=Effective.Effort)) + geom_line(aes(colour=regulated.gear))

# Biomass
tsb <- get_tsb(run_prop[["biols"]][c("ple","sol")])
ggplot(subset(as.data.frame(tsb), year>2004), aes(x=year, y=data)) + geom_line() + facet_wrap(~qname, scales="free", ncol=1)

ssb <- get_ssb(run_prop[["biols"]][c("ple","sol")])
ggplot(subset(as.data.frame(ssb), year>2004), aes(x=year, y=data)) + geom_line() + facet_wrap(~qname, scales="free", ncol=1)

# F
pf_ple_bt2 <- get_f(run_prop, fn=1, cn=1, bn=1, age_range = range(ple)[c("minfbar","maxfbar")])
pf_ple_tr1 <- get_f(run_prop, fn=2, cn=1, bn=1, age_range = range(ple)[c("minfbar","maxfbar")])
f_ple <- pf_ple_bt2 + pf_ple_tr1

pf_sol_bt2 <- get_f(run_prop, fn=1, cn=2, bn=2, age_range = range(sol)[c("minfbar","maxfbar")])
pf_sol_tr1 <- get_f(run_prop, fn=2, cn=2, bn=2, age_range = range(sol)[c("minfbar","maxfbar")])
f_sol <- pf_sol_bt2 + pf_sol_tr1




