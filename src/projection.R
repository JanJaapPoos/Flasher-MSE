# Starting from the results of the hindcast (with the estimated final abundances, and the estimated Qs), project forward doing something
rm(list=ls())
source("funcs.R")

# Load results of hindcast
load("../data/hindcast_om.Rdata")

# Set up objects into the future

final_year <- 2030
biols_proj <- extend_flbiols(biols_hind, final_year)
fisheries_proj <- extend_flfisheries(fisheries_hind, final_year)

# Fix recruitment in biols
biols_proj[["ple"]]@rec@model <- srple1@model
biols_proj[["ple"]]@rec@params <- srple1@params
biols_proj[["sol"]]@rec@model <- srsol1@model
biols_proj[["sol"]]@rec@params <- srsol1@params
biols_proj[["tur"]]@rec@model <- srtur1@model
biols_proj[["tur"]]@rec@params <- srtur1@params
biols_proj[["bll"]]@rec@model <- srtur1@model
biols_proj[["bll"]]@rec@params <- srtur1@params

#------------------------------------------------
# Tets
proj_years <- 2016:2030

# Make effort control
eff_ctrldf <- data.frame(year=rep(proj_years, each=2), quant="effort", fishery=c(1,2), value=NA)
eff_ctrldf[eff_ctrldf$fishery==1,"value"] <- 280000
eff_ctrldf[eff_ctrldf$fishery==2,"value"] <- 750000
eff_ctrl <- fwdControl(eff_ctrldf)
eff_ctrl@FCB <- FCB

# Test run
run <- fwd(object=biols_proj, fishery=fisheries_proj, control=eff_ctrl)
# Extract fishery output
flfs <- run[["fisheries"]]

# Are efforts OK - i.e. did we hit the observed effort?
flfs[["BT2"]]@effort
obseff_bt2
flfs[["TR1"]]@effort
obseff_tr1



pdat <- rbind(
cbind(metric="prop_hat", stock="Ple", fishery="BT2",as.data.frame(landings(flfs[["BT2"]][[1]]))) ,
cbind(metric="obs", stock="Ple", fishery="BT2",as.data.frame(obslan_bt2_ple)),
cbind(metric="prop_hat", stock="Ple", fishery="TR1",as.data.frame(landings(flfs[["TR1"]][[1]]))),
cbind(metric="obs", stock="Ple", fishery="TR1",as.data.frame(obslan_tr1_ple)),
cbind(metric="prop_hat", stock="Sol", fishery="BT2",as.data.frame(landings(flfs[["BT2"]][[2]]))),
cbind(metric="obs", stock="Sol", fishery="BT2",as.data.frame(obslan_bt2_sol)),
cbind(metric="prop_hat", stock="Sol", fishery="TR1",as.data.frame(landings(flfs[["TR1"]][[2]]))),
cbind(metric="obs", stock="Sol", fishery="TR1",as.data.frame(obslan_tr1_sol)))

# Cut off first year and unwanted columns
pdat <- subset(pdat, year > 2004)
pdat <- pdat[,c("metric", "stock", "fishery", "year", "data")]

# View the fits
# Landings
p <- ggplot(pdat, aes(x=year, y=data)) + geom_line(aes(colour=metric))+ facet_grid(stock~fishery, scales="free")
#p <- ggplot(pdat, aes(x=year, y=data)) + geom_line(aes(colour=metric))+ facet_wrap(~fishery + stock, scales="free", ncol=1)
p + xlab("Year") + ylab("Landings")



#--------------------------------------------------------------------
# Set up more projections



# Single TAC plaice - forgone yield on sole 
# Single TAC sole - forgone yield on plaice
# TACs based on 75% (or what ever proportion of total catches is) of MSY
# Or a joint TAC based on .....

# Need to also condition Turbot


# MSY estimated by JJP using the BRP stuff

# For the report:
# Methods:
# conditioning of hindcast
# why are we estimating qs
# fit to partial landings only - not enough info
# fit to partial landings and Fbar
# Recruitment is fixed at estimated recruitment
# Driven by effort

# Results of hindcast:
# Compare OP to single species assessment (which is the 'best' estimate of status) and partial landings
# Differences arise from assessed stock having different weights / maturity / selectivity to those used here
# Why are plaice and sole different to the assessed stock? Why did we use the LH approach?
# T&B fine, because they are data poor, but P&S?





