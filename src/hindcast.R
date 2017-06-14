# Attempt to perform a hindcast based on landings of place and sole, and effort of BT2 and TR1
rm(list=ls())
library(FLasher)
library(ggplot2)

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
#load("../data/biology_and_selectivity_basic_data.Rdata")
load("../data/OM.Rdata")
#---------------------------------------------------
# Need to make objects with the range 2004:2015

# Initial conditions:
# Based on stock assessment
# Or abundance at MSY

# Fix recruitment at levels estimated from the SA
# e.g. 
biols2[[1]]@rec@params <- FLPar(rec, dimnames=list(params="a", year=1:10, iter=1))

names(biols)
biols[[1]]


#---------------------------------------------------
# Option 1
# Project with effort - look at landings




