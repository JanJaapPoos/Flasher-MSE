# Get partial F from OP from FLasher
# Recreates the F equation in the C++ code in FLasher:
# pF = Sel * Q * effort
# Does not check if FC actually fishes B
get_f <- function(op, fn=1, cn=1, bn=1, age_range = c(2,6)){
    # f = alpha * sel * effort
    flf <- op[["fisheries"]][[fn]]
    flc <- flf[[cn]]
    b <- op[["biols"]][[bn]]
    f <- (flc@catch.q[1,] * flf@effort) %*% flc@catch.sel
    fbar <- apply(f[age_range[1]:age_range[2],],2:6,mean)
    return(fbar)
}

get_tsb <- function(biols){
    lapply(biols, function(x) {
           return(quantSums(n(x) * wt(x)))
    })
}

get_ssb <- function(biols){
    lapply(biols, function(x) {
           return(quantSums(mat(x) * n(x) * wt(x)))
    })
}


extend_flbiols <- function(flbs, final_year, nyears=3){
    # last available year - not to be confused with final_year
    last_year <- as.numeric(dimnames(flbs[[1]]@n)$year[length(dimnames(flbs[[1]]@n)$year)])
    mean_years <- ac((last_year - nyears + 1):last_year)
    future_years <- ac((last_year+1):final_year)
    new_flbs <- window(flbs, end=final_year)
    # Need to add data for:
    # m, wt, spwn
    new_flbs <- lapply(new_flbs, function(flb){
        flb@spwn[,future_years] <- apply(flb@spwn[,mean_years], c(1,3:6), mean)
        flb@wt[,future_years] <- apply(flb@wt[,mean_years], c(1,3:6), mean)
        flb@m[,future_years] <- apply(flb@m[,mean_years], c(1,3:6), mean)
        # Special treatment for predictModels - rec, fec and mat
        flb@mat@.Data[[1]] <- window(flb@mat@.Data[[1]], end=final_year)
        flb@mat@.Data[[1]][,future_years] <- apply(flb@mat@.Data[[1]][,mean_years], c(1,3:6), mean)
        flb@rec@.Data[[1]] <- window(flb@rec@.Data[[1]], end=final_year)
        flb@rec@.Data[[1]][,future_years] <- apply(flb@rec@.Data[[1]][,mean_years], c(1,3:6), mean)
        flb@fec@.Data[[1]] <- window(flb@fec@.Data[[1]], end=final_year)
        flb@fec@.Data[[1]][,future_years] <- apply(flb@fec@.Data[[1]][,mean_years], c(1,3:6), mean)
        return(flb)
    })
    return(new_flbs)
}

extend_flfisheries <- function(flfs, final_year, nyears=3){
    # last available year - not to be confused with final_year
    last_year <- as.numeric(dimnames(flfs[[1]][[1]]@landings.n)$year[length(dimnames(flfs[[1]][[1]]@landings.n)$year)])
    mean_years <- ac((last_year - nyears + 1):last_year)
    future_years <- ac((last_year+1):final_year)
    new_flfs <- lapply(flfs, function(flf){
        # flf is a FLFisheryCpp
        # flcs is FLCatches
        flcs <- lapply(flf, function(flc){
            flc <- window(flc, end=final_year)
            flc@landings.n[,future_years] <- apply(flc@landings.n[,mean_years], c(1,3:6), mean)
            flc@landings.wt[,future_years] <- apply(flc@landings.wt[,mean_years], c(1,3:6), mean)
            flc@discards.n[,future_years] <- apply(flc@discards.n[,mean_years], c(1,3:6), mean)
            flc@discards.wt[,future_years] <- apply(flc@discards.wt[,mean_years], c(1,3:6), mean)
            flc@catch.sel[,future_years] <- apply(flc@catch.sel[,mean_years], c(1,3:6), mean)
            flc@price[,future_years] <- apply(flc@price[,mean_years], c(1,3:6), mean)
            return(flc)
        })
        new_flf <- FLFishery(flcs)
        ## Hack to fix range of FLFishery - because capacity etc are empty
        #year_range <- range(as.numeric(dimnames(flf@effort)$year))
        #flf@range[c("minyear", "maxyear")] <- year_range
        # Window of FLFishery is broken when FLQs have 1 year so we do each one by hand
        new_flf@effort <- window(flf@effort, end=final_year)
        new_flf@effort[,future_years] <- apply(flf@effort[,mean_years], c(1,3:6), mean)
        new_flf@hperiod <- window(flf@hperiod, end=final_year)
        new_flf@hperiod[,future_years] <- apply(flf@hperiod[,mean_years], c(1,3:6), mean)
        new_flf@vcost <- window(flf@vcost, end=final_year)
        new_flf@vcost[,future_years] <- apply(flf@vcost[,mean_years], c(1,3:6), mean)
        new_flf@fcost <- window(flf@fcost, end=final_year)
        new_flf@fcost[,future_years] <- apply(flf@fcost[,mean_years], c(1,3:6), mean)
        # Do all FLCatches of the FLFishery
        return(new_flf)
    })
    return(new_flfs)

}




