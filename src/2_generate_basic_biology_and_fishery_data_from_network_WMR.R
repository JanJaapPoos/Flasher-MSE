rm(list=ls())

library("devtools");library("FLife");library(FLBRP);library(stockassessment)
options(devtools.install.args = "--no-multiarch")   
#install_github("flr/Flife")
#install_github("flr/FLCore")

load("w:\\IMARES\\Data\\ICES-WG\\Demersale werkgroep WGNSSK\\2017\\stock\\ple-comb\\04_Final_runs_2017\\ple_AAP_Workspace_final.Rdata")
srple <- as.FLSR(ass.stockOrigDisc, model="bevholt")
srple1 <- fmle(srple,control=list(maxit=3000))
params(srple1)
plot(srple1)

load("w:\\IMARES\\Data\\ICES-WG\\Demersale werkgroep WGNSSK\\2017\\stock\\sol-nsea\\final_run\\output\\_ass.stock_Results.Rdata")
srsol <- as.FLSR(ass.stock, model="ricker")
srsol1 <- fmle(srsol,control=list(maxit=3000))
params(srsol1)
plot(srsol1)

#tur sr comes from prelim fit SAM tur in object that is called "fit"
setwd("d:\\tur-nsea\\lowestoft files\\")
cn<-read.ices("canum.txt")
cw<-read.ices("weca.txt")
dw<-read.ices("weca.txt")
lw<-read.ices("weca.txt")
mo<-read.ices("matprop.txt")
nm<-read.ices("natmor.txt")
pf<-read.ices("fprop.txt")
pm<-read.ices("mprop.txt")
sw<-read.ices("west.txt")
lf<-read.ices("lf.txt")

surveys<-read.ices("fleet_tmb.txt")

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn, 
                    prop.mature=mo, 
                    stock.mean.weight=sw, 
                    catch.mean.weight=cw, 
                    dis.mean.weight=dw, 
                    land.mean.weight=lw,
                    prop.f=pf, 
                    prop.m=pm, 
                    natural.mortality=nm, 
                    land.frac=lf)

conf<-defcon(dat)
conf$keyLogFsta[1,] <- c(0,    1,    2,    3,    4,    5,    6,    7,    7,     7)

conf$keyLogFpar[2,] <- c(0,    1 ,   2  ,  3 ,   4 ,   4  ,  4,   -1,   -1,    -1)
conf$keyLogFpar[3,] <- c(5,    6 ,   7  ,  8 ,   9 ,   9  ,  9,   -1,   -1,    -1)
conf$keyLogFpar[4,] <- c(10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    -1)

conf$keyVarF[1,] <- c(0, 1, 2, 2, 2, 2, 2, 2, 2, 2)

conf$keyVarLogN <-  c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1)

conf$keyVarObs[1,]<-c(0,1,1,1,1,1,1,1,1,1)
conf$keyVarObs[2,]<-c(2,2,2,2,2,2,2,-1,-1,-1)
conf$keyVarObs[3,]<-c(3,3,3,3,3,3,3,-1,-1,-1)
conf$keyVarObs[4,]<-c(4,-1,-1,-1,-1,-1,-1,-1,-1,-1)

conf$obsCorStruct[]<-c("ID","AR","AR","ID")
conf$keyCorObs[2,]<-c(0,0,0,0,0,0,-1,-1,-1)
conf$keyCorObs[3,]<-c(1,1,1,1,1,1,-1,-1,-1)
conf$fbarRange<-c(2,6)
conf$corFlag<-2
par<-defpar(dat,conf)
fit<-sam.fit(dat,conf,par)


summary(fit)[,1]
summary(fit)[,4]
srtur <- FLSR(ssb=FLQuant(c(summary(fit)[,4]), dimnames=list(age="all", year= names(summary(fit)[,4]))),
              rec=FLQuant(c(summary(fit)[,1]), dimnames=list(age="all", year= names(summary(fit)[,4]))),
              model = "ricker")
srtur1 <- fmle(srtur,control=list(maxit=3000))
params(srtur1)
plot(srtur1)

plaice   <- FLPar("linf"=40,"k"=0.31, "t0"=0, "a"=0.00890,"b"=3.053,"a50"=2.5,"ato95"=1.5)
sole     <- FLPar("linf"=35,"k"=0.34, "t0"=0, "a"=0.00762,"b"=3.068,"a50"=2,"ato95"=1) 
turbot   <- FLPar("linf"=55,"k"=0.38, "t0"=0, "a"=0.01508,"b"=3.090,"a50"=3,"ato95"=1) 
brill    <- FLPar("linf"=51,"k"=0.43, "t0"=0, "a"=0.02492,"b"=2.857,"L50"=25,"ato95"=1) 

ple <- window(as(lhEql(lhPar(plaice), m=0.1, spwn=0, range=c(min = 1, max = 15, minfbar = 2, maxfbar = 6, plusgroup = 20)),"FLStock"), end=10)
sol <- window(as(lhEql(lhPar(sole),   m=0.1, spwn=0, range=c(min = 1, max = 15, minfbar = 2, maxfbar = 6, plusgroup = 20)),"FLStock"), end=10)
tur <- window(as(lhEql(lhPar(turbot), m=0.2, spwn=0, range=c(min = 1, max = 15, minfbar = 2, maxfbar = 6, plusgroup = 20)),"FLStock"), end=10)
bll <- window(as(lhEql(lhPar(brill),  m=0.2, spwn=0, range=c(min = 1, max = 15, minfbar = 2, maxfbar = 6, plusgroup = 20)),"FLStock"), end=10)

#set up selectivities for fleets

L50sol80  <- 3.4  * 80  
L50ple80  <- 2.4  * 80  
L50sol100  <- 3.4  * 100  
L50ple100  <- 2.4  * 100  

a50ple80  = log(1 - ((L50ple80/10)/plaice["linf"])) / (-plaice["k"])
a50sol80  = log(1 - ((L50sol80/10)/sole["linf"])) / (-sole["k"])
a50ple100 = log(1 - ((L50ple100/10)/plaice["linf"])) / (-plaice["k"])
a50sol100 = log(1 - ((L50sol100/10)/sole["linf"])) / (-sole["k"])

a50tur80  = log(1 - ((L50ple80/10)/turbot["linf"])) / (-turbot["k"])
a50bll80  = log(1 - ((L50ple80/10)/brill["linf"])) / (-brill["k"])
a50tur100 = log(1 - ((L50ple100/10)/turbot["linf"])) / (-turbot["k"])
a50bll100 = log(1 - ((L50ple100/10)/brill["linf"])) / (-brill["k"])

selple80 <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50ple80,sl=1,sr=5)) )
selsol80 <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50sol80,sl=1,sr=10)) )   

selple100  <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50ple100,sl=1,sr=5)) )   
selsol100  <-(FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50sol100,sl=1,sr=10)) )

seltur80 <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50ple80,sl=1,sr=5)) )
selbll80 <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50sol80,sl=1,sr=10)) )   

seltur100  <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50ple100,sl=1,sr=5)) )   
selbll100  <-(FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50sol100,sl=1,sr=10)) )

################################################################
# Discarding stuff
################################################################

L50pleD  <- 27  
L50solD  <- 24  

L50turD  <- 30  
L50bllD  <- 30  

a50pleD  = log(1 - (L50pleD/plaice["linf"])) / (-plaice["k"])
a50solD  = log(1 - (L50solD/sole["linf"])) / (-sole["k"])
a50turD  = log(1 - (L50turD/turbot["linf"])) / (-turbot["k"])
a50bllD  = log(1 - (L50bllD/brill["linf"])) / (-brill["k"])

Dple <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50pleD,sl=0.1,sr=50000)) )   
Dsol <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50solD,sl=0.1,sr=50000)) )

Dtur <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50turD,sl=0.1,sr=50000)) )   
Dbll <- (FLife::dnormal(FLQuant(1:15,dimnames=list(age=1:15)),params=FLPar(a1=a50bllD,sl=0.1,sr=50000)) )   

save.image("w:\\IMARES\\IJmuiden\\afdeling\\projecten\\data poor mixed fisheries\\JRC\\biology_and_seletivity_basic_data.Rdata")
