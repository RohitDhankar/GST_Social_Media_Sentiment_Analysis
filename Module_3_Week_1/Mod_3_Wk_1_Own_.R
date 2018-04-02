library(parallel)
library(MASS)
library("parallel", lib.loc="C:/Program Files/R/R-3.1.3/library")
library("MASS", lib.loc="C:/Program Files/R/R-3.1.3/library")
RNGkind("L'Ecuyer-CMRG")
# str(RNGkind)
# RNGkind
mc.cores <- detectCores()
mc.cores
results <- mclapply(rep(25, 4),
                    function(nstart) kmeans(Boston, 4, nstart=nstart),
                    mc.cores=mc.cores)
i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]

##
# Rohit -- Jumping to pbdDemo Stuck --- package ‘pbdDEMO’ is not available (as a binary package for R version 3.1.3)
## 
# library(pbdDEMO, quietly = TRUE)
# init.grid()
# 
# 
# if(comm.size() != 4){
#   comm.stop("This example requries 4 processors.")
# }
# 
# 
# # Read in example csv file
# x <- system.file("extra/data/x.csv", package = "pbdDEMO")
