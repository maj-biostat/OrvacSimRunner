library(OrvacRCT)
library(configr)
library(doParallel)
library(foreach)
library(optparse)
library(data.table)
source("setup.R")



main <- function(){

  library(OrvacRCT)
  library(configr)
  library(doParallel)
  library(foreach)
  library(optparse)
  library(data.table)
  source("setup.R")
  
  pkgs <- c("data.table", "futile.logger", "configr", "survival", 
           "foreach", "truncnorm", "beepr", "OrvacRCT")


  starttime <- Sys.time()
  
  # initiate clusters
  debug = F
  cl <- NA
  if(!debug){
    cl <- makeCluster(parallel::detectCores() - 4, outfile="")
    # cl <- makeCluster(3, outfile="")
    registerDoParallel(cl)
  } else {
    registerDoSEQ()
  }
  
  cfg <- get_cfg(print = F)
  if(debug){  
    cfg$n_sims = 1; 
    cfg$sero_info_delay = 0.5;  
    cfg$post_draw = 10
  }

  results <- foreach(i = 1:cfg$n_sims,
                   .errorhandling = 'pass',
                   .packages=pkgs
                   #.options.snow=opts,
                   ) %dopar%{
    
    # i = 1
    set.seed(cfg$seed + i)

    if(debug) {set.seed(1)}
    
    res <- simulate_trial(i, cfg, T)
  
    # flog.info("Finished trial: sim = %s", i)
    return(res)
  }

  dfres1 <- data.frame()
  dfres2 <- data.frame()

  for(i in 1:length(results)){
    myv <- unlist(results[[i]][1:21])
    nm <- names(myv)
    
    dfres1 <- rbind(dfres1, myv)
    colnames(dfres1) <- nm
  }
  


  endtime <- Sys.time()
  difftime(endtime, starttime, units = "hours")

  beepr::beep()
  w <- warnings()
  
  rdsfilename <- file.path("out", 
    paste0("res-",
      format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".RDS"))

  saveRDS(list(results=dfres1, 
    cfg = cfg, 
    warnings = w,
    starttime = starttime, 
    endtime = endtime,
    duration = difftime(endtime, starttime, units = "hours")),
    rdsfilename)
  # assign("last.warning", NULL, envir = baseenv())
  
  if(!debug){
    stopCluster(cl)
  }

  
}

construct_interims <- function(){

  cfg <- get_cfg(print = F)
  cfg$accrual <- 60
  
  d <- get_trial_dat(cfg)
  
  d2 <- data.table(d)
  names(d2) <- cfg$dnames
  
  m <- data.table(get_interims(d, cfg))
  names(m) <- c("idx_start", "idx_end", "t_start", "t_end")
  m[, idx_diff := idx_end - idx_start]
  m[, t_diff := t_end - t_start]
  m
}

main()
