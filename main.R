library(OrvacRCT)
library(configr)
library(doParallel)
library(foreach)
library(optparse)
library(data.table)
source("setup.R")


play_interims <- function(){
  
  library(OrvacRCT)
  library(configr)
  library(doParallel)
  library(foreach)
  library(optparse)
  library(data.table)
  source("setup.R")

  cfg <- get_cfg(print = F)
  d <- get_interims(cfg)
  
  d2 <- data.table(d)
  names(d2) <- c("idx_start", "idx_end", "t_start", "t_end", "n")
  d2[, t_diff:= t_end - t_start]
  d2
}

play_setstate <- function(){
  
  library(OrvacRCT)
  library(configr)
  library(doParallel)
  library(foreach)
  library(optparse)
  library(data.table)
  source("setup.R")

  cfg <- get_cfg(print = F)
  d <- get_interims(cfg)
  d2 <- data.table(d)
  names(d2) <- c("idx_start", "idx_end", "t_start", "t_end", "n")
  d2[, t_diff:= t_end - t_start]
  d2
  
  # set state based on first interim
  l <- test_set_state(cfg, d2[1, n], d2[1, t_end], F, 0)
  
  l <- test_set_state(cfg, d2[1, n], d2[1, t_end], T, 0)
  
}


main <- function(){

  library(OrvacRCT)
  library(configr)
  library(doParallel)
  library(foreach)
  library(optparse)
  library(data.table)
  source("setup.R")
  
  pkgs <- c("data.table", "futile.logger", "configr", "survival", 
           "foreach", "truncnorm", "beepr", "orvacsim")


  starttime <- Sys.time()
  
  # initiate clusters
  debug = T
  cl <- NA
  if(!debug){
    cl <- makeCluster(parallel::detectCores() - 2, outfile="")
    # cl <- makeCluster(3, outfile="")
    registerDoParallel(cl)
  } else {
    registerDoSEQ()
  }
  
  cfg <- get_cfg(print = F)
  cfg$n_sims <- 1
  # cfg$post_draw <- 1000
  
  results <- foreach(i = 1:cfg$n_sims,
                   .errorhandling = 'pass',
                   .packages=pkgs
                   #.options.snow=opts,
                   ) %dopar%{
    
    # i = 1
    set.seed(cfg$seed + i)
    
    res <- simulate_trial(i, cfg, T)
  
    # flog.info("Finished trial: sim = %s", i)
    return(res)
  }
#results[[1]]
  # dfres1 <- data.frame()
  # dfres2 <- data.frame()
  # 
  # for(i in 1:length(results)){
  #   myv <- unlist(results[[i]][1:25])
  #   nm <- names(myv)
  #   dfres1 <- rbind(dfres1, myv)
  #   colnames(dfres1) <- nm
  # }

  endtime <- Sys.time()
  difftime(endtime, starttime, units = "hours")

  beepr::beep()
  w <- warnings()
  
  rdsfilename <- file.path("out", 
    paste0("res-",
      format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".RDS"))

  saveRDS(list(results=results, 
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
