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

# early interim, no fu - reftime is the time of interim for all
play_censoring_nofu1 <- function(){
  
  library(OrvacRCT);  library(configr); library(optparse); library(data.table);  source("setup.R")
  set.seed(1); cfg <- get_cfg(print = F)
  # set state based on first interim
  cur_intrm_idx = 6
  ret <- test_set_state(cfg, dofu = F, cur_intrm_idx, n_target = 0, ref_time = 0)
  dat <- data.table(ret$dat);   names(dat) <- cfg$dnames
  dat[, acc_plus_evtt:= accrt + evtt]
  dat[, age_plus_evtt:= age + evtt]
  dat[, time_of_maxfu:= accrt + cfg$max_age_fu - age]
  # dat[, evtt_b4_ref:= acc_plus_evtt < refime]
  # dat[, age_b4_ref:= acc_plus_evtt < refime]
  intrms2 <- data.table(ret$intrm);   names(intrms2) <- c("idx_start", "idx_end", "t_start", "t_end", "n")
  intrms2[, t_diff:= t_end - t_start]
  intrms2[cur_intrm_idx+1,]
}

# late interim, no fu - reftime is the time of interim for all
play_censoring_nofu2 <- function(){
  
  library(OrvacRCT);  library(configr); library(optparse); library(data.table);  source("setup.R")
  set.seed(1); cfg <- get_cfg(print = F)
  # set state based on first interim
  cur_intrm_idx = 17
  ret <- test_set_state(cfg, dofu = F, cur_intrm_idx, n_target = 0, ref_time = 0)
  dat <- data.table(ret$dat);   names(dat) <- cfg$dnames
  dat[, acc_plus_evtt:= accrt + evtt]
  dat[, age_plus_evtt:= age + evtt]
  dat[, time_of_maxfu:= accrt + cfg$max_age_fu - age]
  # dat[, evtt_b4_ref:= acc_plus_evtt < refime]
  # dat[, age_b4_ref:= acc_plus_evtt < refime]
  intrms2 <- data.table(ret$intrm);   names(intrms2) <- c("idx_start", "idx_end", "t_start", "t_end", "n")
  intrms2[, t_diff:= t_end - t_start]
  intrms2[cur_intrm_idx+1,]
}

# final at max n - reftime is max fu time for each sub
play_censoring_dofu1 <- function(){
  
  # at max sample size for a final 
  library(OrvacRCT);  library(configr); library(optparse); library(data.table);  source("setup.R")
  set.seed(2); cfg <- get_cfg(print = F)
  # cur_intrm_idx is irrelevant as n_target is provided and reftime computed
  cur_intrm_idx = 17
  ret <- test_set_state(cfg, dofu = T, cur_intrm_idx, n_target = 1000, ref_time = 0)
  dat <- data.table(ret$dat);  names(dat) <- cfg$dnames
  dat[, acc_plus_evtt:= accrt + evtt]
  dat[, age_plus_evtt:= age + evtt]
  dat[, time_of_maxfu:= accrt + cfg$max_age_fu - age]
}

# final at enrolled < nmax - reftime is max fu time for each sub
play_censoring_dofu2 <- function(){
  
  # at max sample size for a final 
  library(OrvacRCT);  library(configr); library(optparse); library(data.table);  source("setup.R")
  set.seed(2); cfg <- get_cfg(print = F)
  # cur_intrm_idx is irrelevant as n_target is provided and reftime computed
  cur_intrm_idx = 17
  # ref_time set to zero so that all are computed to max fu
  ret <- test_set_state(cfg, dofu = T, cur_intrm_idx, n_target = 605, ref_time = 0)
  dat <- data.table(ret$dat);  names(dat) <- cfg$dnames
  dat[, acc_plus_evtt:= accrt + evtt]
  dat[, age_plus_evtt:= age + evtt]
  dat[, time_of_maxfu:= accrt + cfg$max_age_fu - age]
}

# interim at < nmax - reftime is max fu time for each sub
play_censoring_dofu3 <- function(){
  
  # at max sample size for a final 
  library(OrvacRCT);  library(configr); library(optparse); library(data.table);  source("setup.R")
  set.seed(3); cfg <- get_cfg(print = F)
  # cur_intrm_idx is irrelevant as n_target is provided and reftime computed
  cur_intrm_idx = 17
  # ref_time set to zero so that all are computed to max fu
  ret <- test_set_state(cfg, dofu = T, cur_intrm_idx, n_target = 569, ref_time = 58.06967)
  dat <- data.table(ret$dat);  names(dat) <- cfg$dnames
  dat[, acc_plus_evtt:= accrt + evtt]
  dat[, age_plus_evtt:= age + evtt]
  dat[, time_of_maxfu:= accrt + cfg$max_age_fu - age]
  
  intrms2 <- data.table(ret$intrm);   names(intrms2) <- c("idx_start", "idx_end", "t_start", "t_end", "n")
  intrms2[, t_diff:= t_end - t_start]
  intrms2[cur_intrm_idx+1,]
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
           "foreach", "truncnorm", "beepr", "OrvacRCT")


  starttime <- Sys.time()
  
  # initiate clusters
  debug = F
  cl <- NA
  if(!debug){
    cl <- makeCluster(parallel::detectCores() - 2, outfile="")
    # cl <- makeCluster(3, outfile="")
    registerDoParallel(cl)
  } else {
    registerDoSEQ()
  }
  
  cfg <- get_cfg(print = F)

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
