



get_cmdline_opts <- function(){
  
  option_list <- list(
    make_option(c("-f", "--cfgfile"), type = "character", default = "cfg1.yaml",
                help = "config file name", metavar = "character"),
    make_option(c("-o", "--use"), type = "logical", default = FALSE,
                help = "override config file with command line settings",
                metavar = "logical"),
    make_option(c("-i", "--id_sim"), type = "character", default = FALSE,
                help = "label for current simulation", metavar = "character"),
    make_option(c("-n", "--n_sims"), type = "integer", default = NULL,
                help = "number of simulations", metavar = "integer"),
    make_option(c("-s", "--seed"), type = "integer", default = NULL,
                help = "random seed", metavar = "integer"),
    make_option(c("-a", "--accrual"), type = "integer", default = NULL,
                help = "accrual rate i.e. people_per_interim_period",
                metavar = "integer"),
    make_option(c("-d", "--delay"), type = "double", default = NULL,
                help = "seroconversion information delay",
                metavar = "double"),
    make_option(c("-b", "--basesero"), type = "double", default = NULL,
                help = "baseline seroconversion prob", metavar = "double"),
    make_option(c("-p", "--trtprobsero"), type = "double", default = NULL,
                help = "trt seroconversion prob", metavar = "double"),
    make_option(c("-m", "--basemediantte"), type = "double", default = NULL,
                help = "baseline median time to med attendance (months)",
                metavar = "double"),
    make_option(c("-t", "--trtmedtte"), type = "double", default = NULL,
                help = "treatment arm median time to med attendance (months)",
                metavar = "double")
  )
  
  opt_parser <- OptionParser(option_list = option_list);
  opt <- parse_args(opt_parser);
  opt
  
}


get_cfg <- function(cfgfile = "cfg.yaml", print = T){
  
  opt <- get_cmdline_opts()
  
  if(print == T){
    cat("Options\n")
    print(opt)
    
  }
  
  tt <- tryCatch(configtmp <- read.config(file = file.path(getwd(), cfgfile)),
                 error=function(e) e,
                 warning=function(w) w)
  ifelse(is(tt,"warning") | is(tt,"error"),"Configuration Warning/Error.
         Please ensure configuration file has terminating empty line.",
         "Configuration File Loaded OK")
  
  l <- list()
  
  #l$dnames <- dnames
  if("id_sim" %in% names(opt)){
    l$id_sim <- opt$id_sim
  } else {
    l$id_sim <- tt$id_sim
  }
  
  if("n_sims" %in% names(opt) ){
    l$n_sims <- opt$n_sims
  } else {
    l$n_sims <- tt$n_sims
  }
  
  l$print_cfg <- tt$print_cfg
  l$print_intrm <- tt$print_intrm

  if("seed" %in% names(opt)){
    l$seed <- opt$seed
  } else {
    l$seed <- tt$seed
  }
  
  l$n_start <- tt$n_start
  l$n_stop <- tt$n_stop
  
  l$int_mnth <- tt$int_mnth 
  l$int_n <- tt$int_n
  
  if("accrual" %in% names(opt)){
    l$accrual <- opt$accrual
  } else {
    l$accrual <- tt$accrual
  }
  
  if("delay" %in% names(opt)){
    l$sero_info_delay <- opt$delay
  } else {
    l$sero_info_delay <- tt$sero_info_delay
  }
  
  l$n_max_sero <- tt$n_max_sero
  l$n_start_clin <- tt$n_start_clin
  l$age_months_lwr <- tt$age_months_lwr
  l$age_months_upr <- tt$age_months_upr
  l$max_age_fu <- tt$max_age_fu
  
  if("basesero" %in% names(opt)){
    l$baseline_prob_sero <- opt$basesero
  } else {
    l$baseline_prob_sero <- tt$baseline_prob_sero
  }

  if("trtprobsero" %in% names(opt)){
    l$trt_prob_sero <- opt$trtprobsero
  } else {
    l$trt_prob_sero <- tt$trt_prob_sero
  }
  l$delta_sero_t3 <- compute_sero_delta(l$baseline_prob_sero, 
    l$trt_prob_sero)
  
  l$prior_beta_a <- tt$prior_beta_a
  l$prior_beta_b <- tt$prior_beta_b

  # time to event control variables
  # exponential
  # rates obtained from formula for med surv time (which are in months)
  # control log(2)/25 
  # treatment log(2)/30 - log(2)/25 
  # log(2)/25  = 0.027773
  # log(2)/30  = 0.023105
  
  # note - interp of survreg is fuckt 
  # see https://www.ms.uky.edu/~mai/Rsurv.pdf (top of page 4)
  # coefs aka. mu are related to rate via
  # mu = log(1/rate) 
  # i.e. rate = 1/exp(mu)
  
  # ignore the following - it relates to lognormal
  # for lognormal median is actually exp(mu) where mu represents
  # the mean parameter to a normal distribution, X for which
  # the lognormal dist is Y ~ exp(X) because the surv func
  # S(t) = 1 - CDF((ln(t) - μ) / σ) when equal to 0.5 gives
  # t_med = exp(μ).
  # So:
  # exp(3.4) ~= 30 months median survival for the control group
  # i.e. the time when surv prob equals 0.5 is about 30 months
  # exp(3.65) ~= 38 months


  if("basemediantte" %in% names(opt)){
    l$ctl_med_tte <- opt$basemediantte
  } else {
    l$ctl_med_tte <- tt$ctl_med_tte
  }
  
  if("trtmedtte" %in% names(opt)){
    l$trt_med_tte <- opt$trtmedtte
  } else {
    l$trt_med_tte <- tt$trt_med_tte
  }  

  l$b0_tte <- log(2)/l$ctl_med_tte 
  l$b1_tte <- (log(2)/l$trt_med_tte) - (log(2)/l$ctl_med_tte)

  l$prior_gamma_a <- tt$prior_gamma_a
  l$prior_gamma_b <- tt$prior_gamma_b

  l$thresh_pp_fut <- tt$thresh_pp_fut
  l$thresh_pp_es <- tt$thresh_pp_es
  l$thresh_p_sup <- tt$thresh_p_sup
  l$thresh_p_fut <- tt$thresh_p_fut
  
  l$post_draw <- 1000

  
  # colourblind palette
  l$cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  l$dnames <- dnames <- c("id", "trt", "accrt", "age", 
                          "serot2", "serot3", "probt3", 
                          "evtt", "cen", "obst", 
                          "reason", "impute", "reftime")
 
  if(print == T){
    cat("Configuration:\n")
    print(l)
    
  } 

  return(l)
  
  
}




compute_sero_delta <- function(p_ctl, p_trt){
  
  delta <- (p_trt - p_ctl)/ (1 - p_ctl)
  delta
  
}
