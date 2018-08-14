#' 95% confindence intervals for odds ratio by wald approximation
#'
#' @param n11 cases where exp = 1 and out = 1
#' @param n00 number of cases where exp = 0 and out = 0
#' @param n01 number of cases where exp = 0 and out = 1
#' @param n10 number of cases where exp = 1 and out = 0
#' @param alpha controls width of confidence interval (1-alpha bands returned)
#' @author matteo-V
#' @importFrom stats qnorm
#' @export
oddsratioWald.proc <- function(n00, n01, n10, n11, alpha = 0.05, outcome){
  #
  #  Compute the odds ratio between two binary variables, x and y,
  #  as defined by the four numbers nij:
  #
  #    n00 = number of cases where x = 0 and y = 0
  #    n01 = number of cases where x = 0 and y = 1
  #    n10 = number of cases where x = 1 and y = 0
  #    n11 = number of cases where x = 1 and y = 1
  #
  OR <- (n00 * n11)/( n01 * n10 )
  #
  #  Compute the Wald confidence intervals:
  #
  siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog
  #
  ORlo <- exp(loglo)
  ORhi <- exp(loghi)
  #set null hypothesis to 0
  nullLogOR <- 0
  test_stat <- (logOR - 0 )/siglog
  pval <- 2 * ( 1 - pnorm(abs(test_stat)) )
  #
  oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, pvalue = pval)
  oframe
}

#' 95% confindence intervals for risk ratio by wald approximation
#'
#' @param n11 number of cases where exp = 1 and out = 1
#' @param n00 number of cases where exp = 0 and out = 0
#' @param n01 number of cases where exp = 0 and out = 1
#' @param n10 number of cases where exp = 1 and out = 0
#' @param alpha controls width of confidence interval (1-alpha bands returned)
#' @author matteo-V
#' @importFrom stats qnorm
#' @export
riskratioWald.proc <- function(n00, n01, n10, n11, alpha = 0.05, outcome){
  #
  #  Compute the odds ratio between two binary variables, x and y,
  #  as defined by the four numbers nij:
  #
  #    n00 = number of cases where x = 0 and y = 0
  #    n01 = number of cases where x = 0 and y = 1
  #    n10 = number of cases where x = 1 and y = 0
  #    n11 = number of cases where x = 1 and y = 1
  #
  RR <- ( n11/n10+n11 )/( n01/ n00+n01 )
  #
  #  Compute the Wald confidence intervals:
  #
  siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(RR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog
  #
  ORlo <- exp(loglo)
  ORhi <- exp(loghi)
  #

  #
  oframe <- data.frame(LowerCI = ORlo, RR = RR, UpperCI = ORhi, alpha = alpha)
  oframe
}

#' compute risk ratio and 95% CI for categorical data
#'
#' @param exposure exposure variable of interest
#' @param outcome outcome variable of interest
#' @author matteo-V
#' @noRd
calculate_risk_ratio_ci <- function(outcome, exposure, var_name){
  ill_table <- table(exposure, outcome)
  removed_vars <- c()
  if(nrow(ill_table) == 2 & ncol(ill_table) == 2){
    res <- riskratioWald.proc( n11 = ill_table[2, 2],
                               n00 = ill_table[1, 1],
                               n10 = ill_table[2, 1],
                               n01 = ill_table[1, 2])
    return(cbind(res, var_name = var_name))
  }
  else{
    removed_vars <- append(removed_vars, var_name)
    cat('\nWarning,', var_name, "does not have 2 levels. Skipping OR calculation")
  }

}

#' compute odds ratio and 95% CI for categorical data
#'
#' @param exposure exposure variable of interest
#' @param outcome outcome variable of interest
#' @author matteo-V
#' @noRd
calculate_odds_ratio_ci <- function(outcome, exposure, var_name){

  ill_table <- table(exposure, outcome)
  removed_vars <- c()
  if(nrow(ill_table) == 2 & ncol(ill_table) == 2){
    res <- oddsratioWald.proc( n11 = ill_table[2, 2],
                               n00 = ill_table[1, 1],
                               n10 = ill_table[2, 1],
                               n01 = ill_table[1, 2])
    return(cbind(res, var_name = var_name))
  }
  else{
    removed_vars <- append(removed_vars, var_name)
    cat('\nWarning,', var_name, "does not have 2 levels. Skipping OR calculation")
  }

}

#' run odds ratios and CI for all variables related to illness
#'
#' @param dat from illness_analysis_frame
#' @param outcome_var outcome variable as character (ili, sari, hemorraghic, encephalitis)
#' @return data_frame of odds ratios and CIs
#' @author matteo-V
#' @importFrom dplyr %>% select data_frame
#' @importFrom base rbind
#' @export
calculate_illness_odds <- function(dat, outcome_var){

  analysis_dat <-
    dat %>%
    get_illness_analysis_dat(outcome_var = outcome_var)

  outcome <- analysis_dat[,1] #first col is outcome
  var_names <- (colnames(analysis_dat[,-1])) #without outcome

  #struct to hold results
  res <- data_frame( var_name = character(),
                     LowerCI = numeric(),
                     OR = numeric(),
                     upperCI = numeric(),
                     pval = numeric() )

  for(ii in var_names){
    res_ii <- calculate_odds_ratio_ci(outcome = outcome,
                                      exposure = analysis_dat[,ii],
                                      var_name = ii)

    res <- rbind(res, res_ii)

  }
  #bonferroni correction to multiple testing
  res %>%
    mutate(significant = if_else(condition = pvalue < (0.05/length(pvalue)),
                                 true = T,
                                 false = F))
}

#' run risk ratios and CI for all variables related to illness
#'
#' @param dat from illness_analysis_frame
#' @param outcome_var outcome variable as character (ili, sari, hemorraghic, encephalitis)
#' @return data_frame of odds ratios and CIs
#' @author matteo-V
#' @importFrom dplyr %>% select data_frame
#' @importFrom base rbind
#' @export
calculate_illness_risk <- function(dat, outcome_var){
  analysis_dat <-
    dat %>%
    get_illness_analysis_dat(outcome_var = outcome_var) %>%
    select(-matches("prov")) %>%
    select(-c(males_in_dwelling, children_in_dwelling))

  outcome <- analysis_dat[,1] #first col is outcome
  var_names <- (colnames(analysis_dat[,-1])) #without outcome

  #struct to hold results
  res <- data_frame( var_name = character(),
                     LowerCI = numeric(),
                     OR = numeric(),
                     upperCI = numeric(),
                     alpha = numeric() )

  for(ii in var_names){
    res_ii <- calculate_risk_ratio_ci(outcome = outcome,
                                      exposure = analysis_dat[,ii],
                                      var_name = ii)

    res <- rbind(res, res_ii)

  }
  return(res)
}
