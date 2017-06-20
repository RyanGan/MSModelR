#' IPTW-estimation method
#'
#' Functions calculates a "causal" estimate of a binary
#' treatment/exposure (a) on an outcome (y) using inverse probability of treatment
#' weighting (IPTW). It can calculate the risk difference or risk ratio.
#' @param estimate Estimate difference or ratio. Defaults to "diff"
#' @keywords iptw
#' @export
#' @examples
#' iptw.estimation()
# inverse probability of treatment weighting (IPTW) ----

iptw.estimation <- function(data, y, a, w, estimate="diff"){
  # step 1: estimate propensity of treatment ----
    prop_mod <- glm(as.formula(paste(a, w, sep = "~")), data = data,
                    family = "binomial")
  # step 2: estimate weights ----
    # vector of propensity exposed for each observation
    prob_exposed <- predict(prop_mod, type = "response")
    # vector of propensity not exposed
    prob_unexposed <- 1 - prob_exposed

    # note: 4/20/17 can't quite figure out how to include a plot and value
    # histogram to check for ETA violation
    # if(eta_plot == "yes"){ # plot probability of exposed
    # eta_fig <- hist(prob_exposed)
    #   }

  # create weights
    wt <- as.numeric(data[,a]==1)/prob_exposed +
          as.numeric(data[,a]==0)/prob_unexposed

  # calculate desired estimates "diff", "ratio", "both" ----
    # calculate mean estimate
    a1_est <- mean(as.numeric(data[,a]==1)*as.numeric(wt)*as.numeric(unlist(data[,y])))
    a0_est <-  mean(as.numeric(data[,a]==0)*as.numeric(wt)*as.numeric(unlist(data[,y])))
    # calculate difference a1 to a0
    if(estimate == "diff"){
      iptw_diff <- a1_est - a0_est
      iptw_val <- iptw_diff
    }
    # calculate ratio of a1/a0
    if(estimate == "ratio"){
      iptw_ratio <- a1_est/a0_est
      iptw_val <- iptw_ratio
    }
    # output a list of figure and value
    #iptw_out <- list(eta_fig, iptw_val)
    return(iptw_val)
} # end function
