#' G-formula method
#'
#' Function calculates a "causal" estimate of a binary
#' treatment/exposure (a) on an outcome (y) using simple substitution
#' (g-formula). It can calculate the risk difference or risk ratio.
#' @param estimate Estimate difference or ratio. Defaults to "diff"
#' @keywords g-formula
#' @export
#' @examples
#' g.formula()

# g-formula function ----
g.formula <- function(data, y, a, q.model, model.family, estimate="diff"){
  # create a = 1 and a = 0 dataframes ----
  a1_df <- a0_df <- data
  a1_df[, a] <- 1
  a0_df[, a] <- 0
  # run q-formula
    model <- glm(as.formula(paste(y, q.model, sep = "~")), data = data,
                 family = model.family)
  # calculate mean expected in a1 and a0
    pr_a1 <- mean(predict(model, newdata = a1_df, type = "response"))
    pr_a0 <- mean(predict(model, newdata = a0_df, type = "response"))

  # calculate desired estimates "diff" or "ratio"
    # calculate difference a1 to a0
    if(estimate == "diff"){
      msm_diff <- pr_a1 - pr_a0
      msm_val <- msm_diff
    }
    # calculate ratio of a1/a0
    if(estimate == "ratio"){
      msm_ratio <- pr_a1/pr_a0
      msm_val <- msm_ratio
    }
    # output
      return(msm_val)

} # end g.formula function



