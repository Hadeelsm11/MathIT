#' Calculate the Interest Rate on a Loan
#'
#' This function calculates the interest rate on a loan given the loan amount, monthly payment, and loan term.
#'
#' @param loan_amount The amount of the loan.
#' @param monthly_payment The monthly payment for the loan.
#' @param loan_term The term of the loan in years.
#'
#' @return The interest rate on the loan.
#'
#' @examples
#' loan_interest_rate(loan_amount = 11000, monthly_payment = 324, loan_term = 10)
#'
loan_interest_rate <- function(loan_amount, monthly_payment, loan_term) {
  # Calculate total number of payments
  num_payments <- loan_term * 12

  # Define function to calculate the residual between the monthly payment
  # and the payment calculated with a given interest rate
  residual_function <- function(interest_rate) {
    payment <- loan_amount * (interest_rate * (1 + interest_rate) ^ num_payments) / ((1 + interest_rate) ^ num_payments - 1)
    residual <- payment - monthly_payment
    return(residual)
  }

  # Set upper and lower bounds for interest rate
  lower_bound <- 0
  upper_bound <- 1

  # Use binary search to find the interest rate that produces
  # a residual close to zero
  while (upper_bound - lower_bound > 1e-8) {
    mid_point <- (lower_bound + upper_bound) / 2
    residual <- residual_function(mid_point)
    if (residual > 0) {
      lower_bound <- mid_point
    } else {
      upper_bound <- mid_point
    }
  }

  # Return the interest rate at the midpoint of the final interval
  return((lower_bound + upper_bound) / 2)
}
