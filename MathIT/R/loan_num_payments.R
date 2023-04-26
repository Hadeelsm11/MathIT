#' Calculate the Total Number of Payments for a Loan
#'
#' @param loan_amount total amount of loan
#' @param interest_rate iterest rate of loan
#' @param monthly_payment monthly loan payments
#'
#' @return returns amount of monthly installments for the loan balance
#' @export
#'
#' @examples loan_num_payments(loan_amount= 25000, interest_rate = 2, monthly_payment = 234)

loan_num_payments <- function(loan_amount, interest_rate, monthly_payment) {
  # Calculate monthly interest rate
  monthly_rate <- interest_rate / 12

  # Use formula to calculate number of payments:
  # n = -log(1 - (r * P) / A) / log(1 + r)
  num_payments <- -log(1 - (monthly_rate * monthly_payment) / loan_amount) / log(1 + monthly_rate)

  return(num_payments)
}
