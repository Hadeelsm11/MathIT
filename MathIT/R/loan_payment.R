#' Calculate Monthly Loan Payments
#'
#' @param loan_amount amount of the loan
#' @param interest_rate rate for interest on loan
#' @param loan_term amount of years for loan
#'
#' @return total monthly payments on loan
#' @export
#'
#' @examples loan_payment(loan_amount = 20000, interest_rate = 5, loan_term = 24)

loan_payment <- function(loan_amount, interest_rate, loan_term) {
  monthly_rate <- interest_rate / 12
  num_payments <- loan_term * 12
  payment <- loan_amount * (monthly_rate * (1 + monthly_rate) ^ num_payments) / ((1 + monthly_rate) ^ num_payments - 1)
  return(payment)
}
