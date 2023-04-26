#' Calculate the Total Interest on a Loan
#'
#' @param loan_amount Total loan amount
#' @param interest_rate Total interest rate for loan amount
#' @param loan_term Length of loan payments in years
#'
#' @return returns total interest on a loan
#' @export
#'
#' @examples total_interest(loan_amount = 500 , interest_rate = 3, loan_term = 3)

total_interest<- function(loan_amount, interest_rate, loan_term){
  interest <- loan_amount * interest_rate * loan_term
  statement<- paste0("The total amount of interest you pay is $", interest, ".")
  return(statement)
}

