% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loan_interest_rate.R
\name{loan_interest_rate}
\alias{loan_interest_rate}
\title{Calculate the Interest Rate on a Loan}
\usage{
loan_interest_rate(loan_amount, monthly_payment, loan_term)
}
\arguments{
\item{loan_amount}{The amount of the loan.}

\item{monthly_payment}{The monthly payment for the loan.}

\item{loan_term}{The term of the loan in years.}
}
\value{
The interest rate on the loan.
}
\description{
This function calculates the interest rate on a loan given the loan amount, monthly payment, and loan term.
}
\examples{
loan_interest_rate(loan_amount = 11000, monthly_payment = 324, loan_term = 10)

}
