\name{LuxuryLoanPortfolio}
\alias{LuxuryLoanPortfolio}
\docType{data}
\title{
LuxuryLoanPortfolio
}
\description{
This dataset contains information on number of loan payments,
interest rates, total interest, and loan amounts for random individuals.
}
\usage{data("LuxuryLoanPortfolio")}
\format{
  A data frame with 1678 observations on the following 4 variables.
  \describe{
    \item{\code{loan_number_payments}}{a numeric vector of number of payments.}
    \item{\samp{loan_interest _rate}}{a numeric vector of interest rates.}
    \item{\code{total_interest}}{a numeric vector of the total interest for the loan.}
    \item{\code{loan_amount}}{a numeric vector of the total loan amount.}
  }
}

\source{
https://www.kaggle.com/datasets/anandaramg/luxury-loan-portfolio
}

\examples{
data(LuxuryLoanPortfolio)
## maybe str(LuxuryLoanPortfolio) ; plot(LuxuryLoanPortfolio) ...
}

