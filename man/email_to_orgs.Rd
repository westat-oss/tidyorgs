% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/email_to_orgs.R
\name{email_to_orgs}
\alias{email_to_orgs}
\title{Match emails to social organizations}
\usage{
email_to_orgs(
  data,
  id,
  input,
  output,
  sector = c("academic", "business", "government", "nonprofit")
)
}
\arguments{
\item{data}{A data frame or data frame extension (e.g. a tibble).}

\item{id}{A numeric or character vector unique to each entry.}

\item{input}{Character vector of emails or email domains that will be matched to 
organizations from one (or all) of five economic sectors (see sector parameter).}

\item{output}{Output column to be created as string or symbol.}

\item{sector}{Sector to match by organizations. Currently, the only option is "academic" 
with "business", "government", "household", and "nonprofit" in development.}
}
\description{
This function matches users to organizations based on their email domains and/or 
sub-domains. For example, users with domains ending in "virginia.edu" will be 
matched to the "university of virginia". This function has been integrated as an 
optional parameter alongside the email_to_sectors() function in the detect_orgs() function.
}
\examples{

library(tidyverse)
library(tidyorgs)
data(github_users)

classified_by_email <- github_users \%>\%
  email_to_orgs(login, email, organization, academic)

}
