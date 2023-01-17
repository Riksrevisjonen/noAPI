% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brreg.R
\name{get_entity}
\alias{get_entity}
\title{Get entity}
\usage{
get_entity(entity, raw_response = FALSE)
}
\arguments{
\item{entity}{A Norwegian organisation number or company name.}

\item{raw_response}{If TRUE a list of class \code{noAPI} is
returned, including the raw \code{httr2_response}.}
}
\value{
data.frame or list
}
\description{
Fetch information about an entity from the \emph{Central Coordinating
Register for Legal Entities} (Enhetsregisteret) based on the entity's
name or organisation number.
}
\details{
See the
\href{https://data.brreg.no/enhetsregisteret/api/docs/index.html}{API documentation}
for further details (in Norwegian only).
}
\examples{
# Get entity by number
df <- get_entity(974760843)

# Get entity by name
df <- get_entity('Riksrevisjonen')

# Get multiple entities
df <- get_entity(c(974760843, 971524960))

}