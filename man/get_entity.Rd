% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brreg.R
\name{get_entity}
\alias{get_entity}
\title{Get entity}
\usage{
get_entity(entity, type = NULL, simplify = TRUE, raw_response = FALSE)
}
\arguments{
\item{entity}{A Norwegian organisation number or company name.}

\item{type}{Optional. Type entity to query. Either main, sub or NULL
(default). See details.}

\item{simplify}{If \code{TRUE} (default), a single data.frame is returned. Ignored
if \code{raw_response} is set to \code{TRUE}.}

\item{raw_response}{If \code{TRUE} a list of class \code{noAPI} is
returned, including the raw \code{httr2_response}.}
}
\value{
data.frame or list
}
\description{
Fetch information about an entity from the
\emph{Central Coordinating Register for Legal Entities} (Enhetsregisteret)
based on the entity's name or organisation number.
}
\details{
Users should be aware that queries for entity names and numbers are handled
somewhat differently. By default (\code{type = NULL}) queries for \emph{numbers}
will first send requests to the registry for \emph{main entities}, but will then
also query the registry for \emph{sub-entities} in case no information is found
in the first query. This ensures that any entity number still in use
will return a valid response. For \emph{names} however \code{get_entity()} will only
query the registry for \emph{main entities}, unless \code{type} is explicitly set to
\code{sub}. Further information on the use of sub-entities can be found
\href{https://www.brreg.no/bedrift/underenhet/}{here}.

The function returns a data.frame by default. If you prefer the output
as a list you can set \code{simplify} to \code{FALSE}. This can be useful to keep
programmatically track of failed queries. If you set \code{raw_response}
to \code{TRUE}, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

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

# Get information on sub-entities (number)
df <- get_entity(999178197)

#' # Get information on sub-entities (name)
df <- get_entity('Kuben Yrkesarena', type = 'sub')

}
