% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbr.R
\name{get_kindergartens}
\alias{get_kindergartens}
\title{Get kindergartens}
\usage{
get_kindergartens(x, simplify = TRUE, raw_response = FALSE)
}
\arguments{
\item{x}{A Norwegian county or municipality code. Use 'all' to retrieve all
kindergartens}

\item{simplify}{If \code{TRUE} (default), a single data.frame is returned. Ignored
if \code{raw_response} is set to \code{TRUE}.}

\item{raw_response}{If \code{TRUE} a list of class \code{noAPI} is
returned, including the raw \code{httr2_response}.}
}
\value{
data.frame or list
}
\description{
Fetch information from the National Kindergarten Registry (NBR).
}
\details{
The function returns a data.frame by default. If you prefer the output
as a list you can set \code{simplify} to \code{FALSE}. This can be useful to keep
programmatically track of failed queries. If you set \code{raw_response}
to \code{TRUE}, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

See the
\href{https://www.udir.no/om-udir/data/nxr/}{API documentation}
for further details (in Norwegian only).
}
\examples{
# Get schools by county
get_kindergartens(11)

# Get schools by municipality
get_kindergartens(1101)

\dontrun{
# Get all schools
df <- get_kindergartens('all')
}
}
\seealso{
\code{\link[=get_counties]{get_counties()}} for retrieving county codes, and \code{\link[=get_municipalities]{get_municipalities()}}
for retrieving municipality codes.
}
