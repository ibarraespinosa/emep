% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ef.R
\name{ef}
\alias{ef}
\title{Emission factors from EMEP EEA Tier 2}
\usage{
ef(nfr, sector, table, type, tech, fuel, aba, region, pol, df)
}
\arguments{
\item{nfr}{Character; NFR.}

\item{sector}{Character; sector.}

\item{table}{Character; table.}

\item{type}{Character; type.}

\item{tech}{Character; technology.}

\item{fuel}{Character; fuel.}

\item{aba}{Character; abatement.}

\item{region}{Character; region.}

\item{pol}{Character; pollutant.}

\item{df}{Logical; To return as data.frame or not. When T it hows also
the units, confidence interval lower, upper and the reference.}
}
\value{
Emission factors or data.frame.
}
\description{
\code{\link{ef}} Returns emission factors from EMEP EEA Tier2.
}
\examples{
\dontrun{
# do not run
ef
}
}
