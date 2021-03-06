% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.censoring.km.estimate.R
\name{censoring.km.estimate}
\alias{censoring.km.estimate}
\title{Get Kaplan-Meier estimate of censoring distribution at time t.}
\usage{
censoring.km.estimate(t, survival.object, censoring.status = 0)
}
\arguments{
\item{t}{time}

\item{survival.object}{survival object containing times and censoring statuses.}

\item{censoring.status}{status corresponding to censored observations, defaults to zero.}
}
\value{
km.estimate
}
\description{
Get Kaplan-Meier estimate of censoring distribution at time t.
}
