% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kernels.R
\name{identity_kernel}
\alias{identity_kernel}
\title{LIME kernel that treats all observations as equally similar to 
observation of interest.}
\usage{
identity_kernel(explained_instance, simulated_instance)
}
\arguments{
\item{explained_instance}{explained instance}

\item{simulated_instance}{new observation}
}
\value{
numeric
}
\description{
LIME kernel that treats all observations as equally similar to 
observation of interest.
}
