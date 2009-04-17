\name{monitor.colony}
\alias{monitor.colony}

\title{A function to monitor the log-likelihood of the sibship analysis being undertaken by Colony.}
\description{
This function examines the intermediate outputs (*.MidResult) from the Colony program. It plots the log-likelihood as a function of the iteration number thereby allowing the user to monitor progress.
The function repeatedly calls a plot at an interval that can be set by the user (the default is 3 seconds). 
It keeps running until the Colony analysis has finished, or until the user interupts it (by pressing the Esc key).

}
\usage{}

\keyword{ data }
