% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traj_fun.R
\name{traj_fun}
\alias{traj_fun}
\title{Trajectory Info}
\usage{
traj_fun(df, timename, xname, yname, idname)
}
\arguments{
\item{df}{dataframe of relocation data}

\item{timename}{column name of posix time}

\item{xname}{column name of x coordinate}

\item{yname}{column name of y coordinate}

\item{idname}{column name for individual ID}
}
\value{
Returns data with movement metrics appended
}
\description{
Add adehabitatLT trajectory vars to data
}
\examples{
\donttest{traj_fun(df, 'TelemDate','Easting','Northing','AID')}
}
\keyword{traj}
\keyword{trajectory,}
