% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rf_sampling.R
\name{rf_sampling}
\alias{rf_sampling}
\title{Spatial Sampling}
\usage{
rf_sampling(x, dat, nsamps, nt, maxnt, proj, ras)
}
\arguments{
\item{x}{list of data.frames containing 'Month','Year','Spp'}

\item{dat}{data.frame of relocation data}

\item{nsamps}{multiplier, number of used locations times nsamps = number available locations}

\item{nt}{mean step length times nt = minimum distance to sample from}

\item{maxnt}{mean step length times nt times maxnt = maximum distance to sample from}

\item{proj}{proj4string of point data}

\item{ras}{raster grid to sample from}
}
\value{
Returns a list containing a report on the sampling and the used/available samples
}
\description{
Create random sample in proximity to used samples
}
\examples{
\donttest{system.time({ sampout<-lapply(d[1],rf_sampling,dat=df,nsamps=2,nt=5,maxnt=5,proj=dfproj,ras=nras) })}
}
\keyword{rf}
\keyword{sampling,}
