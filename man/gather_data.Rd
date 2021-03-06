% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gather_data.R
\name{gather_data}
\alias{gather_data}
\title{clean all data}
\usage{
gather_data(usernames, passwords, ST, STUser, STPass, tempdir, lookup,
  histdata, vecpath)
}
\arguments{
\item{usernames}{vector of ATS usernames}

\item{passwords}{vector of ATS passwords}

\item{ST}{TRUE/FALSE whether to get sirtrack data}

\item{STUser}{email address for sirtrack}

\item{STPass}{password for sirtrack}

\item{tempdir}{Temp directory}

\item{lookup}{capture history table}

\item{histdata}{historic shapefile path}

\item{vecpath}{file path to location of vectronic data}
}
\value{
Resulting object is a list of Two elements. First element is a SpatialPointsDataFrame of
all the GPS data on the account, the second is a dataframe of all transmissions received by
the satellite

\strong{Spatial Data Column Description:}
\item{CollarSerialNumber}{ATS Designated Collar Serial Number}
\item{TelemDate}{POSIX Field of collare fixes in the USER'S timezone. NOTE: may need to alter timezone}
\item{HDOP}{Horizontal Dilution of Precision}
\item{NumSats}{Number of satellites used for GPS Fix}
\item{2D/3D}{Whether fix is a 2d or 3d fix. Values are either 2 or 3}
}
\description{
Download new and historic data and bind all data together
}
\examples{
\donttest{gather_data(usernames=c('yourusername'),passwords=c('yourpassword'),ST=TRUE,STUser='stusername',STPass='stpassword',tempdir='/home/mhayes1/Desktop/Testing/',lookup='path/to/db.csv',histdata='path/to/hist.shp',vecpath='/path/to/vec/data/')}

}
\keyword{Iridium,}
\keyword{iridium}
