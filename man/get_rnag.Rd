% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rnag.r
\name{get_rnag}
\alias{get_rnag}
\title{Retrieve Reasons for Not Achieving Good Status}
\usage{
get_rnag(ea_name = NULL, column = NULL, type = NULL)
}
\arguments{
\item{ea_name}{A string representing the description (\code{name} for 
\code{OC}, \code{MC} or \code{RBD} level downloads or \code{WBID} for 
individual waterbodies) of the features to be extracted. For example 
to extract data for the whole of the Humber RBD, this would be "Humber"; 
also see examples. Must be an exact match to the values used in the EA 
database. Use the \code{\link{search_names}} function to search for 
specific values.}

\item{column}{The column to be searched. Possible options are
\code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC}
(Management Catchment) and \code{RBD} (River Basin District)}

\item{type}{Type of waterbody to be extracted. For Operational/Management
catchment level or RBD level queries, the data can also be subset by
waterbody type. Possible values are \code{River}, \code{Lake},
\code{GroundWaterBody}, \code{TransitionalWater} or \code{CoastalWater}.}
}
\value{
An object of class \code{cde_df} containing the details of the 
Reasons for Not Achieving Good Status for the specified combination 
of criteria.
For details of the meaning of the the different columns returned, 
see \url{https://docs.ropensci.org/cde/articles/cde-output-reference.html}.
}
\description{
Retrieves details of Reasons for Not Achieving Good (RNAG)
status and Reasons For Failure (RFF) from EA Catchment Data Explorer site.
Data can be retrieved by specifying waterbody id
(\code{WBID}), Management Catchment (\code{MC}), Operational
Catchment (\code{OC}) or River Basin District (\code{RBD}).
Start year (\code{startyr}) and end year (\code{endyr}) allow
specific timeranges to be downloaded.
For Management Catchment (\code{MC}), Operational
Catchment (\code{OC}) or River Basin District (\code{RBD}) level
downloads, waterbody \code{type} can also be specified to allow
extraction of specific waterbody types (River, Lake etc).
Data are presented at the level of individual elements that are the
reasons for not achieving good status.
}
\examples{
# get all RNAG issues identified for waterbody GB112071065700
get_rnag("GB112071065700", "WBID")

# get the RNAG issues for Lakes in the Humber RBD, between
# 2013 and 2014
get_rnag(ea_name="Humber", column="RBD", type="Lake")

# get the RNAG issues for Rivers in the Avon Warwickshire
# Management Catchment
get_rnag(ea_name="Avon Warwickshire", column="MC", type="River")

}
