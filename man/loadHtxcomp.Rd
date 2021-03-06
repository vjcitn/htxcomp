% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadhtx.R
\name{loadHtxcomp}
\alias{loadHtxcomp}
\title{load a SummarizedExperiment shell for the htx compendium}
\usage{

  loadHtxcomp(remotePath = "https://s3.amazonaws.com/bcfound-bigrna/htxcompSE.rds",
  cache = BiocFileCache(), genesOnly = TRUE)
}
\arguments{
\item{remotePath}{path to an RDS representation of the DelayedArray-based SummarizedExperiment}

\item{cache}{a BiocFileCache instance, defaulting to value of BiocFileCache()}

\item{genesOnly}{logical(1) if TRUE return reference to 
SummarizedExperiment with gene-level quantifications; in this 
case the remotePath value is
set to `https://s3.amazonaws.com/bcfound-bigrna/rangedHtxGeneSE.rds`.}
}
\description{
load a SummarizedExperiment shell for the htx compendium
}
\examples{
loadHtxcomp
}
