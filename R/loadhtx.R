checkCache_htxcomp = function (cache = BiocFileCache(), genesOnly=TRUE) 
{
    allr = bfcinfo(cache)$rname
    "https://s3.amazonaws.com/bcfound-bigrna/htxcompSE.rds" %in% 
        allr
}


#' load a SummarizedExperiment shell for the htx compendium
#' @importFrom BiocFileCache bfcinfo BiocFileCache bfcrpath
#' @param remotePath path to an RDS representation of the DelayedArray-based SummarizedExperiment
#' @param cache a BiocFileCache instance, defaulting to value of BiocFileCache()
#' @param genesOnly logical(1) if TRUE return reference to 
#' SummarizedExperiment with gene-level quantifications; in this 
#' case the remotePath value is
#' set to `https://s3.amazonaws.com/bcfound-bigrna/rangedHtxGeneSE.rds`.
#' @examples
#' loadHtxcomp
#' @export
loadHtxcomp = function (remotePath = "https://s3.amazonaws.com/bcfound-bigrna/htxcompSE.rds",
    cache = BiocFileCache(), genesOnly=TRUE) 
{
    if (!checkCache_htxcomp(cache)) 
        message("adding RDS to local cache, future invocations will use local image")
    if (genesOnly) remotePath = "https://s3.amazonaws.com/bcfound-bigrna/rangedHtxGeneSE.rds"
    path = bfcrpath(cache, remotePath)
    readRDS(path)
}

