checkCache_htxcomp = function (cache = BiocFileCache()) 
{
    allr = bfcinfo(cache)$rname
    "https://s3.amazonaws.com/bcfound-bigrna/htxcompSE.rds" %in% 
        allr
}


#' load a SummarizedExperiment shell for the htx compendium
#' @importFrom BiocFileCache bfcinfo BiocFileCache bfcrpath
#' @param remotePath path to an RDS representation of the DelayedArray-based SummarizedExperiment
#' @param cache a BiocFileCache instance, defaulting to value of BiocFileCache()
#' @examples
#' loadHtxcomp
#' @export
loadHtxcomp = function (remotePath = "https://s3.amazonaws.com/bcfound-bigrna/htxcompSE.rds",
    cache = BiocFileCache()) 
{
    if (!checkCache_htxcomp(cache)) 
        message("adding RDS to local cache, future invocations will use local image")
    path = bfcrpath(cache, remotePath)
    readRDS(path)
}

