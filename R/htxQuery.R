#' retrieve 'restfulSE' SummarizedExperiment instance for selected studies in htx compendium
#' @param study_accessions character vector of study accessions
#' @return SummarizedExperiment instance
#' @examples
#' htxQuery("ERP011411")
#' @export
htxQuery = function(study_accessions, ...) {
 message("acquiring base restfulSE...")
 htxSE = loadHtxcomp()
 studdata = htxcomp::studTable
 studdata = studdata[which(
     studdata$experiment_accession %in% colnames(htxSE)),]
 todrop = which(duplicated(studdata$experiment_accession))
 if (length(todrop)>0) studdata = studdata[-todrop,]
 exps = studdata$experiment_access[which(studdata$study_accession %in%
        study_accessions)]
 htxSE[, exps]
}
