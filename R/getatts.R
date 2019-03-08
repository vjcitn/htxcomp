
attproc = function(x) {
 tt = x[[1]][,1]
 vals = lapply(x, "[[", "value")
# nv = sapply(vals, length)
# if (!all(nv==nv[1])) {
#   message("discrepant tag sets among samples")
#   message("returning values for common tags")
#   tags = lapply(x, "[[", "tag")
#   lta = sapply(tags,length)
#   tlta = table(lta)
#   mod = as.numeric(names(tlta)[which.max(tlta)])
#   bad = which(lta < mod)
#   vals = vals[-bad]
#   tags = tags[-bad]
#   t1 = tags[[1]]
#   for (i in 1:length(vals)) {
#     names(vals[[i]]) = tags[[i]]
#     t1 = intersect(t1, tags[[i]])
#     }
#   vals = lapply(vals, function(x) x[t1])
#   }
 ans = do.call(rbind, vals)
 colnames(ans) = tt
 ans
}

getStudy = function(studyAcc) 
SRAdbV2::Omicidx$new()$search(q=
  sprintf("study.accession: %s", studyAcc))$scroll()$collate()

#' retrieve a normalized data.frame of sample attributes for SRAdbV2 study tibbles
#' @param studyAcc character(1) accession
#' @param returnBad logical(1) returns tibble of experiment metadata for which number 
#' of fields in sample.attributes is less than the number in the majority
#' @note Sometimes a field is omitted for control experiments, and with manual programming
#' conformant metadata can be produced for these experiments.  Sometimes there is substantial
#' diversity among sample.attribute fields recorded within a study.
#' @export
sampleAtts = function(studyAcc, returnBad=FALSE) {
 st = getStudy(studyAcc)
 nrs = sapply((st %>% select(sample.attributes))[[1]], nrow)
 if (!all(nrs==nrs[1])) {
  warning("varying numbers of sample.attributes recorded through study")
  nrt = table(nrs)
  ind = which.max(nrt) # modal attr count
  num2use = as.numeric(names(nrt)[ind])
  bad = st[which(nrs < num2use),]
  nb = nrow(bad)
  if (!returnBad) message(nb, " experiments were excluded, use 'returnBad=TRUE' to see why their sample attributes are different from the majority")
  st = st[-which(nrs < num2use),]
  }
 ea = st$experiment.accession
 if (returnBad) return(bad)
 data.frame(cbind(study.accession=studyAcc, 
    experiment.accession=ea, attproc(st$sample.attributes)),
    stringsAsFactors=FALSE)
}
