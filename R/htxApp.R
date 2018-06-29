#' explore SRA metadata
#' @import shiny
#' @export
#
# this function deals only with
# extraction of compendium elements.  the overall scope is
# determined by htxcomp::experTable which is the list of
# all experiments with taxon 9606, strategy RNA-seq,
# source transcriptomic
#
htxApp = function() {
 message("acquiring compendium restfulSE...")
 htxSE = loadHtxcomp()
 message("done.")
 studdata = htxcomp::studTable
 studdata = studdata[which(
     studdata$experiment_accession %in% colnames(htxSE)),]
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText(h2("SRA human RNA-seq surveyor.")),
    helpText("Add study accession numbers to 
'keep' box; SummarizedExperiment will
be returned when 'return SE' is pressed"),
    actionButton("btnSend", strong("return SE")),
    textInput("concept", "concept", "cancer"), width=2,
    selectInput("studyAcc", "keep", unique(studdata$study_accession),
      multiple=TRUE)
   ),
   mainPanel(
    dataTableOutput("conceptTable")
   )
  )
 )
 server = function(input, output) {
  output$conceptTable = renderDataTable({
   curTable = studdata
   inds = grep(input$concept, curTable$study_title)
   validate( need(length(inds)>0, "concept not found in titles") )
   tmp = curTable[ inds, ]
   tmp = tmp[-which(duplicated(tmp$study_accession)),]
   rownames(tmp) = tmp$study_accession
   nperstud = table(curTable$study_accession)
   npvec = nperstud[rownames(tmp)]
   tmp = cbind(Nexp=as.numeric(npvec), tmp[,-1])
   tmp
   })
  observe({ if (input$btnSend > 0) isolate({
          curTable = studdata
          curTable = curTable[which(curTable$study_accession %in% input$studyAcc),]
          stopApp(returnValue=htxSE[, curTable$experiment_accession])
                  }) })
  }
 runApp(list(ui=ui, server=server))
}
 
