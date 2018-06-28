#' explore SRA metadata
#' @import shiny
#' @import DT
#' @export
htxApp = function() {
 if (!requireNamespace("glioBulk")) stop(
    "install glioBulk to use this function")
 expdata = htxcomp::experTable
 studdata = htxcomp::studTable
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText("htxcomp surveyor.  Add study accession numbers to 
'keep' box; vector of related experiment accession numbers will
be returned when 'stop app' is pressed."),
    checkboxInput("inComp", "limit to htxcomp", FALSE),
    textInput("concept", "concept", "cancer"), width=2,
    selectInput("studyAcc", "keep", unique(studdata$study_accession),
      multiple=TRUE),
    actionButton("btnSend", "stop app")
   ),
   mainPanel(
    dataTableOutput("conceptTable")
   )
  )
 )
 server = function(input, output) {
  output$conceptTable = renderDataTable({
   curTable = studdata
   if (input$inComp) curTable = curTable[which(
     curTable$experiment_accession %in% glioBulk::uniqueAcc_120518),]
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
                  stopApp(returnValue=input$studyAcc)
                  }) })
  }
 runApp(list(ui=ui, server=server))
}
 
