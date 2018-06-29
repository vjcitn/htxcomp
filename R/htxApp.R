#' explore SRA metadata
#' @import shiny
#' @export
htxApp = function() {
 if (!requireNamespace("glioBulk")) stop(
    "install glioBulk to use this function")
 expdata = htxcomp::experTable
 studdata = htxcomp::studTable
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText(h2("SRA human RNA-seq surveyor.")),
    helpText("Add study accession numbers to 
'keep' box; SummarizedExperiment will
be returned when 'return SE' is pressed."),
    actionButton("btnSend", strong("return SE")),
    checkboxInput("inComp", "Limit scope to htx compendium", TRUE),
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
  getCurTable = reactive({
   ans = studdata
   if (input$inComp) ans = ans[which(
     ans$experiment_accession %in% glioBulk::uniqueAcc_120518),]
   ans
   })
  output$conceptTable = renderDataTable({
   curTable = getCurTable()
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
#          validate(need(input$inComp, "scope must be limited to htx compendium, please check the 'Limit scope' box and verify your selections"))
          curTable = getCurTable()
          if (!input$inComp) warning("limiting scope to htxcomp despite setting of the 'Limit scope' button")
          curTable = curTable[which(curTable$experiment_accession %in% glioBulk::uniqueAcc_120518),]
          curTable = curTable[which(curTable$study_accession %in% input$studyAcc),]
          showNotification("acquiring restfulSE", id="acqnote")
          se = loadHtxcomp()
          removeNotification(id="acqnote")
          stopApp(returnValue=se[, curTable$experiment_accession])
                  }) })
  }
 runApp(list(ui=ui, server=server))
}
 
