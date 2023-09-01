#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(circlize)
library(svglite)
library(JBrowseR)


ui <- fluidPage(

    titlePanel("Genome browser 5.3"),
    tags$head(tags$link(rel = "stylesheet",
                        type = "text/css", href = "style.css")),
    fluidRow(
      column(3,
             wellPanel(
               selectInput("selectChr",
                           label = "Select Chromosome to inspect",
                            choices = list("chr1",
                                           "chr2",
                                           "chr3",
                                           "chr4",
                                           "chr5",
                                           "chr6",
                                           "chr7",
                                           "chr8",
                                           "chr9",
                                           "chr10",
                                           "chr11",
                                           "chr12",
                                           "chr13",
                                           "chr14",
                                           "chr15",
                                           "chr16",
                                           "chr17",
                                           "chr18",
                                           "chr19",
                                           "chr20",
                                           "chr21",
                                           "chr22",
                                           "chrX",
                                           "chrY",
                                           "Other"),
                           selected = "Chr1"),
               textOutput("maxCoords")
             ),
             wellPanel(
               numericInput("start", 
                            label = "Start",
                            value = 0,
                            min = 0,
                            max = 249250621),
               actionButton("sAdd10000000",
                            label = "Add 10000000"),
               actionButton("sAdd10000",
                            label = "Add 10000"),
               actionButton("sAdd10",
                            label = "Add 10"),
               actionButton("sSub10000000",
                            label = "Sub 10000000"),
               actionButton("sSub10000",
                            label = "Sub 10000"),
               actionButton("sSub10",
                            label = "Sub 10")
             ),
             wellPanel(
               numericInput("end", 
                            label = "End",
                            value = 10000000,
                            min = 0,
                            max = 249250621),
               actionButton("eAdd10000000",
                            label = "Add 10000000"),
               actionButton("eAdd10000",
                            label = "Add 10000"),
               actionButton("eAdd10",
                            label = "Add 10"),
               actionButton("eSub10000000",
                            label = "Sub 10000000"),
               actionButton("eSub10000",
                            label = "Sub 10000"),
               actionButton("eSub10",
                            label = "Sub 10")
             ),
             wellPanel(
               numericInput("jsStartInput", "tempStart", 0),
               numericInput("jsEndInput", "tempEnd", 10000000),
               style = "display: none"
             )
      ),
      column(9,
             tabsetPanel(type = "tabs",
                         tabPanel("Plot",
                            div(
                              actionButton("openPlotSidebar", label = "Open Side"),
                              inputPanel(
                                checkboxInput("outerChart", "Show Whole Genome", value = TRUE),
                                conditionalPanel("input.outerChart == true",
                                  actionButton("labelDir", "Label format: )-"),
                                  checkboxGroupInput("outerOptions", NULL, list("Labels",
                                                                                "Axis",
                                                                                "Idiogram",
                                                                                "Lines",
                                                                                "Rainfall",
                                                                                "Density"),
                                                     selected = list("Labels", "Lines"))
                                ),
                                checkboxInput("innerChart", "Show Selected Region", value = TRUE),
                                conditionalPanel("input.innerChart == true",
                                                 sliderInput("zoomLev", "Zoom Level:",
                                                             min = 100, max = 300, value = 200, width = "100%"
                                                 )
                                )
                              ),
                              fileInput("fileIn", "Upload a file"),
                              class = "chartOptions"
                            ),
                            plotOutput("genomePlot", width = "100%", height = "100%"),
                            plotOutput("chrPlot", width = "100%")
                         ),
                         tabPanel("Browser",
                            JBrowseROutput("browserOutput"),
                            actionButton("update", label = "Update values")
                         ),
                         tabPanel("How to Use",
                                  HTML('<h2 class="c11" id="h.f2wdx8s23o41">
                                       <span class="c9">How to Use This Application</span>
                                       </h2><p class="c6"><span class="c10">To upload your own data…</span>
                                       </p><p class="c2"><span class="c10"></span></p><p class="c6">
                                       <span class="c10">Format:</span></p><p class="c2"><span class="c10">
                                       </span></p><a id="t.ff6b03388cf735e11d23d2f8e59500d0f6ef6a8b"></a>
                                       <a id="t.0"></a><table class="c5"><tbody><tr class="c4">
                                       <td class="c13" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">chr </span></p></td><td class="c8" colspan="1" rowspan="1">
                                       <p class="c6"><span class="c0">start </span></p></td>
                                       <td class="c7" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">end </span></p></td><td class="c1" colspan="1" rowspan="1">
                                       <p class="c6"><span class="c0">value1</span></p></td></tr><tr class="c4">
                                       <td class="c13" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">chr1 </span></p></td><td class="c8" colspan="1" rowspan="1">
                                       <p class="c6"><span class="c0">1113777 </span></p></td>
                                       <td class="c7" colspan="1" rowspan="1">
                                       <p class="c6"><span class="c0">3239482 </span></p></td>
                                       <td class="c1" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">0.156375906</span></p></td></tr><tr class="c4">
                                       <td class="c13" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">chr1 </span></p></td>
                                       <td class="c8" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">13172168 </span></p></td>
                                       <td class="c7" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">14687106 </span></p></td>
                                       <td class="c1" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">0.345011929</span></p></td></tr>
                                       <tr class="c4"><td class="c13" colspan="1" rowspan="1">
                                       <p class="c6"><span class="c0">chr1 </span></p></td>
                                       <td class="c8" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">16135123 </span></p></td>
                                       <td class="c7" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">19957414 </span></p></td>
                                       <td class="c1" colspan="1" rowspan="1"><p class="c6">
                                       <span class="c0">-0.795450620</span></p></td></tr>
                                       <tr class="c4"><td class="c13" colspan="1" rowspan="1">
                                       <p class="c3"><span class="c0">chr1 </span></p></td>
                                       <td class="c8" colspan="1" rowspan="1"><p class="c3">
                                       <span class="c0">21553432 </span></p></td>
                                       <td class="c7" colspan="1" rowspan="1"><p class="c3">
                                       <span class="c0">22387200 </span></p></td>
                                       <td class="c1" colspan="1" rowspan="1"><p class="c3">
                                       <span class="c0 c14">1.187606720</span></p></td></tr>
                                       </tbody></table><p class="c2"><span class="c10"></span></p>'),
                                  downloadButton("downloadTemp", "Download Template")
                            )
             )
      ),
      tags$script(src = "script.js"),
    )
)

server <- function(input, output, clientData, session) {
  
  
  
  values <- reactiveValues(maxX = 249250621, labelText = "clockwise", trackHeight = .125, bed = generateRandomBed(nr = 500))
  
  chrList = list("chr1",
                 "chr2",
                 "chr3",
                 "chr4",
                 "chr5",
                 "chr6",
                 "chr7",
                 "chr8",
                 "chr9",
                 "chr10",
                 "chr11",
                 "chr12",
                 "chr13",
                 "chr14",
                 "chr15",
                 "chr16",
                 "chr17",
                 "chr18",
                 "chr19",
                 "chr20",
                 "chr21",
                 "chr22",
                 "chrX",
                 "chrY")
  
  values$startCommas <- reactive({
    separatedNumbers <- strsplit(as.character(input$start), "")[[1]]
    revSepNumbers <- rev(separatedNumbers)
    howManyCommas <- floor(length(revSepNumbers) / 3)
    if(length(revSepNumbers) %% 3 == 0){
      howManyCommas = howManyCommas - 1
    }
    revAddedCommas <- revSepNumbers
    if(howManyCommas > 0){
      for (x in 1:howManyCommas) {
        revAddedCommas <- append(revAddedCommas, ",", after = x * 3 +(x - 1))
      }
    }
    addedCommas <- rev(revAddedCommas)
    finalWithCommas <- paste(addedCommas, collapse = "")
    return(finalWithCommas)
  })
  
  
  
  values$endCommas <- reactive({
    separatedNumbers <- strsplit(as.character(input$end), "")[[1]]
    revSepNumbers <- rev(separatedNumbers)
    howManyCommas <- floor(length(revSepNumbers) / 3)
    if(length(revSepNumbers) %% 3 == 0){
      howManyCommas = howManyCommas - 1
    }
    revAddedCommas <- revSepNumbers
    if(howManyCommas > 0){
      for (x in 1:howManyCommas) {
        revAddedCommas <- append(revAddedCommas, ",", after = x * 3 +(x - 1))
      }
    }
    addedCommas <- rev(revAddedCommas)
    finalWithCommas <- paste(addedCommas, collapse = "")
    return(finalWithCommas)
  })
  
  values$chrNumber <- reactive({
    return(strsplit(input$selectChr, "chr")[[1]][2])
  })
  
  values$isUpdatePressedStart <- FALSE
  values$isUpdatePressedEnd <- FALSE
  
  observe({
    updateNumericInput(session, "start", max = values$maxX)
    updateNumericInput(session, "end", max = values$maxX)
    
    if(as.numeric(input$jsStartInput) <= values$maxX && values$isUpdatePressedStart != FALSE)
    {
      updateNumericInput(session, "start", value = as.numeric(input$jsStartInput))
      values$isUpdatePressedStart <- FALSE
    }
    
    if(as.numeric(input$jsEndInput) <= values$maxX && values$isUpdatePressedEnd != FALSE)
    {
      updateNumericInput(session, "end", value = as.numeric(input$jsEndInput))
      values$isUpdatePressedEnd <- FALSE
    }
    
    output$maxCoords <- renderText({
      paste("Max x: ", values$maxX)
    })
    
    updateNumericInput(session, "selectChr", value = input$selectChr)
    
    if("Axis" %in% input$outerOptions | "Idiogram" %in% input$outerOptions)
    {
      values$trackHeight <- .1
    } else {
      values$trackHeight <- .125
    }
    
    if(!is.null(input$fileIn)){
      print(values$bed)
      values$bed <- read.delim(input$fileIn$datapath, sep = "\t")
    }
  })
  
  templateFile <- read.delim("template.txt")
  
  observeEvent(input$labelDir, {
    if(values$labelText == "clockwise")
    {
      values$labelText = "inside"
      updateActionButton(session, "labelDir", label = "Label format: )|")
    }
    else
    {
      values$labelText = "clockwise"
      updateActionButton(session, "labelDir", label = "Label format: )-")
    }
    
  })
  
  observeEvent(input$update, {
    values$isUpdatePressedStart <- TRUE
    values$isUpdatePressedEnd <- TRUE
  })
  
  observeEvent(input$sAdd10000000, {
    if(input$start + 10000000 <= values$maxX)
    {
      updateNumericInput(session, "start", value = 10000000 + input$start)
    }
  })
  
  observeEvent(input$sAdd10000, {
    if(input$start + 10000 <= values$maxX)
    {
      updateNumericInput(session, "start", value = 10000 + input$start)
    }
  })
  
  observeEvent(input$sAdd10, {
    if(input$start + 10 <= values$maxX)
    {
      updateNumericInput(session, "start", value = 10 + input$start)
    }
  })
  
  observeEvent(input$sSub10000000, {
    if(input$start - 10000000 >= 0)
    {
      updateNumericInput(session, "start", value = input$start - 10000000)
    }
  })
  
  observeEvent(input$sSub10000, {
    if(input$start - 10000 >= 0)
    {
      updateNumericInput(session, "start", value = input$start - 10000)
    }
  })
  
  observeEvent(input$sSub10, {
    if(input$start - 10 >= 0)
    {
      updateNumericInput(session, "start", value = input$start - 10)
    }
  })
  
  observeEvent(input$eAdd10000000, {
    if(input$end + 10000000 <= values$maxX)
    {
      updateNumericInput(session, "end", value = 10000000 + input$end)
    }
  })
  
  observeEvent(input$eAdd10000, {
    if(input$end + 10000 <= values$maxX)
    {
      updateNumericInput(session, "end", value = 10000 + input$end)
    }
  })
  
  observeEvent(input$eAdd10, {
    if(input$end + 10 <= values$maxX)
    {
      updateNumericInput(session, "end", value = 10 + input$end)
    }
  })
  
  observeEvent(input$eSub10000000, {
    if(input$end - 10000000 >= 0)
    {
      updateNumericInput(session, "end", value = input$end - 10000000)
    }
  })
  
  observeEvent(input$eSub10000, {
    if(input$end - 10000 >= 0)
    {
      updateNumericInput(session, "end", value = input$end - 10000)
    }
  })
  
  observeEvent(input$eSub10, {
    if(input$end - 10 >= 0)
    {
      updateNumericInput(session, "end", value = input$end - 10)
    }
  })
    
    #bed <- read.delim("testFigs2.bedgraph")

  
    output$genomePlot <- renderImage({
      
      width  <- session$clientData$output_plot_width
      height <- session$clientData$output_plot_height
      mysvgwidth <- width/96
      mysvgheight <- height/96
      
      outfile <- tempfile(fileext='.svg')
      
      svglite(outfile)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(input$outerChart == TRUE){
        #initialize with chr widths
        circos.par(start.degree = 90)
        circos.initializeWithIdeogram(plotType = NULL)
        #add chr labels
        if("Labels" %in% input$outerOptions){
          circos.track(ylim = c(0,1), track.height = .05, panel.fun = function(x,y){
            mid = get.cell.meta.data("xlim")
            chros = get.cell.meta.data("sector.index")
            if(values$labelText == "inside"){
              chros = strsplit(chros, "chr")[[1]][2]
            }
            circos.text(mean(mid), 3, facing = values$labelText, labels = chros, niceFacing = TRUE, cex = 1.5)
          }, bg.border = NA)
        }
        if("Axis" %in% input$outerOptions){
          circos.track(ylim = c(0, 1), panel.fun = function(x, y) circos.genomicAxis(), track.height = 0.05, bg.border = NA)
        }
        if("Idiogram" %in% input$outerOptions){
          circos.genomicIdeogram()
        }
        
        #add basic tract to show widths 037C6B 60c0b0
        if("Lines" %in% input$outerOptions){
          circos.genomicTrack(values$bed, track.height = values$trackHeight, bg.col = "#dddddd",
                              bg.border = NA, panel.fun = function(region, value, ...) {
            circos.genomicLines(region, value, area = TRUE, col = "#999999", border = NA, ...)
          })
        }
        
        if("Rainfall" %in% input$outerOptions){
          circos.genomicRainfall(values$bed, track.height = values$trackHeight, col = c("#55555580"), pch = 16, cex = 0.6)
        }
        
        if("Density" %in% input$outerOptions){
          circos.genomicDensity(values$bed, border = "black", col = "#eeeeee", track.height = values$trackHeight)
        }
        
        #highlight current chr
        if(input$selectChr %in% chrList){
          highlight.chromosome(input$selectChr)
        }
        circos.clear()
        }
      if(FALSE){
        #don't get rid of last circos
        par(mar = c(1, 1, 1, 1), new = TRUE)
        #set params to create right shape and size
        circos.par("canvas.xlim" = c(-1.75, 1.75), "canvas.ylim" = c(-1.75, 1.75), clock.wise = FALSE,
                   cell.padding = c(0, 0, 0, 0), gap.degree = 180)
        #add new chart
        circos.initializeWithIdeogram(chromosome.index = input$selectChr, plotType = NULL)
        circos.track(ylim = c(0, 1), track.height = .01, panel.fun = function(x, y){
          circos.genomicAxis(labels.cex = 1, major.by = 25000000)
          values$maxX <- get.cell.meta.data("xlim")[[2]]
        }, bg.border = NA)
        circos.genomicIdeogram(track.height = .2)
        circos.track(ylim = c(0, 1), track.height = .01, panel.fun = function(x, y){
          circos.rect(input$start, 0, input$end, 25, col = "#FF000030", border = NA)
        }, bg.border = NA)
        text(0, 0, input$selectChr, cex = 1.5)
        circos.clear()
      }
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      dev.off()
      
      list(src = normalizePath(outfile),
           contentType = 'image/svg+xml',
           width = width,
           height = width,
           alt = "My Plot")
    }, deleteFile = TRUE)
    #if(FALSE){
    output$chrPlot <- renderImage({
      
      width  <- session$clientData$output_plot_width
      height <- session$clientData$output_plot_height
      mysvgwidth <- width/96
      mysvgheight <- height/96
      
      outfile <- tempfile(fileext='.svg')
      
      svglite(outfile)
      plot(c(100, 250), c(300, 450), type = "n",
           main = "2 x 11 rectangles; `rect(100+i,300+i,  150+i,380+i)'")
      i <- 4*(0:10)
      ## draw rectangles with bottom left (100, 300)+i  and top right (150, 380)+i
      rect(100, 300, 150, 380, col="#666666")
      
      dev.off()
      
      list(src = normalizePath(outfile),
           contentType = 'image/svg+xml',
           width = width,
           height = width,
           alt = "My Plot")
    }, deleteFile = TRUE)
    #}
    output$browserOutput <-renderJBrowseR(
      JBrowseR("ViewHg19",
               location = paste(values$chrNumber(), ":", values$startCommas(), "..", values$endCommas(), sep = "")
               )
    )
    
    output$downloadTemp <- downloadHandler(
      filename = "template.txt",
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
