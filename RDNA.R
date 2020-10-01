install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyTree")
install.packages("shinyWidgets")
install.packages("shinyjs")
install.packages("readxl")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("DT")
install.packages("data.table")
install.packages("shinycustomloader")
install.packages("hexbin")
install.packages("colourpicker")
install.packages("r2d3")
install.packages("plotly")
install.packages("shinyjqui")



######load packages

library(shiny)
library(shinydashboard)
library(shinyTree)
library(shinyWidgets)
library(shinyjs)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DT)
library(data.table)
library(shinycustomloader)
library(hexbin)
library(colourpicker)
library(r2d3)
library(plotly)
library(shinyjqui)

options(shiny.maxRequestSize = -1) #no file size limit
shinyApp(
  ui <- div(style = "font-family:'calibri light'", 
            dashboardPage(
              dashboardHeader(title = span(tagList(icon("dna"))), disable = TRUE, titleWidth = 100,
                              dropdownMenu(type = "tasks", badgeStatus = "primary")
              ),
              dashboardSidebar(width = 100, collapsed = TRUE,
                               sidebarMenu(
                                 div(style = "margin-bottom:-.5em; background:#00000000; border-radius:0px; border-color:navy; border-width:0px; font-size:20px; text-align:center", icon("dna"), h6("DNA")), br(),
                                 menuItem(div(style = "font-size:14px","DNA"), tabName = "dna", icon = icon("dna"))
                                 #menuItem(div(style = "font-size:14px","Explore"), tabName = "dataviz", icon = icon("chart-bar")),
                                 #menuItem(div(style = "font-size:14px","Process"), tabName = "dataprocess", icon = icon("cogs")),
                                 #menuItem(div(style = "font-size:14px","Mine"), tabName = "datamine", icon = icon("gem")),
                                 #menuItem(div(style = "font-size:14px","Train|Predict"), tabName = "mlearning", icon = icon("brain")), br(),hr()
                               )
              ),
              dashboardBody(br(),
                            tabItems(
                              
                              
                              # Data Load dropdown button
                              tabItem(tabName = "dna",
                                      ######### Data Visualization dropdown button
                                      div(style = "margin-top:-1em",
                                          #jqui_draggable(
                                          dropdownButton(jqui_resizable(box(
                                            fluidRow(
                                              column(width = 2,
                                                     wellPanel(style = "background:#f9fbfb; border-radius:0px; border-color:white; border-width:.25px",
                                                               div(style = "font-size:10px",shinyTree("dataviztree", search = TRUE, animation = FALSE, theme = 'proton', dragAndDrop = TRUE, themeDots = TRUE, wholerow = TRUE, themeIcons = FALSE, unique = TRUE, multiple = FALSE)),
                                                               div(style = "font-size:10px",shinyTree("graphopttree", search = FALSE, animation = FALSE, theme = 'proton', dragAndDrop = TRUE, themeDots = TRUE, wholerow = TRUE, themeIcons = FALSE, unique = TRUE, multiple = FALSE)),
                                                               div(style = "font-size:10px",shinyTree("colorfacettree", search = FALSE, animation = FALSE, theme = 'proton', dragAndDrop = TRUE, themeDots = TRUE, wholerow = TRUE, themeIcons = FALSE, unique = TRUE, multiple = FALSE)),
                                                               div(style = "font-size:10px",shinyTree("axislabeltree", search = FALSE, animation = FALSE, theme = 'proton', dragAndDrop = TRUE, themeDots = TRUE, wholerow = TRUE, themeIcons = FALSE, unique = TRUE, multiple = FALSE))
                                                     )),
                                              column(width = 8,
                                                     div(style = "border-radius:0px; border-color:white", 
                                                         #box(width = 800, height = 670, status = "primary",
                                                         tags$style(type="text/css", style = "font-size: 18px",
                                                                    ".shiny-output-error { visibility: hidden; }",
                                                                    ".shiny-output-error:before { visibility: visible; content: ''}"
                                                         ),
                                                         #withLoader(plotOutput("graph", width = "900px", height = "675px"), type = "html", loader = "dnaspin")
                                                         withLoader(plotlyOutput("graph", width = "900px", height = "675px"), type = "html", loader = "dnaspin")
                                                         #)
                                                     )
                                              ),
                                              column(width = 2,
                                                     #wellPanel(style = "background:#f9fbfb; border-radius:1px; border-color:white; border-width:.25px; margin-bottom:10px",
                                                     hr(),
                                                     uiOutput("graphparameters")
                                                     #div(style = "font-size:10px",shinyTree("graphopttree", search = TRUE, animation = FALSE, theme = 'proton', dragAndDrop = TRUE, themeDots = TRUE, wholerow = TRUE, themeIcons = FALSE, unique = TRUE, multiple = FALSE))
                                                     #uiOutput("graphoptions")
                                                     #)
                                                     
                                              )
                                            ), width = "1400px", status = "success")),
                                            circle = TRUE, tooltip = TRUE, label = "Visualization",status = "success", icon = icon("chart-bar")
                                          ), br(),
                                          #)
                                      ), br(),
                                      
                                      ########Data Load
                                      jqui_sortable(div(id = "dropdownmenus", div(style = "margin-top:-1em", 
                                                                                  #jqui_draggable(
                                                                                  dropdownButton(
                                                                                    div(style = "font-size:10px",
                                                                                        div(style = "margin-bottom:-1em",
                                                                                            fluidRow(
                                                                                              column(width = 2,
                                                                                                     wellPanel(style = "background:#f9fbfb; border-radius:0px; border-color:white; border-width:.25px",    
                                                                                                               div(style = "margin-bottom:-1em; width:180px", selectInput("fileType_Input","File Type", choices = c("Select" = "", ".csv/.txt" = "csvtxt", ".xlsx/.xls" = "excel", ".dbf" = "dbf", ".shp" = "shp"), selected = "csvtxt")),
                                                                                                               div(style = "margin-bottom:-1em; width:180px", fileInput("datafile", "", multiple = TRUE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv", ".xlsx", ".xls", ".dbf", ".shp"))),
                                                                                                               conditionalPanel(
                                                                                                                 condition = "input.fileType_Input == 'csvtxt'",
                                                                                                                 div(style = "margin-top:-1em",
                                                                                                                     div(style = "width:180px", selectInput("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")),
                                                                                                                     # Input: Select quotes ----
                                                                                                                     div(style = "margin-top:-1em; width:180px", selectInput("quote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'))
                                                                                                                 )
                                                                                                               ),
                                                                                                               conditionalPanel(
                                                                                                                 condition = "input.fileType_Input == 'excel'",
                                                                                                                 div(style = "margin-top:-1em; width:180px", numericInput("sheet", "Sheet Number", value = 1, min = 1, max = NA, step = 1)
                                                                                                                 )
                                                                                                               ),
                                                                                                               div(style = "margin-bottom:0em",
                                                                                                                   div(style = "width:150px", checkboxInput("stringsAsFactors", "strings As Factors", TRUE)),
                                                                                                                   div(style = "width:150px", checkboxInput("col_names", "Column Names", TRUE)),
                                                                                                                   div(style = "width:150px", checkboxInput("row_names", "Include Row Names", TRUE)),
                                                                                                                   div(style = "width:180px", numericInput("skip", "Skip Row", value = 0, min = 0, max = 100, step = 1)),
                                                                                                                   div(style = "width:180px", textInput("na", "Missing Values", value = "")),
                                                                                                                   div(style = "width:150px", radioButtons("disp", "Display", choices = list("All" = "all", "Head" = "head", "Tail" = "tail"), selected = "all", inline = TRUE))
                                                                                                               )
                                                                                                     )
                                                                                              )
                                                                                            )   
                                                                                        )
                                                                                        
                                                                                    ), circle = TRUE, tooltip = TRUE, label = "Upload File",status = "primary", icon = icon("upload"), width = "250px"), br(),#)
                                      ), br(),
                                      ######## Data Table
                                      div(style = "margin-top:-1em", 
                                          #jqui_draggable(
                                          dropdownButton(
                                            div(style = "font-size:10px",
                                                div(style = "margin-bottom:-1em",
                                                    div(style = "font-size:10px", selectizeInput("fileset", "", choices = c("File 1" = "file1", "File 2" = "file2"), selected = "file1")),
                                                    jqui_resizable(
                                                      box(
                                                        #width = 1500, height = 600, status = "primary",
                                                        withLoader(DT::dataTableOutput("contents"), type = "html", loader = "dnaspin"),
                                                        checkboxGroupInput("show_vars", "", choices = names(df) , selected = names(df), inline = TRUE),
                                                        width = "1400px", status = "info"
                                                      )
                                                    )
                                                )
                                                
                                            ), circle = TRUE, tooltip = TRUE, label = "Data Table",status = "info", icon = icon("table")), br(),#)
                                      ), br(),
                                      
                                      #######Data Processing dropdown button
                                      div(style = "margin-top:-1em",
                                          #jqui_draggable(
                                          dropdownButton(jqui_resizable(box(
                                            div(style = "font-size:10px",
                                                fluidRow(
                                                  column(width = 2,
                                                         wellPanel(style = "background:#f9fbfb; border-radius:0px; border-color:white; border-width:.25px",
                                                                   div(style = "margin-top:-1em","Search"),
                                                                   div(style = "font-size:10px",shinyTree("processtree", search = TRUE, animation = FALSE, theme = 'proton', dragAndDrop = TRUE, themeDots = TRUE, wholerow = TRUE, themeIcons = FALSE, unique = TRUE, multiple = FALSE)))),
                                                  column(width = 8, offset = 0,
                                                         div(style = "border-radius:0px; border-color:white",
                                                             withLoader(DT::dataTableOutput("contents_dataprocess"), type = "html", loader = "dnaspin"),br(), br(),
                                                             textAreaInput("codehere", "Your R code here", resize = "vertical", height = 150)
                                                         )
                                                  ),
                                                  column(width = 2,
                                                         wellPanel(style = "background:#f9fbfb; border-radius:1px; border-color:white; border-width:.25px; margin-bottom:10px",
                                                                   uiOutput("innerjoindata")
                                                         )
                                                         
                                                  )
                                                )
                                            ), width = "1400px", status = "danger")), circle = TRUE, tooltip = TRUE, label = "Data Processing",status = "danger", icon = icon("cogs")), br(),#)
                                          
                                      ), br(),
                                      
                                      ########Data Mining dropdown button
                                      div(style = "margin-top:-1em",
                                          #jqui_draggable(
                                          dropdownButton(
                                            fluidRow(
                                              column(width = 2,
                                                     wellPanel(style = "background:#f9fbfb; border-radius:0px; border-color:white; border-width:.25px",
                                                     )),
                                              column(width = 8, offset = 0,
                                                     div(style = "border-radius:0px; border-color:white", box(
                                                       width = 800, height = 670, status = "primary"
                                                     ))
                                              ),
                                              column(width = 2,
                                                     wellPanel(style = "background:#f9fbfb; border-radius:1px; border-color:white; border-width:.25px; margin-bottom:10px")
                                              )
                                            ),
                                            circle = TRUE, tooltip = TRUE, label = "Data Mining",status = "warning", icon = icon("gem")
                                          ), br(),
                                          #)
                                      ), br(),
                                      
                                      #######Machine Learning dropdown button
                                      div(style = "margin-top:-1em",
                                          #jqui_draggable(
                                          dropdownButton(
                                            fluidRow(
                                              column(width = 2,
                                                     wellPanel(style = "background:#f9fbfb; border-radius:0px; border-color:white; border-width:.25px",
                                                     )),
                                              column(width = 8, offset = -1,
                                                     div(style = "border-radius:0px; border-color:white", box(
                                                       width = 800, height = 670, status = "primary"
                                                     ))
                                              ),
                                              column(width = 3,
                                                     wellPanel(style = "background:#f9fbfb; border-radius:1px; border-color:white; border-width:.25px; margin-bottom:10px")
                                                     
                                              )
                                            ),
                                            circle = TRUE, tooltip = TRUE, label = "Machine Learning",status = "primary", icon = icon("brain"), width = "1400px"
                                          ), br(),
                                          #)
                                      ), br()))
                                      
                              )
                            )
              )
            )
  ),
  
  
  server = function(input, output, session) {
    output$processtree <- renderTree({
      sss= list(
        'Combine Datasets' = list(
          'Join' = structure(list('inner join'='1', 'left join'='2', 'right join'='3', 'full join'='4', 'semi join'='5', 'anti join'='6'), stopened = FALSE),
          'Operations' = structure(list('difference'='7', "intersect"='8', 'union'='10'), stopened = FALSE)
        ),
        'Transform'   =  structure(list('arc-sine'='11', 'logarithmic'='12', 'square-root'='13', 'normalization'='14', 'standardization'='15', 'group and summarize'='16'),stopened=FALSE),
        'Reshape'   =  structure(list('gather'='17', 'unite'='18', 'separate'='19', 'spread'='20'), stopened=FALSE),
        'Subset' = list(
          'Features' = structure(list('that are'='21', 'that contain(s)'='22', 'that start(s) with'='23', 'that end(s) with'='24'), stopened = FALSE),
          'Observations' = structure(list('extract rows'='25', 'random subset'='26', 'remove duplicate'='27', 'row slicing'='28', 'Top n rows'='29'),stopened=FALSE) 
        ),
        'Additional Wrangling Tools' = list(
          'Create sequences' = structure(list('of non-random numbers'='30', 'of random numbers'='31', 'of dates' = '32'), stopened = FALSE),
          'Conversion' = structure(list('integer to double'='33', 'double to integer'='34', 'recode factors'='35', 'strings to dates'='36', 'to factor'='37', 'to strings'='38', 'to lower case'='39', 'to upper case'='40', 'to title'='41'), stopened = FALSE),
          'Functions' = structure(list('apply'='42'), stopened = FALSE),
          'Missing Values'= structure(list('exclude'='43', 'recode'='44', 'test for'='45'), stopened = FALSE)
        )
        
      )
      #attr(sss[[1]],"stopened")=TRUE 
      sss
      
    })
    
    
    output$dataviztree <- renderTree({
      sss= list(
        'Graphs' = list(
          "Univariate" = list(
            "Continuous x" = structure(list("Histogram"="histo_cx", "Dotplot1D"="dotplot1d_cx"), stopened = FALSE),
            "Discrete x" = structure(list("Barplot1D"="barplot1d_dx"), stopened = FALSE)
          ),
          "Bivariate" = list(
            "Continuous x - Continuous y" = structure(list("Contour2D"='cont2d_cxcy', "Hexbins"="hexbinx_cxcy", "Scatterplot" = "scatter_cxcy"), stopened = FALSE),
            "Discrete x - Continuous y" = structure(list("Barplot2D" = "barplot2d", "Boxplot"="boxplot_dxcy", "Dotplot2D"="dotplot2d_dxcy", "Violin"="violin_dxcy"), stopened = FALSE),
            "Discrete x - Discrete y" = structure(list("Countplot"="countplot"), stopened = FALSE)
          ),
          "Mutivariate" = structure(list("Alluvial"="alluvial_nxy", "Heatmap" = "heatmap_nxy", "Mosaic"="mosaic_nxy", "Treemap"="treemap_nxy"), stopened = FALSE)
        )
        #"Options" = structure(list("Alluvial" = "alluvialplot", "Barplot2D" = "barplot2d", "Barplot1D" = "barplot1d", "Boxplot" = "box", "Contour 2D" = "cont2d", "Countplot" = "countplot", "Dotplot2D" = "dot2d", "Dotplot1D" = "dotplot1d", "Heatmap" = "heatmap", "Hexbins" = "hexbins", "Histogram" = "histo", "Mosaic" = "mosaicplot", "scatteropt" = "Scatterplot", "Treemap" = "treemap", "Violin" = "violin"), stopened = FALSE),
        #"Color and Facets" = structure(list("Color by" = "colorby", "Facet" = "facet"), stopened = FALSE),
        #"Axis Labels" = structure(list("Axis xy" = "axisxy"), stopened = FALSE)
        
      )
      #attr(sss[[1]],"stopened")=TRUE
      sss
      
    })
    
    
    output$colorfacettree <- renderTree({
      sss= list(
        "Color and Facets" = structure(list("Color by" = "colorby", "Facet" = "facet"), stopened = FALSE)
      )
      sss
    })
    output$axislabeltree <- renderTree({
      sss= list(
        "Axis Labels" = structure(list("Axis xy" = "axisxy"), stopened = FALSE)
      )
      sss
    })
    output$graphopttree <- renderTree({
      sss= list(
        "Graph Options" = structure(list("Alluvial" = "alluvialplot", "Barplot2D" = "barplot2d", "Barplot1D" = "barplot1d", "Boxplot" = "box", "Contour 2D" = "cont2d", "Countplot" = "countplot", "Dotplot 2D" = "dot2d", "Dotplot1D" = "dotplot1d", "Heatmap" = "heatmap", "Hexbins" = "hexbins", "Histogram" = "histo", "Mosaic" = "mosaicplot", "Scatterplot" = "Scatteropt", "Treemap" = "treemap", "Violin" = "violin"), stopened = FALSE)
      )
      sss
    })
    
    
    output$innerjoindata <- renderUI({
      if('inner join' %in% get_selected(input$processtree) | 'left join' %in% get_selected(input$processtree) | 'right join' %in% get_selected(input$processtree) | 'full join' %in% get_selected(input$processtree) | 'semi join' %in% get_selected(input$processtree) | 'anti join' %in% get_selected(input$processtree)) {
        tagList(
          div(style = "font-size:11px; margin-top:-1em", selectInput("joindata", "Data sets", choices = "", selected = "")),
          div(style = "font-size:11px; margin-bottom:-2em", selectInput("joinby_id", "Join by", choices = "", selected = ""))
        )
      } else if ('difference' %in% get_selected(input$processtree) | 'intersect' %in% get_selected(input$processtree) | 'union' %in% get_selected(input$processtree)) {
        div(style = "font-size:11px; margin-top:-1em; margin-bottom:-2em", selectInput("joindata", "Data sets", choices = "", selected = ""))
      }
    })
    
    dataload <- reactive({
      req(input$datafile)
      tryCatch(
        {
          if (length(input$datafile$datapath) == 1 & input$fileType_Input == "csvtxt" & (input$fileset == "file1" | input$fileset == "file2")) {
            df <- read.csv(input$datafile[1,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          }
          if (length(input$datafile$datapath) == 2 & input$fileType_Input == "csvtxt" & input$fileset == "file1") {
            df <- read.csv(input$datafile[1,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          }
          if (length(input$datafile$datapath) == 2 & input$fileType_Input == "csvtxt" & input$fileset == "file2") {
            df <- read.csv(input$datafile[2,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          }
          #    if (length(input$datafile$datapath) == 3 & input$fileType_Input == "csvtxt" & input$fileset == "file1") {
          #      df <- read.csv(input$datafile[1,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          #    }
          #    if (length(input$datafile$datapath) == 3 & input$fileType_Input == "csvtxt" & input$fileset == "file2") {
          #      df <- read.csv(input$datafile[2,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          #    }
          #    if (length(input$datafile$datapath) == 3 & input$fileType_Input == "csvtxt" & input$fileset == "file3") {
          #      df <- read.csv(input$datafile[3,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          #    }
          #    if (length(input$datafile$datapath) == 4 & input$fileType_Input == "csvtxt" & input$fileset == "file1") {
          #      df <- read.csv(input$datafile[1,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          #    }
          #    if (length(input$datafile$datapath) == 4 & input$fileType_Input == "csvtxt" & input$fileset == "file2") {
          #      df <- read.csv(input$datafile[2,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          #    }
          #    if (length(input$datafile$datapath) == 4 & input$fileType_Input == "csvtxt" & input$fileset == "file3") {
          #      df <- read.csv(input$datafile[3,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          #    }
          #    if (length(input$datafile$datapath) == 4 & input$fileType_Input == "csvtxt" & input$fileset == "file4") {
          #      df <- read.csv(input$datafile[4,4], header = input$col_names, sep = input$sep, quote = input$quote, stringsAsFactors = input$stringsAsFactors, na = input$na, skip = input$skip)
          #    }
          
          
          
          
          
          if (length(input$datafile$datapath) == 1 & input$fileType_Input == "excel" & (input$fileset == "file1" | input$fileset == "file2")) {
            df <- read_excel(input$datafile[1,4], sheet = input$sheet, skip = input$skip, col_names = input$col_names, na = input$na)
          }
          if (length(input$datafile$datapath) == 2 & input$fileType_Input == "excel" & input$fileset == "file1") {
            df <- read_excel(input$datafile[1,4], sheet = input$sheet, skip = input$skip, col_names = input$col_names, na = input$na)
          }
          if (length(input$datafile$datapath) == 2 & input$fileType_Input == "excel" & input$fileset == "file2") {
            df <- read_excel(input$datafile[2,4], sheet = input$sheet, skip = input$skip, col_names = input$col_names, na = input$na)
          }
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      updateCheckboxGroupInput(session, "show_vars", "", choices = names(df), selected = names(df), inline = TRUE)
      
      output$graphparameters <- renderUI({
        if("Histogram" %in% get_selected(input$dataviztree)) {
          tagList(
            div(style = "font-size:11px; margin-top:1em", selectInput("xuniv_histo", "X - axis", choices = c("Select X" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1])),
            if("Histogram" %in% get_selected(input$graphopttree))
              tagList(
                div(style = "font-size:11px; display: inline-block; vertical-align:top; width:100px", selectInput("histo_pos", em("Position"), choices = c("Identity" = "identity", "Stack" = "stack"), selected = "stack")),
                div(style = "font-size:11px; display: inline-block; vertical-align:top; width:100px", numericInput("histo_size", em("Size"), value = .5, min = 0, max = 5, step = .1)),
                div(style = "font-size:11px; ", sliderInput("histobins", em("Number of Bins"), value = 20, min = 1, max = 100, step = 1, ticks = FALSE)),
                div(style = "font-size:11px; ", sliderInput("histotrans", em("Histogram Transparency"), value = 1, min = 0, max = 1, step = .1, ticks = FALSE)),
                div(style = "font-size:10px", radioGroupButtons("histo_freq_dens", em("Add Plot"), choices = c("-->" = "", "Frequency"= "freqplot", "Density" = "dens"), status = "primary", direction = "horizontal", size = "sm", justified = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")), selected = "")),
                conditionalPanel(
                  condition = "input.histo_freq_dens == 'freqplot'",
                  #div(style = "font-size:11px; display: inline-block; vertical-align:top; width:100px", selectInput("freqplot_pos", em("Position"), choices = c("Identity" = "identity", "Stack" = "stack"), selected = "stack")),
                  div(style = "font-size:11px", numericInput("freqplot_size", em("Line Size"), value = 1, min = 0, max = 5, step = .1)),
                  div(style = "font-size:11px", colourInput("freqplot_linecol", em("Line Color"), value = "dodgerblue", palette = "limited", returnName = TRUE))
                ),
                conditionalPanel(
                  condition = "input.histo_freq_dens == 'dens'",
                  div(style = "font-size:11px; display: inline-block; vertical-align:top; width:100px", selectInput("dens_kernel", em("Kernel"), choices = c("Gaussian"="gaussian", "Epanechnikov"="epanechnikov", "Rectangular"="rectangular", "Triangular"="triangular", "Biweight"="biweight", "Cosine"="cosine", "Optcosine"="optcosine"), selected = "gaussian")),
                  div(style = "font-size:11px; display: inline-block; vertical-align:top; width:100px", numericInput("dens_size", em("Line Size"), value = 1, min = 0, max = 5, step = .1)),
                  div(style = "font-size:11px", colourInput("denslinecol", em("Line Color"), value = "dodgerblue", palette = "limited", returnName = TRUE))
                  #div(style = "font-size:11px; ", sliderInput("dens_trans", em("Density Transparency"), value = .5, min = 0, max = 1, step = .1))
                )
              )
          )
          
        } else if("Dotplot1D" %in% get_selected(input$dataviztree)) {
          tagList(
            div(style = "font-size:11px; margin-top:-1em", selectInput("xuniv_dot1d", "X - axis", choices = c("Select X" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1])),
            if ("Dotplot1D" %in% get_selected(input$graphopttree))
              tagList(
                div(style = "font-size:11px; ", numericInput("dot1dbins", em("Binwidth"), value = 1, min = .1, max = NA, step = .01)),
                div(style = "font-size:11px", numericInput("dotplot1size", em("Dot size"), value = 1, min = .1, max = NA, step = .1))
              )
          )
          
        } else if("Barplot1D" %in% get_selected(input$dataviztree)) {
          div(style = "font-size:11px; margin-top:-1em", selectInput("xuniv_bar1d", "X - axis", choices = c("Select X" = "", names(df[sapply(df, is.factor)])), selected = names(df[sapply(df, is.factor)])[1]))
        } else if("Contour2D" %in% get_selected(input$dataviztree)) {
          tagList(
            div(style = "font-size:11px; margin-top:-1em", selectInput("xbiv_cont2d", "X - axis", choices = c("Select X" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1])),
            div(style = "font-size:11px", selectInput("ybiv_cont2d", "Y - axis", choices = c("Select X" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1])),
            div(style = "font-size:11px; margin-bottom:2em", selectInput("cline_surfdens", "", choices = c("Contour Lines" = "contline", "Filled Polygon" = "poly", "Surface Density" = "surfdens"), selected = "contline")),
            if ("Contour 2D" %in% get_selected(input$graphopttree))
              tagList(
                div(style = "font-size:11px", numericInput("contbins", em("Number of Bins"), value = 10, min = 1, max = 100, step = 1)),
                div(style = "font-size:11px", sliderInput("conttrans", em("Contour Line Transparency"), value = .5, min = 0, max = 1, step = .1, ticks = FALSE)),
                #div(style = "font-size:11px", checkboxInput("addscatterpoint", "Add Symbols", value = FALSE)),
                div(style = "font-size:11px; ", sliderInput("contdottrans", em("Symbol Transparency"), value = .5, min = 0, max = 1, step = .1, ticks = FALSE))
              )
          )
        } else if("Scatterplot" %in% get_selected(input$dataviztree)) {
          tagList(
            div(style = "font-size:11px; margin-top:-1em", selectInput("xbiv_scat", "X - axis", choices = c("Select X" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1])),
            div(style = "font-size:11px; margin-bottom: 1em", selectInput("ybiv_scat", "Y - axis", choices = c("Select X" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1])),
            if ("Scatterplot" %in% get_selected(input$graphopttree))
              tagList(
                div(style = "font-size:11px", numericInput("symb_size", em("Size"), value = 2, min = 0, max = 10, step = .1)),
                div(style = "font-size:11px", sliderInput("symb_transp", em("Symbol Transparency"), min = 0, max = 1, value = 1, step = 0.01, ticks = FALSE)),
                div(style = "font-size:10px", radioGroupButtons("shapesizesmoo", "", choices = c("-->" = "", "Shape"= "shapeby", "Size" = "sizeby", "Smooth" = "smooth"), status = "primary", direction = "horizontal", size = "xs", justified = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")), selected = "")),
                conditionalPanel(
                  condition = "input.shapesizesmoo == 'shapeby'",
                  div(style = "font-size:11px", selectInput("shapeby_feat", em("By"), choices = c("Select X" = "NULL", names(df[sapply(df, is.factor)])), selected = "NULL")),
                  conditionalPanel(
                    condition = "input.shapeby_feat != 'NULL'",
                    div(style = "font-size:11px", radioGroupButtons("symb_solid", "", choices = c("Solid"= TRUE, "Hollow" = FALSE), justified = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")), size = "sm", selected = TRUE))
                  )
                ),
                conditionalPanel(
                  condition = "input.shapesizesmoo == 'sizeby'",
                  div(style = "font-size:11px; display: inline-block; vertical-align:top; width: 120px", selectInput("sizeby_feat", em("By"), choices = c("Select X" = "NULL", names(df[sapply(df, is.numeric)])), selected = "NULL")),
                  div(style = "font-size:11px; display: inline-block; vertical-align:top; width: 80px", numericInput("symb_maxsize", em("Maximum Size"), value = 5, min = 0, max = NA, step = .1))
                  #div(style = "font-size:11px; display: inline-block; vertical-align:top; width: 120px", selectInput("fill_feat", em("Fill"), choices = c("Select X" = "NULL", "cornsilk"), selected = "NULL"))
                ),
                conditionalPanel(
                  condition = "input.shapesizesmoo == 'smooth'",
                  div(style = "font-size:11px", selectInput("smoothmethod", "", choices = c("Select smoothing method" = "NULL", "Generalized linear model" = "glm", "Linear model" = "lm", "LOESS" = "loess"), selected = "NULL")),
                  conditionalPanel(
                    condition = "input.smoothmethod == 'glm'",
                    div(style = "font-size:11px", selectInput("glmfamily", "", choices = c("Select Family" = "NULL", "Binomial" = "binomial", "Gaussian" = "gaussian", "Gamma" = "Gamma", "Inverse Gaussian" = "inverse.gaussian", "Multinomial" = "multinomial", "Poisson" = "poisson", "Quasi" = "quasi", "Quasi binomial" = "quasibinomial", "Quasi poisson" = "quasipoisson"), selected = "NULL"))
                  ),
                  div(style = "font-size:11px", sliderInput("cilevel", em("Confidence Interval"), min = .9, max = .99, value = .95, step = 0.01, ticks = FALSE))
                )
              )
          )
        } else if("Hexbins" %in% get_selected(input$dataviztree)) {
          tagList(
            div(style = "font-size:11px; margin-top:-1em", selectInput("xbiv_hex", "X - axis", choices = c("Select X" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1])),
            div(style = "font-size:11px; margin-bottom: 1em", selectInput("ybiv_hex", "Y - axis", choices = c("Select X" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1]))
          )
        } else if ("Barplot2D" %in% get_selected(input$dataviztree) | "Boxplot" %in% get_selected(input$dataviztree) | "Violin" %in% get_selected(input$dataviztree)) {
          tagList(
            div(style = "font-size:11px; margin-top:-1em", selectInput("xbiv_bar2dboxvio", "X - axis", choices = c("Select X" = "", names(df[sapply(df, is.factor)])), selected = names(df[sapply(df, is.factor)])[1])),
            div(style = "font-size:11px; margin-bottom:1em", selectInput("ybiv_bar2dboxvio", "Y - axis", choices = c("Select Y" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1]))
          )
        } else if ("Dotplot2D" %in% get_selected(input$dataviztree)) {
          tagList(
            div(style = "font-size:11px; margin-top:-1em", selectInput("xbiv_dot2d", "X - axis", choices = c("Select X" = "", names(df[sapply(df, is.factor)])), selected = names(df[sapply(df, is.factor)])[1])),
            div(style = "font-size:11px; margin-bottom:1em", selectInput("ybiv_dot2d", "Y - axis", choices = c("Select Y" = "", names(df[sapply(df, is.numeric)])), selected = names(df[sapply(df, is.numeric)])[1])),
            if ("Dotplot 2D" %in% get_selected(input$graphopttree))
              tagList(
                div(style = "font-size:11px", sliderInput("dotplot2binw", em("Binwidth"), value = .5, min = .1, max = 1, step = .1, ticks = FALSE)),
                div(style = "font-size:11px", numericInput("dotplot2size", em("Dot size"), value = 1, min = .1, max = NA, step = .1)),
                div(style = "font-size:11px; display: inline-block; vertical-align:top; width:95px", selectInput("dotplot2fill", em("Fill"), choices = c("Hollow" = "NA", "Black" = "black"), selected = "black")),
                div(style = "font-size:11px; display: inline-block; vertical-align:top; width:95px", selectInput("stackdirect", em("Stack Direction"), choices = c("Center" = "center", "Up" = "up"), selected = "center"))
                #div(style = "font-size:11px", prettySwitch("dotplot2fill", em("Fill"), value = TRUE))
              )
          )
        } else if ("Countplot" %in% get_selected(input$dataviztree)) {
          tagList(
            div(style = "font-size:11px; margin-top:-1em", selectInput("xbiv_count", "X - axis", choices = c("Select X" = "", names(df[sapply(df, is.factor)])), selected = names(df[sapply(df, is.factor)])[1])),
            div(style = "font-size:11px; margin-bottom:1em", selectInput("ybiv_count", "Y - axis", choices = c("Select X" = "", names(df[sapply(df, is.factor)])), selected = names(df[sapply(df, is.factor)])[1]))
          )
        }
        
      })
      if(input$disp == "head") {
        return(head(df))
      }
      if(input$disp == "tail") {
        return(tail(df))
      } else {
        return(df)
      }
    })
    
    output$contents <- DT::renderDataTable({
      DT::datatable(dataload()[,input$show_vars], options = list(lengthMenu = list(seq(10, 100, 10), c('10','20','30','40','50','60','70','80','90','100')),pageLength = 10))
      #datafile()[,input$show_vars] 
    })
    output$contents_dataprocess <- DT::renderDataTable({
      DT::datatable(dataload()[,input$show_vars], options = list(lengthMenu = list(seq(10, 100, 10), c('10','20','30','40','50','60','70','80','90','100')),pageLength = 10))
      #datafile()[,input$show_vars] 
    })
    
    glm_smooth <- function(...) {
      geom_smooth(method = input$smoothmethod, method.args = list(family = input$glmfamily), ...)
    }
    
    #output$graph <- renderPlot({
    output$graph <- renderPlotly({
      
      #Pot empty ggplot
      if (is.null(unlist(get_selected(input$dataviztree)))){
        p <- ggplot(dataload())}
      ###Plot Histogram
      if ("Histogram" %in% get_selected(input$dataviztree) & length(input$xuniv_histo) == 1)
        p <- ggplot(dataload(), aes_string(x = input$xuniv_histo)) + geom_histogram(color = "white")
      if (length(input$xuniv_histo) == 1 & "Histogram" %in% get_selected(input$dataviztree) & "Histogram" %in% get_selected(input$graphopttree)) {
        p <- ggplot(dataload(), aes_string(x = input$xuniv_histo)) + geom_histogram(aes(fill = after_stat(count)), size = input$histo_size, position = input$histo_pos, color = "white", bins = input$histobins, alpha = input$histotrans) + scale_fill_viridis_c()
        if (input$histo_freq_dens == "freqplot")
          p <- p + geom_freqpoly(size = input$freqplot_size, colour = input$freqplot_linecol, position = input$histo_pos, bins = input$histobins)
        if (input$histo_freq_dens == "dens")
          p <- p + aes(y = ..density..) + geom_density(size = input$dens_size, colour = input$denslinecol, position = input$histo_pos, kernel = input$dens_kernel)
      }
      
      
      
      ###Plot Dotplot1D
      if ("Dotplot1D" %in% get_selected(input$dataviztree) & length(input$xuniv_dot1d) == 1)
        p <- ggplot(dataload(), aes_string(x = input$xuniv_dot1d)) + geom_dotplot(method = "histodot", binwidth = input$dot1dbins) + scale_y_continuous(NULL, breaks = NULL)
      if(length(input$xuniv_dot1d) == 1 & "Dotplot1D" %in% get_selected(input$dataviztree) & "Dotplot1D" %in% get_selected(input$graphopttree)){
        p <- ggplot(dataload(), aes_string(x = input$xuniv_dot1d)) + geom_dotplot(aes(fill = ..count..), method = "histodot", binwidth = input$dot1dbins, dotsize = input$dotplot1size) + scale_y_continuous(NULL, breaks = NULL) + scale_fill_viridis_c()
      }
      
      ###Plot Barplot one dimension
      if (length(input$xuniv_bar1d) == 1 & "Barplot1D" %in% get_selected(input$dataviztree))
        p <- ggplot(dataload(), aes_string(x = input$xuniv_bar1d)) + geom_bar(color = "white")
      
      ###Plot Hexagon bins
      if (length(input$xbiv_hex) == 1 & length(input$ybiv_hex) == 1 & "Hexbins" %in% get_selected(input$dataviztree))
        p <- ggplot(dataload(), aes_string(x = input$xbiv_hex, y = input$ybiv_hex)) + stat_binhex() + scale_fill_viridis_c()
      
      ###Plot Scatterplot
      if (length(input$xbiv_scat) == 1 & length(input$ybiv_scat) == 1 & "Scatterplot" %in% get_selected(input$dataviztree))
        p <- ggplot(dataload(), aes_string(x = input$xbiv_scat, y = input$ybiv_scat)) + geom_point()
      #p <- ggplot(dataload(), aes_string(x = input$xbiv_scat, y = input$ybiv_scat)) + geom_point(aes_string(shape = input$shapeby_feat), size = input$symb_size, alpha = input$symb_transp) + scale_shape(solid = input$symb_solid)
      if (length(input$xbiv_scat) == 1 & length(input$ybiv_scat) == 1 & "Scatterplot" %in% get_selected(input$dataviztree) & "Scatterplot" %in% get_selected(input$graphopttree)){
        p <- ggplot(dataload(), aes_string(x = input$xbiv_scat, y = input$ybiv_scat)) + geom_point(aes_string(shape = input$shapeby_feat), size = input$symb_size, alpha = input$symb_transp) + scale_shape(solid = input$symb_solid)
        if(input$shapesizesmoo == 'sizeby' & input$sizeby_feat != "NULL")
          p <- ggplot(dataload(), aes_string(x = input$xbiv_scat, y = input$ybiv_scat)) + geom_point(aes_string(size = input$sizeby_feat), shape = 21, fill = "cornsilk", alpha = input$symb_transp) + scale_size_area(max_size = input$symb_maxsize)
        
        if(input$shapesizesmoo == "smooth" & input$smoothmethod != "NULL")
          p <- ggplot(dataload(), aes_string(x = input$xbiv_scat, y = input$ybiv_scat)) + geom_point(aes_string(shape = input$shapeby_feat), size = input$symb_size, alpha = input$symb_transp) + geom_smooth(method = input$smoothmethod, fullrange = TRUE, level = input$cilevel, method.args = list(family = input$glmfamily))
        
        
        #if(input$shapesizesmoo == "smooth" & input$smoothmethod == "lm")
        #p <- ggplot(dataload(), aes_string(x = input$xbiv_scat, y = input$ybiv_scat)) + geom_point(aes_string(shape = input$shapeby_feat), size = input$symb_size, alpha = input$symb_transp) + glm_smooth(level = input$cilevel, fullrange=TRUE)
        #if(input$shapesizesmoo == "smooth" & input$smoothmethod == "auto")
        #p <- ggplot(dataload(), aes_string(x = input$xbiv_scat, y = input$ybiv_scat)) + geom_point(aes_string(shape = input$shapeby_feat), size = input$symb_size, alpha = input$symb_transp) + glm_smooth(level = input$cilevel, fullrange=TRUE)
        #if(input$shapesizesmoo == "smooth" & input$smoothmethod == "glm" & input$glmfamily != "NULL")
        #p <- ggplot(dataload(), aes_string(x = input$xbiv_scat, y = input$ybiv_scat)) + geom_point(aes_string(shape = input$shapeby_feat), size = input$symb_size, alpha = input$symb_transp) + glm_smooth(level = input$cilevel, fullrange=TRUE)
        
      }
      
      ###Plot Contour lines and density surface
      if (length(input$xbiv_cont2d) == 1 & length(input$ybiv_cont2d) == 1 &  "Contour2D" %in% get_selected(input$dataviztree)){
        if (input$cline_surfdens == "contline")###contour lines
          p <- ggplot(dataload(), aes_string(x = input$xbiv_cont2d, y = input$ybiv_cont2d)) + stat_density2d(bins = input$contbins, bins = input$contbins)
        if (input$cline_surfdens == "poly")###filled polygon
          p <- ggplot(dataload(), aes_string(x = input$xbiv_cont2d, y = input$ybiv_cont2d)) + stat_density2d(aes(fill= after_stat(level)), geom="polygon", bins = input$contbins) + scale_fill_viridis_c()
        if (input$cline_surfdens == "surfdens")###surface density
          p <- ggplot(dataload(), aes_string(x = input$xbiv_cont2d, y = input$ybiv_cont2d)) + stat_density2d(aes(fill= after_stat(density)), geom="raster", bins = input$contbins, contour = FALSE) + scale_fill_viridis_c()
        if ("Contour 2D" %in% get_selected(input$graphopttree))
          p <- p + stat_density2d(alpha = input$conttrans) + geom_point(size = 2, alpha = input$contdottrans)
      }
      ###Plot Barplot 2 dimensions  
      if (length(input$xbiv_bar2dboxvio == 1) & length(input$ybiv_bar2dboxvio) == 1 & "Barplot2D" %in% get_selected(input$dataviztree))
        p <- ggplot(dataload(), aes_string(x = input$xbiv_bar2dboxvio, y = input$ybiv_bar2dboxvio)) + geom_col()
      
      ###Plot Boxplot 2 dimensions  
      if (length(input$xbiv_bar2dboxvio == 1) & length(input$ybiv_bar2dboxvio) == 1 & "Boxplot" %in% get_selected(input$dataviztree))
        p <- ggplot(dataload(), aes_string(x = input$xbiv_bar2dboxvio, y = input$ybiv_bar2dboxvio)) + geom_boxplot()
      
      ###Plot Dotplot 2 dimensions  
      if (length(input$xbiv_dot2d == 1) & length(input$ybiv_dot2d) == 1 & "Dotplot2D" %in% get_selected(input$dataviztree))
        p <- ggplot(dataload(), aes_string(x = input$xbiv_dot2d, y = input$ybiv_dot2d)) + geom_dotplot(binaxis = "y", na.rm = TRUE, binwidth = .2, dotsize = 2, method = "histodot", binpositions = "all")
      if ("Dotplot2D" %in% get_selected(input$dataviztree) & "Dotplot 2D" %in% get_selected(input$graphopttree))
        p <- ggplot(dataload(), aes_string(x = input$xbiv_dot2d, y = input$ybiv_dot2d)) + geom_dotplot(fill = input$dotplot2fill, binaxis = "y", na.rm = TRUE, binwidth = input$dotplot2binw, dotsize = input$dotplot2size, stackdir = input$stackdirect, method = "histodot", binpositions = "all")
      
      ###Plot Violin
      if (length(input$xbiv_bar2dboxvio == 1) & length(input$ybiv_bar2dboxvio) == 1 & "Violin" %in% get_selected(input$dataviztree))
        p <- ggplot(dataload(), aes_string(x = input$xbiv_bar2dboxvio, y = input$ybiv_bar2dboxvio)) +  geom_violin(scale = "area")
      
      ###Plot Countplot  
      if (length(input$xbiv_count == 1) & length(input$ybiv_count) == 1 & "Countplot" %in% get_selected(input$dataviztree))
        p <- ggplot(dataload(), aes_string(x = input$xbiv_count, y = input$ybiv_count)) +  geom_count()  
      
      
      #print(p)
      print(ggplotly(p) %>% hide_legend() %>% config(displaylogo = FALSE))
    })
    
    
  }
)





