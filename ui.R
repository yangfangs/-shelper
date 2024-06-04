#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinyFiles)
library(shinyjs)
shinyUI(fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
 
  # Application title
  titlePanel("Shelper"),
  
  # Main tabs or panels for Primer and Variant
  tabsetPanel(
    tabPanel("Primer-Design",
             sidebarLayout(
               sidebarPanel(
            
                 radioButtons("genomeVersion2", "1. Select Genome Version:",
                              choices = c("hg19 (GRCh37)" = "hg19", "hg38 (GRCh38)" = "hg38"),
                              selected = "hg19"),
                 # Input field for location
                 textInput("location_primer", "2. Input Genome Location:"),
                 # # File upload control 
                 # fileInput("file_primer", "File upload:"),
                 # Submit button
                 
                 checkboxInput('example1', tags$span(style="color: red", 'Load Example Data'), FALSE),
                 actionButton("submit_primer", "Submit"),
                 tabPanel("Statistics", includeHTML("Statistics.html")),
               ),
               mainPanel(
                 # Nested tabs for Plot and Results
                 tabsetPanel(
                   id = "mainTabset2", 
                   tabPanel("Help", includeHTML("primerHelp.html")),
                   # tabPanel("primer", tableOutput("primer"))
                   tabPanel("primer",  uiOutput("tables")) 
                  
                 )
               )
             ),
    ),
    tabPanel("Variant-Validation",
             sidebarLayout(
               sidebarPanel(
            
                 radioButtons("genomeVersion", "1. Select Genome Version:",
                              choices = c("hg19 (GRCh37)" = "hg19", "hg38 (GRCh38)" = "hg38"),
                              selected = "hg19"),
             
                 textAreaInput("textData", "2. Input vilidate location and file name (split by comma ',' )", "", rows = 10),
        
                 fileInput("file1", "3. Upload Chromatogram File (.ab1 or .scf)", 
                           multiple = TRUE,
                           accept = c(".ab1")),
                 checkboxInput('example', tags$span(style="color: red", 'Load Example Data'), FALSE),
                 actionButton("submit", "Submit"),
                 # div(class = "loader", id = "loading", style = "visibility: hidden;")
                 # div(class = "loader", id = "loading", "Now start vilidate，please wait a moment...")
                 # tabPanel("Statistics", includeHTML("Statistics.html")),

               ),
               mainPanel(
                 
                 tabsetPanel(
                   id = "mainTabset", 
                   tabPanel("Help", includeHTML("Validation2.html")),
                   
                   tabPanel("Results", 
                            uiOutput("dynamicTable"),
                            tags$br(style="clear:both"),
                            plotOutput("fig_text"),
                            tags$br(style="clear:both"),
                            uiOutput("dynamicUI")
                            )
                  
                 )
                 
              
               )

             )
    )
  ),

))