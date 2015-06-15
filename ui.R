library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("Upload a dataset"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ';'),
      tags$hr(),
      p('No language dataset at hand? You can use the following datasets,',
        'First, save files to computer and then upload',
        a(href = 'FinnoUgric.csv', 'Finno Ugric Dataset'), 'or',
        a(href = 'GenesisSet.csv', 'Genesis Dataset'),'.','Note: Larger files require more time when calculating distances.'
      ),
      tags$hr(),
      tags$hr(),
      
      # Decimal interval with step value
      radioButtons("dist", label = h3("Distance measures used:"),
                   choices = list("Levenshtein only" = "lv",
                     "40% Levenshtein, 60% LCS (Warning, takes minutes on larger datasets)" = "comb",
                     "Longest Common Subsequence only (Slow)" = "lcs"),selected = "lv")
    ),
    
    
    mainPanel(
      
      ## INTRODUCTORY MESSAGE PANEL. HIDDEN WHEN A FILE IS UPLOADED!
      conditionalPanel("!output.fileUploaded",
                       p("Welcome to the Language Distance Analyzr web application. This application was developed within the framework of the data-mining course (MTAT.03.183) at the University of Tartu. This tool offers a simple-to-use interface for students, researches and interested persons to concurrently analyze and visualize the lexical similarity of multiple entities such as languages. "),
						p("To get started, please upload a suitable dataset using the form on the left. "),
						p("For more information on the project including algorithm and source code check out the "), a("Github repository.", href="https://github.com/jaks6/LangDist")
						 
                       
                       ),
      
      ## ANALYSIS PANEL. ONLY VISIBLE ONCE A FILE HAS BEEN UPLOADED
      conditionalPanel("output.fileUploaded",
                       
        tabsetPanel(type = "tabs", 
                    tabPanel("Data", tableOutput("data"),align="center"), 
                    tabPanel("Distance Graph", 
                             #downloadButton('downloadPDF', 'Download PDF version (higher resolution)'),
                             plotOutput("distanceGraph")
                             ), 
                    tabPanel("Distance Matrix", plotOutput("distanceMatrix"),align="center"),
                    tabPanel("Distance Tree", plotOutput("distanceTree"),align="center")
        )
      )
      
    )
  )
))