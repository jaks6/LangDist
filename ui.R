library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("Upload a dataset"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose Dataset CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      #tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator:',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ';'),
      #tags$hr(),
      p('No language dataset at hand? You can use the following datasets,',
        '\nFirst, save files to computer and then and upload. (Note that larger datasets take more time)'),
      
      tags$ul(class="examplelist",
        list(
          tags$li(
            a(href = 'FinnoUgric.csv', target="_blank", 'Finno Ugric Dataset'),
            '(Small Dataset)'
            ),
          tags$li(
            a(href = 'DrStrangeloveSubtitles.csv',  target="_blank",'Dr. Strangelove Subtitles Dataset'),
            '(Medium file, 13 languages, ~250 characters per language)'
            ),
          tags$li(a(href = 'GenesisSet.csv', target="_blank",'Genesis Dataset')
                  , '(Large file, 20 languages, ~4000 characters per language)'
            )
          )),
  
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
                       h4(p("Welcome to the Language Distance Analyzer web application.")),
                      p(tags$em("This application was developed as part of the data-mining course (MTAT.03.183) at the University of Tartu.")),
                        p("This tool offers a simple-to-use interface for students, researches and interested persons to concurrently analyze and visualize the lexical similarity of multiple entities such as languages. "),
						p("To get started, please upload a suitable dataset using the form on the left. "),
						p("For more information on the project including algorithm and source code check out the "), a("Github repository.", href="https://github.com/jaks6/LangDist")
						 
                       
                       ),
      
      ## ANALYSIS PANEL. ONLY VISIBLE ONCE A FILE HAS BEEN UPLOADED
      conditionalPanel("output.fileUploaded",
                       p(textOutput("executiontime")),
                       
        tabsetPanel(type = "tabs", 
                    tabPanel("Data", tableOutput("data"),align="center"), 
                    tabPanel("Distance Graph", 
                             conditionalPanel("!output.distanceGraph",
                                              p("Processing, please wait. When using Longest Common Subsequence(LCS), large files can take a while (even minutes!)")
                                              ),
                             conditionalPanel("output.distanceGraph",
                               downloadButton('downloadPDF', 'Download PDF version (higher resolution)')
                             ),
                             plotOutput("distanceGraph")
                             ), 
                    tabPanel("Distance Matrix", 
                             conditionalPanel("!distanceMatrix",
                                              p("Processing, please wait. When using Longest Common Subsequence(LCS), large files can take a while (even minutes!)")
                                              ),
                             plotOutput("distanceMatrix"),
                             align="center"),
                    tabPanel("Distance Tree", 
                             conditionalPanel("!distanceTree",
                                              p("Processing, please wait. When using Longest Common Subsequence(LCS), large files can take a while (even minutes!)")
                             ),
                             plotOutput("distanceTree")
                             ,align="center")
        )
      )
      
    )
  )
))