library(shiny)

library(pheatmap)
library(stringdist)
library(stringr)
library("Rgraphviz")
library(stringi)
library(qualV) #For LCS

shinyServer(function(input, output) {
  # A temp file to save the pdf
  outfile <- tempfile(pattern="distance_graph_", tmpdir="www/pdfs", fileext='.pdf')
  
  ## This function reads the input file and returns the matrix of distances
  create_distance_matrix  <- function(inFile){
    if (is.null(inFile)) return(NULL)
    
    start.time  <- Sys.time()
    data <- as.matrix(read.csv(inFile$datapath,sep=input$sep,header=input$header,encoding="GBK",stringsAsFactors=FALSE))
    
    textPreprocessor  <- function (x){
      x  <- tolower(x)
      x  <- apply(x, 2,function(x){stri_trans_general(x,"Latin-ASCII")})
      x  <- apply(x, 2,function(x){str_replace_all(x, "[[:punct:]\"-<>\n\t\r]" , "")})
      return(x)
    }
    
    data <- textPreprocessor(data)
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    
    standardDist <- function(x,y){
      distances <- stringdist(x,y)/pmax(nchar(x),nchar(y))
      return(sum(distances))
    }
    range1to10 <- function(x){1+((x-min(x))*9/(max(x)-min(x)))}
    #range0to1  <- function(x){ 1-  (x-min(x)) / (max(x)- min(x))}
        
    ## Takes two languages and returns the length of the longest common subsequence
    LLCS <- function(x,y){
      ## Join the lines into one big string
      x  <- paste(x, collapse = '')
      y  <- paste(y, collapse = '')
      if(x==y) return(0)
      ##2: Remove spaces, split strings into a vector of chars.
      x  <- strsplit(str_replace_all(x, " ", ""), "")
      y  <- strsplit(str_replace_all(y, " ", ""), "")
      
      return(LCS(unlist(x),unlist(y))$LLCS)
    }
    
    combinedDistsMatrix <- function(file, lv_ratio, lcs_ratio){   
      ### Applies given function to all pairs in argument "file"; returns a matrix
      ## where the upper triangle containts the results
      applyToMatrix <- function(file, fun){
        M  <- matrix(0, nrow = length(file), ncol = length(file))
        for (i in 1:(nrow(M)-1)){
          for (j in (i+1):nrow(M)){
            M[i,j]  <- fun(file[[i]],file[[j]])
          }
        }
        return(M)
      }
      
      LV_mat  <- LCS_mat  <- 0
      if(lv_ratio > 0){
        LV_mat  <- applyToMatrix(file, standardDist)
        LV_mat[upper.tri(LV_mat)] <- range1to10(LV_mat[upper.tri(LV_mat)]) # Higher scores are better
      }
      if(lcs_ratio > 0){
        LCS_mat  <- applyToMatrix(file, LLCS)
        LCS_mat[upper.tri(LCS_mat)]  <-  11 - range1to10(LCS_mat[upper.tri(LCS_mat)]) #The lower, the better for LV
      }
      ## Combine the distance measures:
      M  <- ( lv_ratio * LV_mat + lcs_ratio * LCS_mat)
      ## reflect the upper triangle: obtain a symmetric matrix
      M  <- t(M) + M
      dimnames(M)  <- list(colnames(data),colnames(data))
      return(M)
    }

    ##Construct a language-language matrix    
    distance_mode <- reactive({return(input$dist) })
    if (distance_mode() =="lv"){M  <- combinedDistsMatrix(data,1,0); print("LV")}
    else if (distance_mode() =="comb"){M  <- combinedDistsMatrix(data,0.4,0.6); print("COMBO")}
    else if (distance_mode() =="lcs"){M  <- combinedDistsMatrix(data,0,1); print("LCS")}
    
    end.time <- Sys.time();
    time.taken <- end.time - start.time
    print(time.taken)
    return(M)
  }
  
  # Show the values using an HTML table
  #output$executiontime <- renderTable({
   # sliderValues()
  #})
  
  ##Define the distance matrix as reactive, such that whenever the input changes,
  ## Any plots, etc that use the matrix will also be updated.
  distMatrix  <- reactive(
    create_distance_matrix(input$file1)  
)
  
  
  output$fileUploaded <- reactive({
    return(!is.null(input$file1))
  })
  output$showHelp  <- reactive({return(is.null(input$file1))})
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  ### DATASET TABLE ########
  output$data <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep)
  })
  
  ### GRAPH OF DISTANCE ###########
  output$distanceGraph <- renderPlot({
    withProgress(message = 'PROCESSING...', value = 0.1, {
      incProgress(0.1, detail = "Calculating lexical distances")
      M  <- distMatrix()
      incProgress(0.5, detail = "Drawing graph of distances")
      dists  <- unlist(as.list(M))
      
      f_  <- function(x){      abs(255 - (255* log10(x)))    }
      range05to5 <- function(x){0.5 + ((x-min(x))*4.5/(max(x)-min(x)))}
      g1 <- graphAM(adjMat=M,values = list(weight=dists))
      g1 <- layoutGraph(g1)
      
      
      ## Try defining some EDGE attributes
      ## Get the edge weights
      ew <-  unlist(edgeWeights(g1))
      
      ## Some need removing for some reason somehow...
      ew <-  ew[setdiff(seq(along = ew), removedEdges(g1))]
      ## Label the attributes vector (so it can be used properly)
      names(ew) <-  edgeNames(g1)
      alphacolor = str_c("#000000",sprintf("%.2x",round(f_(ew),0)))
      names(alphacolor) <- names(ew)
      penwidth=5.5- range05to5(ew)
      
      eAttrs <- list(len = ew, label = round(ew,2), fontcolor = alphacolor, color=alphacolor, weight=alphacolor, lwd=penwidth)
      incProgress(0.2, detail="Rendering Graph")
      
      plot(g1, "neato", edgeAttrs=eAttrs)
      
      pdf(outfile)
      plot(g1, "neato", edgeAttrs=eAttrs)
      dev.off()

    })
  }, height = 860, width = 860)
  
  
  #### The Distance Matrix Heatmap ##########
  output$distanceMatrix <- renderPlot({
    withProgress(message = 'PROCESSING...', value = 0.1, {
      incProgress(0.1, detail = "Calculating lexical distances")
      M  <- distMatrix()
      
      incProgress(0.6, detail = "Drawing heatmap")
      pheatmap(M,treeheight_row = 0, treeheight_col = 0,clustering_method = "single" )
    })
   
  }, height = 500, width = 600 )
  
  
  #### The Dendogram ##########
  #incProgress(0.1, detail = "Rendering Distance Tree")
  output$distanceTree <- renderPlot({
    withProgress(message = 'PROCESSING...', value = 0.1, {
      incProgress(0.1, detail = "Calculating lexical distances")
      M  <- distMatrix()
      
      incProgress(0.6, detail = "Drawing dendogram")
      plot(hclust(dist(M),method = "single"), main = "",xlab="",sub="",axes = F, ylab = "")
    })
  }, height = 500, width = 700)
  
  
  output$downloadPDF <- downloadHandler(    
    filename = function() { 
      paste(input$file1, '.pdf', sep='') 
    },
    content <- function(file) {
      file.copy(outfile, file)
      file.remove(outfile)
    }
  )
  
  
  
})