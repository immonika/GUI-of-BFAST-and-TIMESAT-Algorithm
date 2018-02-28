# GUI-of-BFAST-and-TIMESAT-Algorithm
This code is for development of Graphical User Interface of BFAST and TIMESAT Algorithm . These algorithms are implemented in R
#installing of packages
# install.packages("shiny")
# install.packages("png")
# install.packages("sp")
# install.packages("raster")
# install.packages("zoo")
# install.packages("rgdal")
# install.packages("tiff")
# install.packages("date")
# install.packages("timeSeries")
# install.packages("tseries")
# install.packages("foreach")
# install.packages("bfast")
# install.packages("base")


#libraries used
library("shiny")
library("png")
library("sp")
library("raster")
library("zoo")
library("rgdal")
library("tiff")
library("date")
library("timeSeries")
library("tseries")
library("foreach")
library("bfast")
library("base")

options(shiny.maxRequestSize = 30*1024^2)

#user interface
ui <- fluidPage(
  
  sidebarPanel(
    # Input: Select a file ----
    splitLayout(
      textInput("path1", "TIFF File:"),
      textInput("path2", "Date File:"))
    ,
    actionButton("browse1", "Browse Files"),
    
    checkboxInput("plotGraph", label = "Plot Graph from X & Y", value = FALSE),
    
    splitLayout(
      
      numericInput("X1", "X(col_start)", value = 1),
      
      numericInput("Y1", "Y(row_start)", value = 1)
    ),
    
    splitLayout(
      numericInput("X2", "X Max(col_end)", value = 1),
      
      numericInput("Y2", "Y Max(row_end)", value = 1)
    ),
    
    
    
    splitLayout(
      verbatimTextOutput("hover_info"),
      
      verbatimTextOutput("click_info")
    ),
    
    
    
    selectInput("select", "Select Graph", 
                choices = list("All Trends" = 1, "Seasonal Trend" = 2, "Time Trend" = 3 , "Start/End of Season"=4 , "Minima /Maxima"=5), 
                selected = 1),
    
    selectInput("method", "Method for Start/End of Season", 
                choices = list("Seasonal Amplitude" = 1), 
                selected = 1),
    
    splitLayout(
      numericInput("max_iter", "Max Iterations", value = 4),
      
      numericInput("breaks", "Breakpoints", value = 3)
    ),
    
    
    numericInput("h", "Value of h", min = 0,step = 0.1, value = 0.1),
    
    
    
    splitLayout(
      numericInput("threshold", "Threshold(SOS)",min = 0,step = 0.01, value = 0.25),
      
      numericInput("threshold1", "Threshold(EOS)",min = 0,step = 0.01, value = 0.25)
    ),
    
    
    
    tags$b("Save BFAST Components"),
    
    splitLayout(
      downloadButton("downloadData", "Seasonal"),
      
      downloadButton("downloadData1", "Trend")
    ),
    br(),
    tags$b("Save Start/End of Season"),
    
    #splitLayout(
      downloadButton("downloadData2", "Start")
      #downloadButton("downloadData3", "End")
    
    
  ),
  column(width = 8),
  column(width = 8,
         
         imageOutput("image1",width = "100%", height = "100%",
                     
                     click = "image_click",
                     hover = hoverOpts(
                       id = "image_hover"
                     )
         ),
         #if(select == "1"){
         plotOutput("plot1", width = "100%", height = "450px", click = NULL,
                    dblclick = NULL, hover = NULL, hoverDelay = NULL,
                    hoverDelayType = NULL, brush = NULL, clickId = NULL, hoverId = NULL,
                    inline = FALSE)
         
  )
  
  
)



##Server Code

server <- function(input, output, session) {
  
  observe({
    
    if (input$browse1 == 0) return()
    
    updateTextInput(session, "path1",  value = file.choose())
    updateTextInput(session, "path2",  value = file.choose())
  })
  
  # Generate an image with black lines every 10 pixels
  # output$image1 <- renderImage({
  #   filename <- normalizePath(file.path(input$path1))
  #   
  #   # Return a list containing the filename and alt text
  #   list(src = filename)
  #   
  # }, deleteFile = FALSE)
  #   
  output$image1 <- renderImage({
    # Get width and height of image output
    f<- stack(input$path1)
    modisbrick<-brick(f)

    par(col.axis = "white", col.lab = "white", tck = 0)
    plotRGB(modisbrick,
            r = 4, g = 3, b = 2,
            stretch = "lin",
            axes = TRUE)


    #define output path
    width  <- nrow(modisbrick)
    height <- ncol(modisbrick)
    npixels <- width * height
    # Fill the pixels for R, G, B
    m <- matrix(1, nrow = height, ncol = width)
    # Add gray vertical and horizontal lines every 10 pixels
    m[seq_len(ceiling(height/10)) * 10 - 9, ] <- 0.75
    m[, seq_len(ceiling(width/10)) * 10 - 9]  <- 0.75

    # Convert the vector to an array with 3 planes
    img <- array(c(m, m, m), dim = c(height, width, 3))

    # Write it to a temporary file
    outfile <- tempfile(fileext = ".png")
    writePNG(img, target = outfile)

    # Return a list containing information about the image
    list(
      src = outfile,
      contentType = "image/png",
      width = width,
      height = height,
      alt = "This is alternate text"
    )
  })


  output$hover_info <- renderPrint({
    cat("Current \nLocation: ")
    cat("\nX: ")
    cat(round(input$image_hover$x))
    cat("  Y: ")
    cat(round(input$image_hover$y))
    
  })
  
  output$click_info <- renderPrint({
    cat("Location: ")
    cat("\nX: ")
    cat(round(input$image_click$x))
    cat("  Y: ")
    cat(round(input$image_click$y))
    
  })
  
  get_Array <- reactive({
    #
    #Create a vector so as to take date values from text file
    
    tfile1 <- input$path1
    tfile2 <- input$path2
    
    
    line=readLines(file(tfile2))
    
    arr=vector(mode = "logical", length(line))
    
    for (i in 1:length(line))
    {
      arr[i]<-line[i]
    }
    year1 <- as.numeric(substring(arr[1],1,4))
    year2 <- as.numeric(substring(arr[length(arr)],1,4))
    len <- length(arr)
    #Create a vector so as to insert the dates through Date format
    n<-length(line)
    darr<-vector(mode="logical",n+1)
    x=0
    while(x<n+1)
    {
      x<-x+1;
      y<-arr[x]
      darr[x]<-as.Date(y);
    }
    #generate a time series object
    yday365 <- function(x) {
      x <- as.POSIXlt(x,origin=arr[1],format="%Y-%m-%d")
      mdays <- c(0L,31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 
                 31L, 30L, 31L)
      cumsum(c(mdays))[1L + x$mon] + x$mday
    }
    
    
    #Import Modis Layerstack Data & convert in brick format
    f<- stack(tfile1)
    modisbrick<-brick(f)
    
    
    #define output path
    rowno<-nrow(modisbrick)
    colno<-ncol(modisbrick)
    
    
    
    i<-input$image_click$x
    j<-input$image_click$y
    
    if(is.null(i))
    {i<-1}
    
    if(is.null(j))
    {j<-1}
    
    # if(input$plotGraph == TRUE)
    # {
    #   i<-input$X1
    #   j<-input$Y1
    # }
    
    cell<-modisbrick[(i-1)*colno+j]
    
    data<- as.vector(modisbrick[(i-1)*colno+j])
    ################################Dates Generation##############################
    #Select file containing dates
    zz <- zoo(data, 1900 + as.POSIXlt(arr)$year + (yday365(arr) - 1)/365, frequency = 365)
    tso <- as.ts(zz)
    newtso<-na.remove(tso)
    ###########################BFAST Processing###################################
    a<-bfast(newtso,h=input$h,season=c("harmonic"), max.iter = input$max_iter,breaks=input$breaks, hpc ="foreach",level=2,type="OLS-MOSUM")
    
    a
  })
  
  get_Array1 <- reactive({
    #
    #Create a vector so as to take date values from text file
    
    tfile1 <- input$path1
    tfile2 <- input$path2
    
    
    line=readLines(file(tfile2))
    
    arr=vector(mode = "logical", length(line))
    
    for (i in 1:length(line))
    {
      arr[i]<-line[i]
    }
    year1 <- as.numeric(substring(arr[1],1,4))
    year2 <- as.numeric(substring(arr[length(arr)],1,4))
    len <- length(arr)
    #Create a vector so as to insert the dates through Date format
    n<-length(line)
    darr<-vector(mode="logical",n+1)
    x=0
    while(x<n+1)
    {
      x<-x+1;
      y<-arr[x]
      darr[x]<-as.Date(y);
    }
    #generate a time series object
    yday365 <- function(x) {
      x <- as.POSIXlt(x,origin=arr[1],format="%Y-%m-%d")
      mdays <- c(0L,31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 
                 31L, 30L, 31L)
      cumsum(c(mdays))[1L + x$mon] + x$mday
    }
    
    
    #Import Modis Layerstack Data & convert in brick format
    f<- stack(tfile1)
    modisbrick<-brick(f)
    
    
    #define output path
    rowno<-nrow(modisbrick)
    colno<-ncol(modisbrick)
    
    i<-input$X1
    j<-input$Y1
    
    cell<-modisbrick[(i-1)*colno+j]
    
    data<- as.vector(modisbrick[(i-1)*colno+j])
    ################################Dates Generation##############################
    #Select file containing dates
    zz <- zoo(data, 1900 + as.POSIXlt(arr)$year + (yday365(arr) - 1)/365, frequency = 365)
    tso <- as.ts(zz)
    newtso<-na.remove(tso)
    ###########################BFAST Processing###################################
    a<-bfast(newtso,h=input$h,season=c("harmonic"), max.iter = input$max_iter,breaks=input$breaks, hpc ="foreach",level=2,type="OLS-MOSUM")
    
    a
  })
  
  
  
  output$plot1 <- renderPlot({
    if(input$plotGraph == FALSE){
      a <- get_Array()
      temp1 = ""
      temp2 = ""
      if(is.null(input$image_click$x))
      {temp1 = paste("Row :",1,sep = " ")}
      else
      {
        temp1 = paste("Row :",round(input$image_click$x),sep = " ")
      }
      
      if(is.null(input$image_click$y))
      {temp2 = paste("Column :",1,sep = " ")}
      else
      {
        temp2 = paste("Column :",round(input$image_click$y),sep = " ")
      }
      tem = paste(temp1,temp2,sep = " , ")
      if(input$select == 1)
      {
        plot(a,ANOVA=TRUE,sub=tem)
      }
      else if(input$select == 2)
      {
        plot(a[["output"]][[length(a[["output"]])]][["St"]],ylab = "St",main=tem)
      }
      else if(input$select == 3)
      {
        plot(a[["output"]][[length(a[["output"]])]][["Tt"]], ylab="Tt",main=tem)
      }
      else if(input$select == 4)
      {
        
        ##Start/End of Season
        plot(a[["output"]][[length(a[["output"]])]][["St"]],ylab = "St",main=tem)
        season<-as.vector(a[["output"]][[length(a[["output"]])]][["St"]])
        
        
        line=readLines(file(input$path2))
        
        arr=vector(mode = "logical", length(line))
        
        for (i in 1:length(line))
        {
          arr[i]<-line[i]
        }
        year1 <- as.numeric(substring(arr[1],1,4))
        year2 <- as.numeric(substring(arr[length(arr)],1,4))
        len <- length(season)
        
        
        xaxis<-seq(year1,year2,(year2-year1+1)/len)
        xl<-seq(1:len)
        infl <- c(FALSE, diff(diff(season)>0)!=0)
        
        ##TIMESAT
        
        extreme<-season[infl]
        extremepos<-xl[infl]
        n<-length(extreme)
        
        if(extreme[1]<extreme[2]){
          start<-0 #min
          n1<-1
          if(extreme[n-1]>extreme[n]){
            end<-0 #min
            n2<-n-2
          }else{
            end<-1 #max
            n2<-n-3
          }
        }else{
          start<-1  #max
          n1<-2
          if(extreme[n-1]>extreme[n]){
            end<-0  #min
            n2<-n-2
          }else{
            end<-1 #max
            n2<-n-3
          }
        }
        
        if(start==0&&end==0)
        {
          num_min<-ceiling(n/2)
          num_max<-floor(n/2)
        }
        if(start==1&&end==1)
        {
          num_max<-ceiling(n/2)
          num_min<-floor(n/2)
        }else{
          num_min<-n/2
          num_max<-n/2
        }
        
        sos<-array(0,dim=c(floor(n/2)))
        eos<-array(0,dim=c(floor(n/2)))
        
        meanpeak<-0
        meanbase<-0
        #meanpeak and meanbase 
        if(start==0){
          for(i in 2:length(extreme)){
            if(i%%2!=0)
            {
              meanbase=meanbase+extreme[i]
            }else{
              meanpeak=meanpeak+extreme[i]
            }}
        }else{ 
          for(i in 1:length(extreme)){
            if(i%%2!=0)
            {
              
              meanpeak=meanpeak+extreme[i]
            }else{
              meanbase=meanbase+extreme[i]
            }}}
        
        ##numofmax=length(extreme)/2
        meanpeak=meanpeak/num_max
        meanbase=meanbase/num_min
        k<-1
        #i<-n1+2
        startmethod<-input$method
        startcutoff<- input$threshold
        startcutoff1<- input$threshold1
        
        for(i in seq(n1, n2, 2))
        {
          minvalleft<-extreme[i]
          minvalright<-extreme[i+2]
          maxvalcenter<-extreme[i+1]
          amplitude = maxvalcenter - minvalleft
          
          
          if (startmethod == 1){          #%Annual amplitude
            valleft = minvalleft + startcutoff*amplitude;
          }
          
          
          else if (startmethod == 2){        #%Relative amplitude
            valleft = meanbase + startcutoff*(meanpeak - meanbase)
          }
          
          minposleft<-extremepos[i]
          minposright<-extremepos[i+2]
          maxposcenter<-extremepos[i+1]
          
          posleft = 0;
          for(j in minposleft:maxposcenter){
            if (season[j] > valleft)
            { 
              if(season[j-1] < valleft)
              {
                
                posleft = j-1 + (valleft - season[j-1])/(season[j] - season[j-1]) 
                break
              }else
              {posleft<- -1e30;
              break
              }
            }
          }
          
          amplitude = maxvalcenter - minvalright;
          
          if (startmethod == 1){ 
            valright = minvalright + startcutoff1*amplitude
          }
          
          else if (startmethod == 2) { 
            valright = meanbase + startcutoff1*(meanpeak - meanbase)
          }
          
          
          posright = 0;
          
          for (j in seq(minposright,maxposcenter,-1)){
            
            if (season[j] > valright && season[j+1] < valright)
            {
              posright = j + (season[j]-valright)/(season[j] - season[j+1]);
              break
            }
            
          }
          sos[k]<-posleft
          eos[k]<-posright
          k<-k+1
          
        }
        
        points(xaxis[sos], season[sos], col="blue")
        points(xaxis[eos], season[eos], col="red")
        
        
      }
      else if(input$select == 5){
        
        plot(a[["output"]][[length(a[["output"]])]][["St"]],ylab = "St",main=tem)
        season<-as.vector(a[["output"]][[length(a[["output"]])]][["St"]])
        
        # year1 <- as.numeric(substring(arr[1],1,4))
        # year2 <- as.numeric(substring(arr[length(arr)],1,4))
        len <- length(season)
        
        xaxis<-seq(2001,2014,(14)/len)
        xl<-seq(1:len)
        infl <- c(FALSE, diff(diff(season)>0)!=0)
        points(xaxis[infl ], season[infl ], col="blue")
      }}else{
        a <- get_Array1()
        temp1 = ""
        temp2 = ""
        #if(input$plotGraph == TRUE){
        temp1 = paste("Row :",input$X1,sep = " ")
        temp2 = paste("Column :",input$Y1,sep = " ")
        
        # }else{
        # if(is.null(input$image_click$x))
        # {temp1 = paste("Row :",1,sep = " ")}
        # else
        # {
        #   temp1 = paste("Row :",round(input$image_click$x),sep = " ")
        # }
        # 
        # if(is.null(input$image_click$y))
        # {temp2 = paste("Column :",1,sep = " ")}
        # else
        # {
        #   temp2 = paste("Column :",round(input$image_click$y),sep = " ")
        # }
        
        
        tem = paste(temp1,temp2,sep = " , ")
        if(input$select == 1)
        {
          plot(a,ANOVA=TRUE,sub=tem)
        }
        else if(input$select == 2)
        {
          plot(a[["output"]][[length(a[["output"]])]][["St"]],ylab = "St",main=tem)
        }
        else if(input$select == 3)
        {
          plot(a[["output"]][[length(a[["output"]])]][["Tt"]], ylab="Tt",main=tem)
        }
        else if(input$select == 4)
        {
          
          ##Start/End of Season
          plot(a[["output"]][[length(a[["output"]])]][["St"]],ylab = "St",main=tem)
          season<-as.vector(a[["output"]][[length(a[["output"]])]][["St"]])
          
          
          line=readLines(file(input$path2))
          
          arr=vector(mode = "logical", length(line))
          
          for (i in 1:length(line))
          {
            arr[i]<-line[i]
          }
          year1 <- as.numeric(substring(arr[1],1,4))
          year2 <- as.numeric(substring(arr[length(arr)],1,4))
          len <- length(season)
          
          
          xaxis<-seq(year1,year2,(year2-year1+1)/len)
          xl<-seq(1:len)
          infl <- c(FALSE, diff(diff(season)>0)!=0)
          
          ##TIMESAT
          
          extreme<-season[infl]
          extremepos<-xl[infl]
          n<-length(extreme)
          
          if(extreme[1]<extreme[2]){
            start<-0 #min
            n1<-1
            if(extreme[n-1]>extreme[n]){
              end<-0 #min
              n2<-n-2
            }else{
              end<-1 #max
              n2<-n-3
            }
          }else{
            start<-1  #max
            n1<-2
            if(extreme[n-1]>extreme[n]){
              end<-0  #min
              n2<-n-2
            }else{
              end<-1 #max
              n2<-n-3
            }
          }
          
          # if(start==0&&end==0)
          # {
          #   num_min<-ceiling(n/2)
          #   num_max<-floor(n/2)
          # }
          # if(start==1&&end==1)
          # {
          #   num_max<-ceiling(n/2)
          #   num_min<-floor(n/2)
          # }else{
          #   num_min<-n/2
          #   num_max<-n/2
          # }
          
          sos<-array(0,dim=c(floor(n/2)))
          eos<-array(0,dim=c(floor(n/2)))
          
          
          k<-1
          #i<-n1+2
          # startmethod<-1
          startcutoff<- input$threshold
          startcutoff1<- input$threshold1
          
          for(i in seq(n1, n2, 2))
          {
            minvalleft<-extreme[i]
            minvalright<-extreme[i+2]
            maxvalcenter<-extreme[i+1]
            amplitude = maxvalcenter - minvalleft
            
            
            #if (startmethod == 1){          #%Annual amplitude
            valleft = minvalleft + startcutoff*amplitude;
            #}
            
            
            # else if (startmethod == 2){        #%Relative amplitude
            #   valleft = meanbase + startcutoff*(meanpeak - meanbase)
            # }
            
            minposleft<-extremepos[i]
            minposright<-extremepos[i+2]
            maxposcenter<-extremepos[i+1]
            
            posleft = 0;
            for(j in minposleft:maxposcenter){
              if (season[j] > valleft)
              { 
                if(season[j-1] < valleft)
                {
                  
                  posleft = j-1 + (valleft - season[j-1])/(season[j] - season[j-1]) 
                  break
                }else
                {posleft<- -1e30;
                break
                }
              }
            }
            
            amplitude = maxvalcenter - minvalright;
            
            #if (startmethod == 1){ 
            valright = minvalright + startcutoff1*amplitude
            #}
            # 
            # else if (startmethod == 2) { 
            #   valright = meanbase + startcutoff1*(meanpeak - meanbase)
            # }
            
            
            posright = 0;
            
            for (j in seq(minposright,maxposcenter,-1)){
              
              if (season[j] > valright && season[j+1] < valright)
              {
                posright = j + (season[j]-valright)/(season[j] - season[j+1]);
                break
              }
              
            }
            sos[k]<-posleft
            eos[k]<-posright
            k<-k+1
            
          }
          
          points(xaxis[sos], season[sos], col="blue")
          points(xaxis[eos], season[eos], col="red")
        }
        else if(input$select == 5){
          
          plot(a[["output"]][[length(a[["output"]])]][["St"]],ylab = "St",main=tem)
          season<-as.vector(a[["output"]][[length(a[["output"]])]][["St"]])
          
          year1 <- 2001 #as.numeric(substring(arr[1],1,4))
          year2 <- 2014 #as.numeric(substring(arr[length(arr)],1,4))
          len <- length(season)
          
          xaxis<-seq(year1,year2,(year2-year1+1)/len)
          xl<-seq(1:len)
          infl <- c(FALSE, diff(diff(season)>0)!=0)
          points(xaxis[infl ], season[infl ], col="blue")
        }
      }
    
  })
  
  ##create 3D array for Seasonal component
  
  
  ##saving the seasonal component in CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("seasonal", ".csv", sep = "")
    },
    content = function(file1) {
      
     # dataInput<-reactive({
        
        line=readLines(file(input$path2))
        
        arr=vector(mode="logical",length(line))
        
        for (i in 1:length(line))
        {
          arr[i]<-line[i]
        }
        #Create a vector so as to insert the dates through Date format
        n<-length(line)
        darr<-vector(mode="logical",n+1)
        x=0
        while(x<n+1)
        {
          x<-x+1;
          y<-arr[x]
          darr[x]<-as.Date(y);
        }
        #generate a time series object
        yday365 <- function(x) {
          x <- as.POSIXlt(x,origin=arr[1],format="%Y-%m-%d")
          mdays <- c(0L,31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 
                     31L, 30L, 31L)
          cumsum(c(mdays))[1L + x$mon] + x$mday
          
        }
        
        f<- stack(input$path1)
        modisbrick<-brick(f)
        
        rowno<-nrow(modisbrick)
        colno<-ncol(modisbrick)
        
        if(input$X2>=input$X1)
        {
          iMin <- input$X1
          iMax <- input$X2
        }
        else
        {
          iMin <- 1
          iMax <- 1
        }
        if(input$Y2>=input$Y1)
        {
          jMin <- input$Y1
          jMax <- input$Y2
        }
        else
        {
          jMin <- 1
          jMax <- 1
        }
        
        write.csv(NA,file1,row.names = FALSE)
        #seasonarr<-array(0,dim=c(iMax,jMax,length(line)))
        for(i in iMin:iMax)
        {
          for(j in jMin:jMax)
          {
            cell<-modisbrick[(i-1)*jMax+j]
            
            data<- as.vector(modisbrick[(i-1)*jMax+j])
            ################################Dates Generation##############################
            #Select file containing dates
            zz <- zoo(data, 1900 + as.POSIXlt(arr)$year + (yday365(arr) - 1)/365, frequency = 365)
            tso <- as.ts(zz)
            newtso<-na.remove(tso)
            ###########################BFAST Processing###################################
            a<-bfast(newtso,h=input$h,season=c("harmonic"), max.iter = input$max_iter,breaks=input$breaks, hpc ="foreach",level=2,type="OLS-MOSUM")
            season<-as.vector(a[["output"]][[length(a[["output"]])]][["St"]])
            cat(i)
            cat(j)
            cat(" ")
            cat(season)
            cat("                                                             ")
            tmp<-read.csv(file1)
            tmp<-cbind(tmp,season)
            write.csv(tmp, file1, row.names = FALSE)
            
            
            
          }
        }
        
        #seasonarr
        
      #})
      #write.csv(dataInput(), file, row.names = FALSE)
      showModal(modalDialog(
        title = "Important message",
        "File containing the Seasonal Data has been successfully saved!",
        easyClose = TRUE
      ))
    }
  )
  
  ##create 3D array for Seasonal component
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("trend", ".csv", sep = "")
    },
    content = function(file2) {
      
      # dataInput<-reactive({
      
      line=readLines(file(input$path2))
      
      arr=vector(mode="logical",length(line))
      
      for (i in 1:length(line))
      {
        arr[i]<-line[i]
      }
      #Create a vector so as to insert the dates through Date format
      n<-length(line)
      darr<-vector(mode="logical",n+1)
      x=0
      while(x<n+1)
      {
        x<-x+1;
        y<-arr[x]
        darr[x]<-as.Date(y);
      }
      #generate a time series object
      yday365 <- function(x) {
        x <- as.POSIXlt(x,origin=arr[1],format="%Y-%m-%d")
        mdays <- c(0L,31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 
                   31L, 30L, 31L)
        cumsum(c(mdays))[1L + x$mon] + x$mday
        
      }
      
      f<- stack(input$path1)
      modisbrick<-brick(f)
      
      rowno<-nrow(modisbrick)
      colno<-ncol(modisbrick)
      
      if(input$X2>=input$X1)
      {
        iMin <- input$X1
        iMax <- input$X2
      }
      else
      {
        iMin <- 1
        iMax <- 1
      }
      if(input$Y2>=input$Y1)
      {
        jMin <- input$Y1
        jMax <- input$Y2
      }
      else
      {
        jMin <- 1
        jMax <- 1
      }
      
      write.csv(NA,file2,row.names = FALSE)
      #seasonarr<-array(0,dim=c(iMax,jMax,length(line)))
      for(i in iMin:iMax)
      {
        for(j in jMin:jMax)
        {
          cell<-modisbrick[(i-1)*jMax+j]
          
          data<- as.vector(modisbrick[(i-1)*jMax+j])
          ################################Dates Generation##############################
          #Select file containing dates
          zz <- zoo(data, 1900 + as.POSIXlt(arr)$year + (yday365(arr) - 1)/365, frequency = 365)
          tso <- as.ts(zz)
          newtso<-na.remove(tso)
          ###########################BFAST Processing###################################
          a<-bfast(newtso,h=input$h,season=c("harmonic"), max.iter = input$max_iter,breaks=input$breaks, hpc ="foreach",level=2,type="OLS-MOSUM")
          trend<-as.vector(a[["output"]][[length(a[["output"]])]][["Tt"]])
          cat(i)
          cat(j)
          cat(" ")
          cat(trend)
          cat("                                                             ")
          tmp<-read.csv(file2)
          tmp<-cbind(tmp,trend)
          write.csv(tmp, file2, row.names = FALSE)
          
          
          
        }
      }
      
      #seasonarr
      
      #})
      #write.csv(dataInput(), file, row.names = FALSE)
      showModal(modalDialog(
        title = "Important message",
        "File containing the Seasonal Data has been successfully saved!",
        easyClose = TRUE
      ))
    }
  )
  
  
  ##saving the seasonal component in CSV file
  
  
  
  
  ##create 3D array for SOS
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("sos", ".csv", sep = "")
    },
    content = function(file3) {
      
      # dataInput<-reactive({
      
      line=readLines(file(input$path2))
      
      arr=vector(mode="logical",length(line))
      
      for (i in 1:length(line))
      {
        arr[i]<-line[i]
      }
      #Create a vector so as to insert the dates through Date format
      n<-length(line)
      darr<-vector(mode="logical",n+1)
      x=0
      while(x<n+1)
      {
        x<-x+1;
        y<-arr[x]
        darr[x]<-as.Date(y);
      }
      #generate a time series object
      yday365 <- function(x) {
        x <- as.POSIXlt(x,origin=arr[1],format="%Y-%m-%d")
        mdays <- c(0L,31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 
                   31L, 30L, 31L)
        cumsum(c(mdays))[1L + x$mon] + x$mday
        
      }
      
      f<- stack(input$path1)
      modisbrick<-brick(f)
      
      rowno<-nrow(modisbrick)
      colno<-ncol(modisbrick)
      
      if(input$X2>=input$X1)
      {
        iMin <- input$X1
        iMax <- input$X2
      }
      else
      {
        iMin <- 1
        iMax <- 1
      }
      if(input$Y2>=input$Y1)
      {
        jMin <- input$Y1
        jMax <- input$Y2
      }
      else
      {
        jMin <- 1
        jMax <- 1
      }
      
      write.csv(NA,file3,row.names = FALSE)
      #seasonarr<-array(0,dim=c(iMax,jMax,length(line)))
      for(i1 in iMin:iMax)
      {
        for(j1 in jMin:jMax)
        {
          cell<-modisbrick[(i1-1)*jMax+j1]
          
          data<- as.vector(modisbrick[(i1-1)*jMax+j1])
          ################################Dates Generation##############################
          #Select file containing dates
          zz <- zoo(data, 1900 + as.POSIXlt(arr)$year + (yday365(arr) - 1)/365, frequency = 365)
          tso <- as.ts(zz)
          newtso<-na.remove(tso)
          ###########################BFAST Processing###################################
          a<-bfast(newtso,h=input$h,season=c("harmonic"), max.iter = input$max_iter,breaks=input$breaks, hpc ="foreach",level=2,type="OLS-MOSUM")
          season<-as.vector(a[["output"]][[length(a[["output"]])]][["St"]])
          
          
          xl<-seq(1:length(season))
          infl <- c(FALSE, diff(diff(season)>0)!=0)
          
          ##TIMESAT
          
          extreme<-season[infl]
          extremepos<-xl[infl]
          n<-length(extreme)
          
          if(extreme[1]<extreme[2]){
            start<-0 #min
            n1<-1
            if(extreme[n-1]>extreme[n]){
              end<-0 #min
              n2<-n-2
            }else{
              end<-1 #max
              n2<-n-3
            }
          }else{
            start<-1  #max
            n1<-2
            if(extreme[n-1]>extreme[n]){
              end<-0  #min
              n2<-n-2
            }else{
              end<-1 #max
              n2<-n-3
            }
          }
          
          if(start==0&&end==0)
          {
            num_min<-ceiling(n/2)
            num_max<-floor(n/2)
          }
          if(start==1&&end==1)
          {
            num_max<-ceiling(n/2)
            num_min<-floor(n/2)
          }else{
            num_min<-n/2
            num_max<-n/2
          }
          
          sos<-array(0,dim=30)
          eos<-array(0,dim=30)
          
          meanpeak<-0
          meanbase<-0
          #meanpeak and meanbase 
          if(start==0){
            for(i in 2:length(extreme)){
              if(i%%2!=0)
              {
                meanbase=meanbase+extreme[i]
              }else{
                meanpeak=meanpeak+extreme[i]
              }}
          }else{ 
            for(i in 1:length(extreme)){
              if(i%%2!=0)
              {
                
                meanpeak=meanpeak+extreme[i]
              }else{
                meanbase=meanbase+extreme[i]
              }}}
          
          meanpeak=meanpeak/num_max
          meanbase=meanbase/num_min
          k<-1
          #i<-n1+2
          for(i in seq(n1, n2, 2))
          {
            minvalleft<-extreme[i]
            minvalright<-extreme[i+2]
            maxvalcenter<-extreme[i+1]
            amplitude = maxvalcenter - minvalleft
            
            startmethod<-input$method
            startcutoff<- input$threshold
            if (startmethod == 1){          #%Annual amplitude
              valleft = minvalleft + startcutoff*amplitude;
            }
            else if (startmethod == 2) #%Absolute value
            {        
              if (minvalleft < startcutoff && startcutoff < maxvalcenter)     
                valleft<-startcutoff
              else
                valleft<--1e30
            }
            
            else if (startmethod == 3){        #%Relative amplitude
              valleft = meanbase + startcutoff*(meanpeak - meanbase)
            }
            
            minposleft<-extremepos[i]
            minposright<-extremepos[i+2]
            maxposcenter<-extremepos[i+1]
            
            posleft = 0;
            for(j in minposleft:maxposcenter){
              if (season[j] > valleft)
              { 
                if(season[j-1] < valleft)
                {
                  
                  posleft = j-1 + (valleft - season[j-1])/(season[j] - season[j-1]) 
                  break
                }else
                {posleft<- -1e30;
                break
                }
              }
            }
            
            amplitude = maxvalcenter - minvalright;
            
            if (startmethod == 1){ 
              valright = minvalright + startcutoff*amplitude
            }
            else if (startmethod == 2) 
            {  
              if( minvalright < startcutoff && startcutoff < maxvalcenter ) 
                valright = startcutoff
              else
                valright = -1e30;   
            }
            
            else if (startmethod == 3) { 
              valright = meanbase + startcutoff*(meanpeak - meanbase)
            }
            
            
            posright = 0;
            
            for (j in seq(minposright,maxposcenter,-1)){
              
              if (season[j] > valright && season[j+1] < valright)
              {
                posright = j + (season[j]-valright)/(season[j] - season[j+1]);
                break
              }
              
            }
            sos[k]<-posleft
            eos[k]<-posright
            k<-k+1
          }
          cat(i1)
          cat(j1)
          cat(" ")
          cat(sos)
          cat("                                                             ")
          tmp<-read.csv(file3)
          tmp<-cbind(tmp,sos)
          tmp<-cbind(tmp,eos)
          write.csv(tmp, file3, row.names = FALSE )
          
          
        }
      }
      #write.csv(dataInput(), file, row.names = FALSE)
      showModal(modalDialog(
        title = "Important message",
        "File containing the Seasonal Data has been successfully saved!",
        easyClose = TRUE
      ))
    }
  )
  ##saving the SOS in CSV file
  
  ##create 3D array for EOS
  
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)
