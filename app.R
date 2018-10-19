# load packages
library(shiny)
library(shinyalert)
library(gplots)
library(ggfortify)
library(impute)
library(DT)

# user interface function
ui <- fluidPage(
  theme='bootstrap.css',
  useShinyalert(),
  titlePanel('PEAKSviz', windowTitle='PEAKSviz'),
  tabsetPanel(
    tabPanel(
      'Instructions',
      h3("Welcome to PEAKSviz!"),
      br(),
      p(
        'This application performs data visualization and statistical analyss for PEAKS output. Click on the tab at the top of the page to perform a protein or peptide analysis.'
      ),
      br(),
      p(
        'If you pick protein analysis, you will need to upload a proteins.csv file from PEAKS. If you pick peptide analysis, you will need to upload a protein-peptides.csv file from PEAKS.'
      )
    ),
    
    
    
    #tags$head(tags$style(HTML("#mainpanel li a[data-value = 'step2'], #mainpanel li a[data-value = 'step3'], #mainpanel li a[data-value = 'step4'] {
                               #display: none;
                               #}"))),
    tabPanel(
      'Protein statistical analysis',
      navlistPanel(
        'Follow these steps',
        id='mainpanel',
        tabPanel(
          title='Step 1: Load data',
          value='step1',
          fluidRow(
            column(6, strong('Upload PEAKS proteins.csv file'))
          ),
          fluidRow(
            column(6,
              fileInput(
                'file',
                NULL,
                accept=c(
                  'text/csv',
                  'text/comma-separated-values,text/plain',
                  '.csv'
                )
              )
            ),
            column(1,
              actionButton('fHelp', '?')
            )
          ),
          fluidRow(
            column(6, strong('Project name'))
          ),
          fluidRow(
            column(6,
              textInput('pName', NULL, placeholder='e.g. myProject')
            ),
            column(1,
              actionButton('nHelp', '?')
            )
          ),
          fluidRow(
            column(6, strong('Imputation method'))
          ),
          fluidRow(
            column(6,
                   radioButtons(
                     'imp', NULL,
                     c(
                       '"Smart" k-Nearest Neighbors' = 'knn',
                       'Median Imputation' = 'med'
                     )
                   )
            ),
            column(1, br(), actionButton('iHelp', '?'))
          ),
          fluidRow(
            column(6, strong('Number of different groups'))
          ),
          fluidRow(
            column(6,
                   selectInput(
                     'n_groups',
                     NULL,
                     c(0,2:6),
                     selected=0
                   )
            ),
            column(1, actionButton('gHelp', '?'))
          ),
          fluidRow(
            column(6, strong('Proteins of interest (one per line)'))
          ),
          fluidRow(
            column(6,
                   textAreaInput(
                     'protList',
                     NULL,
                     height='300px'
                   )
            ),
            column(1, actionButton('pHelp', '?'))
          )
        ),
        tabPanel(
          title='Step 2: Select columns',
          value='step2',
          uiOutput('colSelect')
        ),
        tabPanel(
          title='Step 3: Group samples',
          value='step3',
          uiOutput('groups')
        ),
        tabPanel(
          title='Step 4: Results',
          value='step4',
          tabsetPanel(
            id='res_panel',
            tabPanel(
              'Scaled data',
              downloadButton('downloadTable', 'Download scaled data'),
              p(),
              DT::dataTableOutput('sdata')
            ),
            tabPanel(
              'Heatmap',
              downloadButton('downloadHeatmap', 'Download heatmap'),
              p(),
              plotOutput('heatmap')
            ),
            tabPanel(
              'PCA',
              tabsetPanel(
                tabPanel(
                  'Plot',
                  downloadButton('downloadPCA', 'Download PCA plot'),
                  p(),
                  plotOutput('pcaPlot')
                ),
                tabPanel(
                  'Contribution to Separation',
                  downloadButton('download_loadings', 'Download contributon to separation'),
                  p(),
                  DT::dataTableOutput('loadings')
                )
              )
            ),
            tabPanel(
              'Clustering',
              downloadButton('downloadClust', 'Download hierarchical clustering dendrogram'),
              p(),
              plotOutput('clust')
            ),
            tabPanel(
              'Differential expression',
              downloadButton('download_diffexp', 'Download differential expression results'),
              p(),
              DT::dataTableOutput('diffexp')
            )
          )
        )
      )
    ),
    
    
    
    tabPanel(
      'Peptide statistical analysis',
      navlistPanel(
        'Follow these steps',
        tabPanel(
          'Step 1: Load data',
          fluidRow(
            column(6, strong('Upload PEAKS protein-peptides.csv file'))
          ),
          fluidRow(
            column(
              6,
              fileInput(
                'pep_file',
                NULL,
                accept=c(
                  'text/csv',
                  'text/comma-separated-values,text/plain',
                  '.csv'
                )
              )
            ),
            column(1, actionButton('pep_fHelp', '?'))
          ),
          fluidRow(
            column(6, strong('Project name'))
          ),
          fluidRow(
            column(
              6,
              textInput('pep_pName', NULL, placeholder='e.g. myProject')
            ),
            column(1, actionButton('pep_nHelp', '?'))
          ),
          fluidRow(
            column(6, strong('Imputation method'))
          ),
          fluidRow(
            column(
              6,
              radioButtons(
                'pep_imp', NULL,
                c(
                  '"Smart" k-Nearest Neighbors' = 'knn',
                  'Median Imputation' = 'med'
                )
              )
            ),
            column(1, br(), actionButton('pep_iHelp', '?'))
          ),
          fluidRow(
            column(6, strong('Number of different groups'))
          ),
          fluidRow(
            column(
              6,
              selectInput(
                'pep_n_groups',
                NULL,
                c(0,2:6),
                selected=0
              )
            ),
            column(1, actionButton('pep_gHelp', '?'))
          ),
          fluidRow(
            column(6, strong('Enter protein'))
          ),
          fluidRow(
            column(
              6,
              selectizeInput(
                'pep_prot',
                NULL,
                choices='',
                selected='',
                multiple=FALSE
              )
            ),
            column(1, actionButton('pep_pHelp', '?'))
          )
        ),
        tabPanel(
          'Step 2: Select columns',
          uiOutput('pep_colSelect')
        ),
        tabPanel(
          'Step 3: Group samples',
          uiOutput('pep_groups')
        ),
        tabPanel(
          'Step 4: Results',
          tabsetPanel(
            id='pep_res_panel',
            tabPanel(
              'Scaled data',
              downloadButton('pep_downloadTable', 'Download scaled peptide data'),
              p(),
              DT::dataTableOutput('pep_sdata')
            ),
            tabPanel(
              'Heatmap',
              downloadButton('pep_downloadHeatmap', 'Download heatmap'),
              p(),
              plotOutput('pep_heatmap')
            ),
            tabPanel(
              'PCA',
              tabsetPanel(
                tabPanel(
                  'Plot',
                  downloadButton('pep_downloadPCA', 'Download PCA plot'),
                  p(),
                  plotOutput('pep_pcaPlot')
                ),
                tabPanel(
                  'Contribution to Separation',
                  downloadButton('pep_download_loadings', 'Download contributon to separation'),
                  p(),
                  DT::dataTableOutput('pep_loadings')
                )
              )
            ),
            tabPanel(
              'Clustering',
              downloadButton('pep_downloadClust', 'Download hierarchical clustering dendrogram'),
              p(),
              plotOutput('pep_clust')
            ),
            tabPanel(
              'Differential expression',
              downloadButton('pep_download_diffexp', 'Download differential expression results'),
              p(),
              DT::dataTableOutput('pep_diffexp')
            )
          )
        )
      )
    )
  )
)







# server function
server <- function(input, output, session) {
  # maximum file size
  options(shiny.maxRequestSize=30*1024^2)
  
  # where to keep the data
  values     <- reactiveValues(orig_data=NULL, scaled_data=NULL, groups=NULL, imp=NULL, prots=NULL)
  pep_values <- reactiveValues(orig_data=NULL, scaled_data=NULL, groups=NULL, imp=NULL, prots=NULL)
  
  # load the data for proteins
  observeEvent(input$file, {
    # read data
    d.f <- read.csv(input$file$datapath, stringsAsFactors=F)
    rownames(d.f) <- d.f$Accession
    
    # set data
    values$orig_data <- d.f
  })
  
  # popup with info about loading the proteins.csv file
  observeEvent(input$fHelp, {
    shinyalert(
      text='Upload your proteins.csv file here for protein-level analysis',
      type='info'
    )
  })
  
  # load the data for peptides
  observeEvent(input$pep_file, {
    # read data
    d.f <- read.csv(input$pep_file$datapath, stringsAsFactors=F)
    rownames(d.f) <- paste(d.f$Peptide, ': ', d.f$Protein.Accession, sep='')
    
    # set data
    pep_values$orig_data <- d.f
    
    # update choice for protein selection
    updateSelectizeInput(
      session,
      'pep_prot',
      choices=c(sort(unique(d.f$Protein.Accession)), ''),
      selected=''
    )
  })
  
  # popup with info about loading the protein-peptides.csv file
  observeEvent(input$pep_fHelp, {
    shinyalert(
      text='Upload your protein-peptides.csv file here for peptide-level analysis',
      type='info'
    )
  })
  
  # popup with info about the project name
  observeEvent(input$nHelp, {
    shinyalert(
      text='Give your project a descriptive name (e.g. Mouse Cancer Study)',
      type='info'
    )
  })
  
  # popup with info about the project name
  observeEvent(input$pep_nHelp, {
    shinyalert(
      text='Give your project a descriptive name (e.g. Mouse Cancer Study)',
      type='info'
    )
  })
  
  # which imputation method for proteins
  observeEvent(input$imp, {
    if (input$imp == 'med') {
      values$imp <- 'med'
    } else {
      values$imp <- 'knn'
    }
  })
  
  # popup with info about imputation
  observeEvent(input$iHelp, {
    shinyalert(
      text='k-Nearest Neighbors imputation infers protein abundance based on similarity among samples\n\nMedian imputation assigns missing points the median expression for each sample',
      type='info'
    )
  })
  
  # which imputation method for peptides
  observeEvent(input$pep_imp, {
    if (input$pep_imp == 'med') {
      pep_values$imp <- 'med'
    } else {
      pep_values$imp <- 'knn'
    }
  })
  
  # popup with info about imputation
  observeEvent(input$pep_iHelp, {
    shinyalert(
      text='k-Nearest Neighbors imputation infers protein abundance based on similarity among samples\n\nMedian imputation assigns missing points the median expression for each sample',
      type='info'
    )
  })
  
  # scale the data for proteins
  observeEvent(input$columns, {
    if(!is.null(input$columns)) {
      d <- as.data.frame(values$orig_data[,input$columns])
      
      # get rid of '-' values
      d[d == '-'] <- 0
      
      # make all columns numeric
      for (i in 1:ncol(d)) d[,i] <- as.numeric(d[,i])
      
      # log
      l <- d
      l[l == 0] <- 1
      l <- log(l, 10)
      l[l == 0] <- NA
      
      # impute
      if (values$imp == 'med') { # median impute
        for (i in 1:ncol(l)) {
          tmp <- l[,i]
          tmp[is.na(tmp)] <- median(tmp, na.rm=T)
          l[,i] <- tmp
        }
        
        # scale
        s <- scale(l)
        attr(s, 'scaled:center') <- NULL
      } else { # "smart" k-nearest neighbors
        k <- floor(sqrt(ncol(l))) - 1
        if (k > 0) { # we can justify using KNN
          qq <- impute.knn(as.matrix(l), k)$data
          
          s <- scale(qq)
          attr(s, 'scaled:center') <- NULL
          attr(s, 'scaled:scale')  <- NULL
        } else { # cannot justify using k-nearest neighbors, so just use median imp
          for (i in 1:ncol(l)) {
            tmp <- l[,i]
            tmp[is.na(tmp)] <- median(tmp, na.rm=T)
            l[,i] <- tmp
          }
          
          # scale
          s <- scale(l)
          attr(s, 'scaled:center') <- NULL
          attr(s, 'scaled:scale')  <- NULL
        }
      }
      
      # subset proteins
      if (input$protList != '') {
        # get proteins from user
        prots <- strsplit(input$protList, '\n')[[1]]
        
        # clean up protein names
        #prots <- sapply(
          #prots, function(x){
            #y <- strsplit(x, '|', fixed=T)[[1]]
            #z <- strsplit(y[length(y)], '_', fixed=T)[[1]][1]
            #return(z)
          #}
        #)
        
        # subset
        s <- s[rownames(s) %in% prots,]
      }
      
      # return scaled data
      values$scaled_data <- s
    }
  })
  
  # scale the data for peptides
  observeEvent(input$pep_columns, {
    if(!is.null(input$pep_columns) && (length(input$pep_columns) > 1)) {
      d <- as.data.frame(pep_values$orig_data[,input$pep_columns])
      
      # get rid of '-' values
      d[d == '-'] <- 0
      
      # make all columns numeric
      for (i in 1:ncol(d)) d[,i] <- as.numeric(d[,i])
      
      # log
      l <- d
      l[l == 0] <- 1
      l <- log(l, 10)
      l[l == 0] <- NA
      
      # impute
      if (pep_values$imp == 'med') { # median impute
        for (i in 1:ncol(l)) {
          tmp <- l[,i]
          tmp[is.na(tmp)] <- median(tmp, na.rm=T)
          l[,i] <- tmp
        }
        
        # scale
        s <- scale(l)
        attr(s, 'scaled:center') <- NULL
        attr(s, 'scaled:scale')  <- NULL
      } else { # "smart" k-nearest neighbors
        k <- floor(sqrt(ncol(l))) - 1
        if (k > 0) { # we can justify using KNN
          qq <- impute.knn(as.matrix(l), k)$data
          
          s <- scale(qq)
          attr(s, 'scaled:center') <- NULL
          attr(s, 'scaled:scale')  <- NULL
        } else { # cannot justify using k-nearest neighbors, so just use median imp
          for (i in 1:ncol(l)) {
            tmp <- l[,i]
            tmp[is.na(tmp)] <- median(tmp, na.rm=T)
            l[,i] <- tmp
          }
          
          # scale
          s <- scale(l)
          attr(s, 'scaled:center') <- NULL
          attr(s, 'scaled:scale')  <- NULL
        }
      }
      
      # add proteins back to scaled data
      prtn <- c()
      for (i in 1:nrow(s)) {
        prtn <- c(prtn, strsplit(rownames(s)[i], ': ', fixed=T)[[1]][2])
      }
      
      s <- cbind.data.frame(prtn, s)
      colnames(s)[1] <- 'Protein.Accession'
      
      # subset proteins
      if (input$pep_prot != '') {
        # get protein from user
        prot <- input$pep_prot
        
        # subset
        s <- s[s[,1] %in% prot,]
      }
      
      # return scaled data
      pep_values$scaled_data <- s
    }
  })
  
  # decide whether to show or hide the differential expression tab for proteins
  observeEvent(input$n_groups, {
    if (input$n_groups == 0) {
      hideTab(inputId='res_panel', target='Differential expression')
    } else {
      showTab(inputId='res_panel', target='Differential expression')
    }
  })
  
  # decide whether to show or hide the differential expression tab for peptides
  observeEvent(input$pep_n_groups, {
    if (input$pep_n_groups == 0) {
      hideTab(inputId='pep_res_panel', target='Differential expression')
    } else {
      showTab(inputId='pep_res_panel', target='Differential expression')
    }
  })
  
  # get the selected choices for proteins
  output$colSelect <- renderUI({
    checkboxGroupInput(
      'columns',
      'Select the data columns:',
      colnames(values$orig_data),
      colnames(values$orig_data)[grep('*rea', colnames(values$orig_data))]
    )
  })
  
  # get the selected choices for peptides
  output$pep_colSelect <- renderUI(
    checkboxGroupInput(
      'pep_columns',
      'Select the data columns:',
      colnames(pep_values$orig_data),
      colnames(pep_values$orig_data)[grep('*rea', colnames(pep_values$orig_data))]
    )
  )
  
  # group the selected choices for proteins
  output$groups <- renderUI(
    if (input$n_groups > 0) {
      lapply(
        input$columns,
        function(x) {
          selectInput(
            x,
            paste(x, ':', sep=''),
            1:input$n_groups,
            1
          )
        }
      )
    } else {
      p('This step is unnecessary; continue to step 4')
    }
  )
  
  # popup with info about sample grouping
  observeEvent(input$gHelp, {
    shinyalert(
      text='If you know the groupings of samples a priori (e.g. control vs. disease), select the number of distinct groups of samples in your data; otherwise, leave the number at 0',
      type='info'
    )
  })
  
  # popup with info about protein selection
  observeEvent(input$pHelp, {
    shinyalert(
      text='If you are interested in a subset of proteins you can enter those proteins here, either in PEAKS accession format or gene name format',
      type='info'
    )
  })
  
  # group the selected choices for peptides
  output$pep_groups <- renderUI(
    if (input$pep_n_groups > 0) {
      lapply(
        input$pep_columns,
        function(x) {
          selectInput(
            x,
            paste(x, ':', sep=''),
            1:input$pep_n_groups,
            1
          )
        }
      )
    } else {
      p('This step is unnecessary; continue to step 4')
    }
  )
  
  # popup with info about sample grouping
  observeEvent(input$pep_gHelp, {
    shinyalert(
      text='If you know the groupings of samples a priori (e.g. control vs. disease), select the number of distinct groups of samples in your data; otherwise, leave the number at 0',
      type='info'
    )
  })
  
  # popup with info about protein selection
  observeEvent(input$pep_pHelp, {
    shinyalert(
      text='Enter the protein for which you are interested in seeing peptide changes',
      type='info'
    )
  })
  
  # handle table download for proteins
  output$downloadTable <- downloadHandler(
    filename=function() {
      paste('Scaled protein data ', Sys.time(), '.csv', sep='')
    },
    content=function(fname) {
      write.csv(values$scaled_data, fname, row.names=T, quote=F)
    }
  )
  
  # print to table for proteins
  output$sdata <- DT::renderDataTable({
    s <- round(values$scaled_data, 3)
    s
  })
  
  # handle table download for peptides
  output$pep_downloadTable <- downloadHandler(
    filename=function() {
      paste('Scaled peptide data ', Sys.time(), '.csv', sep='')
    },
    content=function(fname) {
      write.csv(pep_values$scaled_data, fname, row.names=T, quote=F)
    }
  )
  
  # print to table for peptides
  output$pep_sdata <- DT::renderDataTable({
    d <- pep_values$scaled_data
    p <- d$Protein.Accession
    s <- as.matrix(d[,2:ncol(d)])
    s <- round(s, 3)
    
    # merge
    qq <- cbind.data.frame(p, s)
    
    # correct accession column name
    colnames(qq)[1] <- 'Protein.Accession'
    
    # return
    qq
  })
  
  # handle heatmap download for proteins
  output$downloadHeatmap <- downloadHandler(
    filename=function() {
      paste('Protein heatmap ', Sys.time(), '.png', sep='')
    },
    content=function(fname) {
      png(fname)
      heatmap.2(
        values$scaled_data,
        trace='none',
        col=colorpanel(25, 'blue', 'red'),
        main=paste(input$pName, '\nProtein heatmap', sep='')
      )
      dev.off()
    }
  )
  
  # plot heatmap for proteins
  output$heatmap <- renderPlot({
    heatmap.2(
      values$scaled_data,
      trace='none',
      col=colorpanel(50, 'blue', 'red'),
      main=paste(input$pName, '\nProtein heatmap', sep='')
    )
  })
  
  # handle heatmap download for peptides
  output$pep_downloadHeatmap <- downloadHandler(
    filename=function() {
      paste('Peptide heatmap ', input$pep_prot, ' ', Sys.time(), '.png', sep='')
    },
    content=function(fname) {
      # set data
      s <- as.matrix(pep_values$scaled_data[,2:ncol(pep_values$scaled_data)])
      
      png(fname)
      heatmap.2(
        s[,2:ncol(s)],
        trace='none',
        col=colorpanel(25, 'blue', 'red'),
        main=paste(input$pep_pName, '\nPeptide heatmap\n', input$pep_prot, sep='')
      )
      dev.off()
    }
  )
  
  # plot heatmap for peptides
  output$pep_heatmap <- renderPlot({
    # set data
    s <- as.matrix(pep_values$scaled_data[,2:ncol(pep_values$scaled_data)])
    
    colnames(s) <- sapply(
      colnames(s),
      function(x) {
        paste(substr(x, 1, 2), strsplit(x, '\\D+')[[1]][-1], sep='')
      }
    )
    
    heatmap.2(
      s,
      trace='none',
      col=colorpanel(50, 'blue', 'red'),
      main=paste(input$pep_pName, '\nHeatmap\n', input$pep_prot, sep='')
    )
  })
  
  # handle PCA download for proteins
  output$downloadPCA <- downloadHandler(
    filename=function() {
      paste('Protein PCA ', Sys.time(), '.png', sep='')
    },
    content=function(fname) {
      s <- t(values$scaled_data)
      s2 <- cbind(s, sapply(rownames(s), function(x) paste('Group', as.character(input[[x]]))))
      colnames(s2)[ncol(s2)] <- 'Groups'
      
      png(fname)
      print(
        autoplot(prcomp(s), data=s2, colour='Groups', shape=FALSE) +
        ggtitle(paste(input$pName, '\nProtein PCA', sep='')) +
        theme(plot.title=element_text(hjust=0.5))
      )
      dev.off()
    }
  )
  
  # plot PCA for proteins
  output$pcaPlot <- renderPlot({
    s <- t(values$scaled_data)
    s2 <- cbind(s, sapply(rownames(s), function(x) paste('Group', as.character(input[[x]]))))
    colnames(s2)[ncol(s2)] <- 'Groups'
    
    pc <- prcomp(s)
    autoplot(pc, data=s2, colour='Groups', shape=FALSE) +
    ggtitle(paste(input$pName, '\nProtein PCA', sep='')) +
    theme(plot.title=element_text(hjust=0.5))
  })
  
  # PCA loadings table for proteins
  output$loadings <- DT::renderDataTable({
    s  <- t(values$scaled_data)
    pc <- prcomp(s)
    
    aload <- abs(pc$rotation)
    q <- sweep(aload, 2, colSums(aload), '/')*100
    q <- q[order(-q[,1]),1:5]
    q <- round(q, 3)
    
    # make the matrix a character
    for (i in 1:nrow(q)) {
      for (j in 1:ncol(q)) {
        q[i,j] <- paste(q[i,j], '%', sep='')
      }
    }
    
    q
  },
  rownames=TRUE)
  
  # handle PCA loadings table download for proteins
  output$download_loadings <- downloadHandler(
    filename=function() {
      paste('Protein PCA loadings ', Sys.time(), '.csv', sep='')
    },
    content=function(fname) {
      s <- t(values$scaled_data)
      pc <- prcomp(s)
      
      aload <- abs(pc$rotation)
      q <- sweep(aload, 2, colSums(aload), '/')*100
      q <- q[order(-q[,1]),1:5]
      q <- round(q, 3)
      
      # make the matrix a character
      for (i in 1:nrow(q)) {
        for (j in 1:ncol(q)) {
          q[i,j] <- paste(q[i,j], '%', sep='')
        }
      }
      
      write.csv(q, fname, row.names=F, quote=F)
    }
  )
  
  # handle PCA download for peptides
  output$pep_downloadPCA <- downloadHandler(
    filename=function() {
      paste('Peptide PCA ', input$pep_prot, ' ', Sys.time(), '.png', sep='')
    },
    content=function(fname) {
      s <- t(pep_values$scaled_data[,2:ncol(pep_values$scaled_data)])
      s2 <- cbind(s, sapply(rownames(s), function(x) paste('Group', as.character(input[[x]]))))
      colnames(s2)[ncol(s2)] <- 'Groups'
      
      # change row names
      rownames(s2) <- sapply(
        rownames(s2),
        function(x) {
          paste(substr(x, 1, 2), strsplit(x, '\\D+')[[1]][-1], sep='')
        }
      )
      
      png(fname)
      print(
        autoplot(prcomp(s), data=s2, colour='Groups', shape=FALSE) +
          ggtitle(paste(input$pep_pName, '\nPeptide PCA\n', input$pep_prot, sep='')) +
          theme(plot.title=element_text(hjust=0.5))
      )
      dev.off()
    }
  )
  
  # plot PCA for peptides
  output$pep_pcaPlot <- renderPlot({
    s <- t(pep_values$scaled_data[,2:ncol(pep_values$scaled_data)])
    s2 <- cbind(s, sapply(rownames(s), function(x) paste('Group', as.character(input[[x]]))))
    colnames(s2)[ncol(s2)] <- 'Groups'
    
    # change row names
    rownames(s2) <- sapply(
      rownames(s2),
      function(x) {
        paste(substr(x, 1, 2), strsplit(x, '\\D+')[[1]][-1], sep='')
      }
    )
    
    pc <- prcomp(s)
    autoplot(pc, data=s2, colour='Groups', shape=FALSE) +
      ggtitle(paste(input$pep_pName, '\nPeptide PCA\n', input$pep_prot, sep='')) +
      theme(plot.title=element_text(hjust=0.5))
  })
  
  # PCA loadings table for peptides
  output$pep_loadings <- DT::renderDataTable({
    s  <- t(pep_values$scaled_data[,2:ncol(pep_values$scaled_data)])
    pc <- prcomp(s)
    
    aload <- abs(pc$rotation)
    q <- sweep(aload, 2, colSums(aload), '/')*100
    q <- q[order(-q[,1]),1:5]
    q <- round(q, 3)
    
    # make the matrix a character
    for (i in 1:nrow(q)) {
      for (j in 1:ncol(q)) {
        q[i,j] <- paste(q[i,j], '%', sep='')
      }
    }
    
    q
  },
  rownames=TRUE)
  
  # handle PCA loadings table download for peptides
  output$pep_download_loadings <- downloadHandler(
    filename=function() {
      paste('Peptide PCA loadings ', Sys.time(), '.csv', sep='')
    },
    content=function(fname) {
      s <- t(pep_values$scaled_data[,2:ncol(pep_values$scaled_data)])
      pc <- prcomp(s)
      
      aload <- abs(pc$rotation)
      q <- sweep(aload, 2, colSums(aload), '/')*100
      q <- q[order(-q[,1]),1:5]
      q <- round(q, 3)
      
      # make the matrix a character
      for (i in 1:nrow(q)) {
        for (j in 1:ncol(q)) {
          q[i,j] <- paste(q[i,j], '%', sep='')
        }
      }
      
      write.csv(q, fname, row.names=T, quote=F)
    }
  )
  
  # handle hierarchical clustering download for proteins
  output$downloadClust <- downloadHandler(
    filename=function() {
      paste('Protein hierarchical clustering ', Sys.time(), '.png', sep='')
    },
    content=function(fname) {
      s <- t(values$scaled_data)
      
      png(fname)
      plot(hclust(dist(s)), main=paste(input$pName, '\nProtein hierarchical clustering', sep=''), xlab='Samples', sub='')
      dev.off()
    }
  )
  
  # plot hierarchical clustering for proteins
  output$clust <- renderPlot({
    s <- t(values$scaled_data)
    
    plot(hclust(dist(s)), main=paste(input$pName, '\nProtein hierarchical clustering', sep=''), xlab='Samples', sub='')
  })
  
  # handle hierarchical clustering download for peptides
  output$pep_downloadClust <- downloadHandler(
    filename=function() {
      paste('Peptide hierarchical clustering ', Sys.time(), '.png', sep='')
    },
    content=function(fname) {
      s <- t(pep_values$scaled_data[,2:ncol(pep_values$scaled_data)])
      
      # change row names
      rownames(s) <- sapply(
        rownames(s),
        function(x) {
          paste(substr(x, 1, 2), strsplit(x, '\\D+')[[1]][-1], sep='')
        }
      )
      
      png(fname)
      plot(hclust(dist(s)), main=paste(input$pName, '\nPeptide hierarchical clustering\n', input$pep_prot, sep=''), xlab='Samples', sub='')
      dev.off()
    }
  )
  
  # plot hierarchical clustering for peptides
  output$pep_clust <- renderPlot({
    s <- t(pep_values$scaled_data[,2:ncol(pep_values$scaled_data)])
    
    # change row names
    rownames(s) <- sapply(
      rownames(s),
      function(x) {
        paste(substr(x, 1, 2), strsplit(x, '\\D+')[[1]][-1], sep='')
      }
    )
    
    plot(hclust(dist(s)), main=paste(input$pName, '\nPeptide hierarchical clustering\n', input$pep_prot, sep=''), xlab='Samples', sub='')
  })
  
  # handle t-test table download for proteins
  output$download_diffexp <- downloadHandler(
    filename=function() {
      paste('Protein t-test ', Sys.time(), '.csv', sep='')
    },
    content=function(fname) {
      s <- values$scaled_data
      
      # check the number of groups
      if (input$n_groups == 2) { # two groups, let's do t-tests
        # get groups
        grp1 <- c()
        grp2 <- c()
        
        for (i in 1:length(input$columns)) {
          if (input[[input$columns[i]]] == 1) {
            grp1 <- c(grp1, i)
          } else {
            grp2 <- c(grp2, i)
          }
        }
        
        tt <- data.frame(Protein=character(), t=double(), df=double(), p.value=double(), stringsAsFactors=F)
        j  <- 1
        
        for (i in 1:nrow(s)) {
          q <- try(t.test(s[i,grp1], s[i,grp2]))
          
          if (class(q) != 'try-error') {
            tt[j,] <- c(rownames(s)[i], q$statistic, q$parameter, q$p.value)
            j <- j + 1
          }
        }
        
        # multiple testing correction
        tt$bonferroni <- p.adjust(tt$p.value)
        tt$FDR        <- p.adjust(tt$p.value, 'fdr')
        
        # write to file
        write.csv(tt, fname, row.names=F, quote=F)
      } else { # at least three groups, let's do ANOVA
        out <- data.frame(Protein=character(), F=double(), p.value=double(), stringsAsFactors=F)
        x  <- 1
        
        for (i in 1:nrow(s)) {
          tst <- data.frame(Value=double(), Group=character(), stringsAsFactors=F)
          ct  <- 1
          
          for (j in 1:ncol(s)) {
            if (!is.na(s[i,j])) {
              tst[ct,] <- c(s[i,j], input[[colnames(s)[j]]])
              ct <- ct + 1
            }
          }
          
          # fix up columns (just in case)
          tst[,1] <- as.numeric(tst[,1])
          tst[,2] <- factor(tst[,2])
          
          # model
          fit <- try(lm(Value ~ Group, data=tst), silent=T)
          
          # check if it worked
          if (class(fit) != 'try-error') {
            # get anova
            an <- anova(fit)
            
            # add row to output
            out[x,] <- c(rownames(s)[i], an[1,4], an[1,5])
            
            # increment row number
            x <- x + 1
          }
        }
        
        # multiple testing correction
        out$bonferroni <- p.adjust(out$p.value)
        out$FDR        <- p.adjust(out$p.value, 'fdr')
        
        # write to file
        write.csv(out, fname, row.names=F, quote=F)
      }
    }
  )
  
  # print differential expression results for proteins
  output$diffexp <- DT::renderDataTable(
    {
      s <- values$scaled_data
      
      # check the number of groups
      if (input$n_groups == 2) { # two groups, let's do t-tests
        # get groups
        grp1 <- c()
        grp2 <- c()
        
        for (i in 1:length(input$columns)) {
          if (input[[input$columns[i]]] == 1) {
            grp1 <- c(grp1, i)
          } else {
            grp2 <- c(grp2, i)
          }
        }
        
        tt <- data.frame(Protein=character(), t=double(), df=double(), p.value=double(), stringsAsFactors=F)
        j  <- 1
        
        for (i in 1:nrow(s)) {
          q <- try(t.test(s[i,grp1], s[i,grp2]))
          
          if (class(q) != 'try-error') {
            tt[j,] <- c(rownames(s)[i], q$statistic, q$parameter, q$p.value)
            j <- j + 1
          }
        }
        
        # multiple testing correction
        tt$bonferroni <- p.adjust(tt$p.value)
        tt$FDR        <- p.adjust(tt$p.value, 'fdr')
        
        # print out
        tt
      } else { # at least three groups, let's do ANOVA
        out <- data.frame(Protein=character(), F=double(), p.value=double(), stringsAsFactors=F)
        x  <- 1
        
        for (i in 1:nrow(s)) {
          tst <- data.frame(Value=double(), Group=character(), stringsAsFactors=F)
          ct  <- 1
          
          for (j in 1:ncol(s)) {
            if (!is.na(s[i,j])) {
              tst[ct,] <- c(s[i,j], input[[colnames(s)[j]]])
              ct <- ct + 1
            }
          }
          
          # fix up columns (just in case)
          tst[,1] <- as.numeric(tst[,1])
          tst[,2] <- factor(tst[,2])
          
          # model
          fit <- try(lm(Value ~ Group, data=tst), silent=T)
          
          # check if it worked
          if (class(fit) != 'try-error') {
            # get anova
            an <- anova(fit)
            
            # add row to output
            out[x,] <- c(rownames(s)[i], an[1,4], an[1,5])
            
            # increment row number
            x <- x + 1
          }
        }
        
        # multiple testing correction
        out$bonferroni <- p.adjust(out$p.value)
        out$FDR        <- p.adjust(out$p.value, 'fdr')
        
        out
      }
    },
    rownames=FALSE
  )
  
  # handle t-test table download for peptides
  output$pep_download_diffexp <- downloadHandler(
    filename=function() {
      paste('Peptide t-test ', Sys.time(), '.csv', sep='')
    },
    content=function(fname) {
      s <- pep_values$scaled_data[,2:ncol(pep_values$scaled_data)]
      
      # check the number of groups
      if (input$pep_n_groups == 2) { # two groups, let's do t-tests
        # get groups
        grp1 <- c()
        grp2 <- c()
        
        for (i in 1:length(input$pep_columns)) {
          if (input[[input$pep_columns[i]]] == 1) {
            grp1 <- c(grp1, i)
          } else {
            grp2 <- c(grp2, i)
          }
        }
        
        tt <- data.frame(Peptide=character(), t=double(), df=double(), p.value=double(), stringsAsFactors=F)
        j  <- 1
        
        for (i in 1:nrow(s)) {
          q <- try(t.test(s[i,grp1], s[i,grp2]))
          
          if (class(q) != 'try-error') {
            tt[j,] <- c(rownames(s)[i], q$statistic, q$parameter, q$p.value)
            j <- j + 1
          }
        }
        
        # multiple testing correction
        tt$bonferroni <- p.adjust(tt$p.value)
        tt$FDR        <- p.adjust(tt$p.value, 'fdr')
        
        # write to file
        write.csv(tt, fname, row.names=F, quote=F)
      } else { # at least three groups, let's do ANOVA
        out <- data.frame(Peptide=character(), F=double(), p.value=double(), stringsAsFactors=F)
        x   <- 1
        
        for (i in 1:nrow(s)) {
          tst <- data.frame(Value=double(), Group=character(), stringsAsFactors=F)
          ct  <- 1
          
          for (j in 1:ncol(s)) {
            if (!is.na(s[i,j])) {
              tst[ct,] <- c(s[i,j], input[[colnames(s)[j]]])
              ct <- ct + 1
            }
          }
          
          # fix up columns (just in case)
          tst[,1] <- as.numeric(tst[,1])
          tst[,2] <- factor(tst[,2])
          
          # model
          fit <- try(lm(Value ~ Group, data=tst), silent=T)
          
          # check if it worked
          if (class(fit) != 'try-error') {
            # get anova
            an <- anova(fit)
            
            # add row to output
            out[x,] <- c(rownames(s)[i], an[1,4], an[1,5])
            
            # increment row number
            x <- x + 1
          }
        }
        
        # multiple testing correction
        out$bonferroni <- p.adjust(out$p.value)
        out$FDR        <- p.adjust(out$p.value, 'fdr')
        
        # write to file
        write.csv(out, fname, row.names=F, quote=F)
      }
    }
  )
  
  # print differential expression results for peptides
  output$pep_diffexp <- DT::renderDataTable(
    {
      s <- pep_values$scaled_data[,2:ncol(pep_values$scaled_data)]
      
      # check the number of groups
      if (input$pep_n_groups == 2) { # two groups, let's do t-tests
        # get groups
        grp1 <- c()
        grp2 <- c()
        
        for (i in 1:length(input$pep_columns)) {
          if (input[[input$pep_columns[i]]] == 1) {
            grp1 <- c(grp1, i)
          } else {
            grp2 <- c(grp2, i)
          }
        }
        
        tt <- data.frame(Peptide=character(), t=double(), df=double(), p.value=double(), stringsAsFactors=F)
        j  <- 1
        
        for (i in 1:nrow(s)) {
          q <- try(t.test(s[i,grp1], s[i,grp2]))
          
          if (class(q) != 'try-error') {
            tt[j,] <- c(rownames(s)[i], q$statistic, q$parameter, q$p.value)
            j <- j + 1
          }
        }
        
        # multiple testing correction
        tt$bonferroni <- p.adjust(tt$p.value)
        tt$FDR        <- p.adjust(tt$p.value, 'fdr')
        
        # print out
        tt
      } else { # at least three groups, let's do ANOVA
        out <- data.frame(Peptide=character(), F=double(), p.value=double(), stringsAsFactors=F)
        x  <- 1
        
        for (i in 1:nrow(s)) {
          tst <- data.frame(Value=double(), Group=character(), stringsAsFactors=F)
          ct  <- 1
          
          for (j in 1:ncol(s)) {
            if (!is.na(s[i,j])) {
              tst[ct,] <- c(s[i,j], input[[colnames(s)[j]]])
              ct <- ct + 1
            }
          }
          
          # fix up columns (just in case)
          tst[,1] <- as.numeric(tst[,1])
          tst[,2] <- factor(tst[,2])
          
          # model
          fit <- try(lm(Value ~ Group, data=tst), silent=T)
          
          # check if it worked
          if (class(fit) != 'try-error') {
            # get anova
            an <- anova(fit)
            
            # add row to output
            out[x,] <- c(rownames(s)[i], an[1,4], an[1,5])
            
            # increment row number
            x <- x + 1
          }
        }
        
        # multiple testing correction
        out$bonferroni <- p.adjust(out$p.value)
        out$FDR        <- p.adjust(out$p.value, 'fdr')
        
        out
      }
    },
    rownames=FALSE
  )
}

# run the app
shinyApp(ui=ui, server=server)