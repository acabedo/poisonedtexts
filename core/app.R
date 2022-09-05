library(shiny)
library(udpipe)
library(DT)
library(tidyverse)
library(ngram)
library(tidytext)
library(textreadr)
library(stringi)
library(reshape2)
library(shinycssloaders)
library(party)
library(plotly)
library(shinybusy)


ui <- fluidPage(

    # Application title
    titlePanel("Poisoned texts. v.1.0 beta"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 4,
          
          splitLayout(cellWidths = c("33%", "67%"),
                      cellArgs = list(style = "align: right"),
                     tags$image(src="poisoned-logo.png",width=90,height=100),
          # selectInput("tagono","Add Upos tags?",choices = c("yes","no"),selected = "no"), 
          fileInput("texts","Input TXT texts",multiple = TRUE,accept = c(".pdf",".docx",".txt"))),
          
          selectInput("udpipemodel","Udpipe Language Model",choices = c("spanish","english","french","catalan","italian","german","chinese","portuguese")),
            
          #   selectInput("deletesymbol","Delete symbols and punctuation?",choices = c("yes","no")),
          # selectInput("deletenumbers","Delete numbers?",choices = c("yes","no")),
            sliderInput("stringcoincidence","Number of ngrams",
                        min = 1,
                        max = 50,
                        value = 3
                        ),
            
          actionButton("poison",icon=icon(name = "skull",lib = "font-awesome"),"Generate"),
          tags$hr(),

          shinycssloaders::withSpinner(downloadButton("report", "Generate report")),
          radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),selected = 'HTML',inline = TRUE)
            
        ,
        tags$hr(),
        tags$p("Cabedo, A. (2022) Poisoned texts. V.1.0 beta. Available at:",tags$a(href="https://github.com/acabedo/poisonedtexts","https://github.com/acabedo/poisonedtexts"),tags$br(),
               tags$p("GNU General Public License v3.0 ")),
        
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          tabsetPanel(type = "tabs",
                      
                             
                      tabPanel("File vs File",
                               sliderInput("copylimit",
                                           "Percentage of similarity treshold",
                                           min = 1,
                                           max = 50,
                                           value = 10),
                               shinycssloaders::withSpinner(DTOutput("coocout"))),
                      tabPanel("Summaries",DTOutput("summarytable"),DTOutput("summarytable2")),
                      tabPanel("Tokens",
                               selectizeInput(
                                 inputId = "filtertoken",
                                 label = "Enter a token to filter",
                                 choices = NULL,
                                 selected = NULL,
                                 multiple = TRUE,
                                 width = "100%",
                                 options = list(
                                   'plugins' = list('remove_button'),
                                   'create' = TRUE,
                                   'persist' = TRUE
                                 )
                               )
                      ,DTOutput("tokentable"),actionButton("show1","Help")),         
                      tabPanel("Upos",selectInput("filterupos","Filter upos",choices = c("NOUN","ADJ","ADV","VERB","PUNCT","PROPN","DET"),multiple = TRUE), DTOutput("upostable"),actionButton("show","Help")),          
                      tabPanel("Ngrams", DTOutput("annotationgrams")),
                      tabPanel("Matrix", DTOutput("ngrammatrix")),
                      # tabPanel("Tagged words", shinycssloaders::withSpinner(DTOutput("annotation"))),
                      tabPanel("Sentences",uiOutput("selecttable"), DTOutput("sentences")),
                      tabPanel("Style", 
                               selectInput("selectmap","Add variables to heatmap",choices = c("qdet","qpropn","qadv","qprep","qcconj","qcomma","qcolon"),multiple = TRUE),
                               DTOutput("styleoutput"),plotOutput("treeout")),
                      tabPanel("Texts",
                               DTOutput("textsoutput"))
                      
                     
          )
          
        )
    
))

# Define server logic required to draw a histogram
server <- function(input, output) {

# Reactives ---------------------------------------------------------------

  spanish <- reactive(udpipe_download_model(language = input$udpipemodel))
  udmodel_spanish <- reactive(udpipe_load_model(file = spanish()$file_model))
  
  annotation <- eventReactive(input$poison,{
  
      req(input$texts)
lista <- purrr::map(input$texts$datapath, read_file) %>%
        purrr::set_names(input$texts$name)

# if(input$tagono=="no")    
# {y <- udpipe_annotate(udmodel_spanish(), x = as.character(lista),tagger = "none", parser = "none")} else {y <- udpipe_annotate(udmodel_spanish(), x = as.character(lista))}
y <- udpipe_annotate(udmodel_spanish(), x = as.character(lista))
y <- as.data.frame(y)
d <- input$texts%>%mutate(doc_id=paste("doc",row_number(),sep=""))%>%select(doc_id,name)
y <- y%>%left_join(d,by="doc_id")%>%mutate(doc_id=name)%>%select(-name)
if(is.null(input$filterupos)){y}else{y<- y%>%filter(!(upos%in%!!input$filterupos))}
if(is.null(input$filtertoken)){y}else{y<- y%>%filter(!(token%in%!!input$filtertoken))}
  })
  
 # output$tokensfilter <- renderUI(selectInput("filtertoken","Filter token",choices = unique(annotation()%>%select(token)),multiple=TRUE))
  
  output$selecttable <- renderUI(
    selectInput("ttablesel","Select filenames to check matching ngrams",choices = summarytable()%>%select(filename),multiple = TRUE)

  )
  
style <- reactive({
    
    p <- annotation()%>%group_by(doc_id,sentence_id)%>%summarise(
      words = n(),
      qnoun = (sum(upos=="NOUN",na.rm = TRUE)/words)*100,
      qpropn = (sum(upos=="PROPN",na.rm = TRUE)/words)*100,
      qcconj = (sum(upos=="CCONJ",na.rm = TRUE)/words)*100,
      qadv = (sum(upos=="ADV",na.rm = TRUE)/words)*100,
      qprep = (sum(upos=="ADP",na.rm = TRUE)/words)*100,
      qadj = (sum(upos=="ADJ",na.rm = TRUE)/words)*100,
      qverb = (sum(upos=="VERB",na.rm = TRUE)/words)*100,
      qdet = (sum(upos=="DET",na.rm = TRUE)/words)*100,
      qcomma = (sum(token==",",na.rm = TRUE)/words)*100,
      qcolon = (sum(token==";",na.rm = TRUE)/words)*100
      
    )%>%group_by(doc_id)%>%select(-sentence_id)%>%summarise_if(is.numeric, mean, na.rm = TRUE)%>%mutate(across(where(is.numeric), round, 1))
    
    
  })
output$styleoutput <- renderDT({datatable(style()%>%select(doc_id,words,qnoun,qadj,qverb,!!input$selectmap))})
annotationgrams <- reactive({

annotation <- annotation()%>%group_by(doc_id,sentence_id,sentence)%>%summarise(n())


  
  p<- annotation %>%group_by(doc_id,sentence_id)%>% unnest_tokens(ngram,sentence,token="ngrams", n= input$stringcoincidence)%>%group_by(doc_id,sentence_id,ngram)%>%summarise(freq=n())%>%filter(ngram!="")%>%select(-freq)
  

 


    
   
    
  })
  
  summarytable <- reactive({
    
    p <- annotation()%>%filter(upos!="PUNCT")%>% group_by(doc_id)%>% summarise(paragraphs=n_distinct(paragraph_id),sentences = n_distinct(sentence_id),tokens = n())%>%rename(filename=doc_id)
    
  })
  summarytable2 <- reactive({
    
    p <- ngramstable()
    p <- melt(p)
    p <- p%>%filter(variable!="total")%>%ungroup()%>%group_by(variable)%>% mutate(total = as.numeric(value)) %>%group_by(variable,type) %>%summarise(freq=sum(total))%>%mutate(total= round((freq/sum(freq))*100,2))%>%filter(variable!="columns")%>%rename(ngram=variable)
    # freq_rel = paste((n() / total)*100,"%",sep = ""))
    
  })
  output$summarytable <- renderDT(datatable(summarytable()))
  output$summarytable2 <- renderDT(datatable(summarytable2()))
  
  sentencestable <- reactive({
  
    
    p<- annotationgrams()%>%group_by(ngram)%>%mutate(total=n())%>%filter(total>1)
    d <- annotation()%>%group_by(doc_id,sentence_id)%>%summarise(sentence=max(sentence),freq=n())
    p <- p%>%left_join(d,by=c("doc_id","sentence_id"))%>%select(-freq)%>%rowwise()%>% mutate(ngram2 = paste("<tag style='color:red;'>",ngram,"</tag>",sep=""), sentence2 = gsub("[\\.\\,\\:\\;\\-\\(\\)]","",sentence),sentence2= gsub(ngram,ngram2,sentence2,ignore.case = TRUE))%>%select(-ngram2,-sentence)%>%filter(doc_id%in%!!input$ttablesel)%>%arrange(ngram)
   # p <- annotation()%>%group_by(doc_id,sentence_id)%>% unnest_tokens(output=ngram,input=sentence,token="ngrams", n= input$stringcoincidence,format = "text")%>%ungroup()%>%group_by(doc_id,sentence_id,ngram)%>%summarise(freq=n())%>%group_by(ngram)%>%mutate(total=n())%>%filter(total>1)%>%select(-freq)
   # p <- p%>% left_join(annotation()%>%select(doc_id,sentence_id,sentence)%>%group_by(doc_id,sentence_id)%>%summarise(n(),sentence=max(sentence)),by=c("doc_id","sentence_id"))
   

      })
  
treeres <- reactive({
  
  p <- style()%>%select(doc_id,words,qnoun,qnoun,qadj,qverb,!!input$selectmap)%>%mutate_if(is.character,as.factor)
  p <- column_to_rownames(p, var="doc_id")
  p <- scale(p)
  # d <- ctree(doc_id~., data=p)
  
  
})
# Outputs -----------------------------------------------------------------
output$treeout <- renderPlot(heatmap(treeres(), scale = "column"))
output$sentences <- renderDT({datatable(sentencestable(),escape = FALSE)})
output$uploadtable <- renderTable({
  req(input$texts)
   d <- input$texts
   d <- d%>%mutate(filename1 = paste("doc",row_number(),sep=""))
  })
  output$annotation <- renderDT({annotation()})
  output$annotationgrams <- renderDT({annotationgrams()})
  output$upostable <- renderDT({

    annotation <- annotation()%>%group_by(doc_id)%>%mutate(total = n())%>%ungroup()
    
    annotation1<- annotation%>%group_by(doc_id,upos)%>%summarise(frec = n(), frecrel = round((n()/last(total))*1000,2))%>%arrange(upos,doc_id)
  
  })
  tokentable <- reactive({
    
    annotation <- annotation()%>%group_by(doc_id)%>%mutate(total = n())%>%ungroup()
    
    annotation1<- annotation%>%group_by(doc_id,token)%>%summarise(frec = n(), frecrel = round((n()/last(total))*1000,2))%>%arrange(token,doc_id)
    
    # annotation <- annotation()%>%group_by(doc_id)%>%mutate(total = n())%>%ungroup()
    
    # annotation1<- annotation()%>%mutate(t=!!input$ttablesel)%>%group_by(doc_id,t)%>%summarise(sentence_id=max(sentence_id),token=max(token),token_id=max(token_id),sentence=max(sentence),frec = n())%>%arrange(doc_id,sentence_id,token_id)
    # annotation2 <- annotation()%>%mutate(t=!!input$ttablesel)%>%group_by(t)%>%summarise(frec2 = n())%>%filter(frec2==1)
    # annotation1 <- annotation1%>%left_join(annotation2, by="t")%>%ungroup()%>%mutate(lexico=ifelse(is.na(frec2),"shared","only"), lexico =ifelse(lag(t,1)==t|lead(t,1)==t,"once",lexico))
  })
  output$tokentable <- renderDT({
    
   datatable(tokentable())
    
  })
ngramsnumber <- reactive({p <- input$stringcoincidence})
observeEvent(input$show, {
  showModal(modalDialog(
    title = "Important message",
    "This is an important message!"
  ))
})
observeEvent(input$show1, {
  showModal(modalDialog(
    title = "Important message",
    "This is an important message!"
  ))
})
ngramstable <- reactive({

  # p <- annotationgrams()%>% group_by(ngram,doc_id)%>%summarise(freq = n())%>%spread(doc_id,freq,fill = "0",convert = TRUE)%>%ungroup()%>%mutate_at(vars(-ngram), as.numeric)%>%mutate(total = rowSums(select_if(.,is.numeric),na.rm = TRUE),type=ifelse(rowSums(.==0)==0&(total/(ncol(.)-1))==1,"once",ifelse((((rowSums(.==0)==0)|(rowSums(.==0)<ncol(.)/2)&ncol(.)>3))&total>=2,"shared","only")),columns=ncol(.))
  p <- annotationgrams()%>% group_by(ngram,doc_id)%>%summarise(freq = n())%>%spread(doc_id,freq,fill = "0",convert = TRUE)%>%ungroup()%>%mutate_at(vars(-ngram), as.numeric)%>%mutate(total = rowSums(select_if(.,is.numeric),na.rm = TRUE),
type=ifelse(rowSums(select_if(.,is.numeric)==0)>=ncol(.)-2,"only",NA),
type=ifelse(is.na(type)&(rowSums(select_if(.,is.numeric)==1)==(ncol(.)-1))|(rowSums(select_if(.,is.numeric)==1)==2&total==2),"once",type),type=ifelse(is.na(type)&total>=2,"shared",type))
  

  })

ngramscooc <- reactive({
  totals <- annotationgrams()%>%rename(filename=doc_id)%>% group_by(filename)%>%summarise(total=n())
  p <- annotationgrams()%>% group_by(ngram,doc_id)%>%summarise(freq = n())%>%mutate(freq=ifelse(freq>1,1,freq)) %>%spread(doc_id,freq,fill = "0",convert = TRUE)%>%ungroup()%>%mutate_at(vars(-ngram), as.numeric)%>%mutate(total = rowSums(select_if(.,is.numeric),na.rm = TRUE),type=ifelse(rowSums(.==0)==1,"only",NA),type=ifelse((rowSums(.>=0)/ncol(.))==1 |(rowSums(.==1)>=2 & ncol(.)>=3&total>=2) ,"once","shared"))
  # p <- annotationgrams()%>% group_by(ngram,doc_id)%>%summarise(freq = n())%>%mutate(freq=ifelse(freq>1,1,freq)) %>%spread(doc_id,freq,fill = "0",convert = TRUE)%>%ungroup()%>%mutate_at(vars(-ngram), as.numeric)%>%mutate(total = rowSums(select_if(.,is.numeric),na.rm = TRUE),type=ifelse(rowSums(.==0)==0&(total/(ncol(.)-1))==1,"once",ifelse(rowSums(.==0)==0&total>2,"shared","only")))
  X <- p%>%select(-ngram,-total,-type)
  X <- as.matrix(X)
  out <- crossprod(X)  # Same as: t(X) %*% X
  diag(out) <- 0       # (b/c you don't count co-occurrences of an aspect with itself)

out <- as.data.frame(out)
out <- add_rownames(out, var = "filename")
out <- melt(out)
out <- out%>%left_join(totals,by="filename")%>%mutate(percent=as.numeric(round((value/total)*100,2)))%>%arrange(desc(percent))%>%filter(variable!=filename)

})
output$coocout <- renderDT({datatable(ngramscooc())%>% formatStyle(
  'percent',
  color = styleInterval(input$copylimit, c('black', 'red')))})
ngramstablemelt <- reactive({p <- melt(ngramstable())})
output$ngmeltout <- renderDT({ngramstablemelt()})
  annotationtable <- reactive({
   table(annotation()$token,annotation()$doc_id)
    })
  output$ngrammatrix <- renderDT(datatable(ngramstable(),rownames = TRUE))
  output$exclusivetable <- renderDT(
    
    # annotation <- annotation()%>%group_by(doc_id)%>%mutate(total = n())%>%ungroup()
    # 
    # annotation1<- annotation%>%group_by(doc_id,token)%>%summarise(frec = n(), frecrel = round((n()/last(total))*1000,2))%>%arrange(token,doc_id)%>%filter(frec==1)
    datatable(as.data.frame.matrix(annotationtable()),rownames = TRUE)
    
  )
textscompletes <- reactive({
    
    p <- annotation()%>%select(doc_id,sentence_id,sentence)%>%group_by(doc_id,sentence_id)%>%summarise(freq=n(), sentence = max(sentence))%>%arrange(doc_id,sentence_id)%>%select(-freq)
    
  })
  output$textsoutput <- renderDT(datatable(textscompletes()))
 output$report <- downloadHandler(

    filename = function() {
      paste('poisonedtext','_',Sys.time(),'.', sep = '', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      shiny::withProgress(
        message = paste0("Downloading", input$dataset, " Data"),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
        
      src <- normalizePath('poisoned.Rmd')
      src2 <- normalizePath('poisoned-logo.png')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'poisoned.Rmd', overwrite = TRUE)
      file.copy(src2, 'poisoned-logo.png', overwrite = TRUE)
      # params <- list(table1 = ngramscooc(),sentences = sentencestable(),summary1 = summarytable(),summary2 = summarytable2())
      library(rmarkdown)
      out <- render('poisoned.Rmd', switch(
        input$format,
        PDF = pdf_document(toc = TRUE,latex_engine = "xelatex", toc_depth = 4, number_sections = TRUE), HTML = html_document(toc = TRUE, toc_depth = 4, toc_float = TRUE, number_sections = TRUE), Word = word_document(toc = TRUE, toc_depth = 4, number_sections = TRUE)
      ),)
      file.rename(out, file)
        }
      )
    }
  )
  # output$report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "report.docx",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "venomous.Rmd")
  #     file.copy("venomous.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(table1 = ngramscooc(),sentences = sentencestable(),summary1 = summarytable(),summary2 = summarytable2())
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
}

# Run the application 
shinyApp(ui = ui, server = server)
