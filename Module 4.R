# Module 4 Code

# CS 688

# Fall 1 2020

# September 29, 2020


install.packages("tidyverse")
install.packages("aRxiv")
install.packages("rvest")
install.packages("shiny")
install.packages("WikipediR")
install.packages("shinydashboard")

### --- Example 1: Retrieving abstracts from arXiv --------
library(aRxiv)
arxiv_count('au:"Andrew Gelman"') # How many articles by Andrew Gelman?
rec <- arxiv_search('au:"Andrew Gelman"') # Retrieve articles from Andrew Gelman (only 10 articles by default - see ?arvix_search for more info)
nrow(rec)

# More complicated search - author and abstract
rec <- arxiv_search('au:"Andrew Gelman" AND abs:"hypothesis testing"')
nrow(rec)

# Display data frame returned from arXiv
library(tidyverse)
as_tibble(rec)

# Open the articles in Web Browser
arxiv_open(rec)

# Text Mining is an option, too - here's a corpus
library(tm)
arxivCorpus <- VCorpus(VectorSource(rec$abstract))


### --- Example 2: Access Virtual Cooking Classes --------
library(rvest)
library(stringr)
library(lubridate)
classeshtml <- read_html("https://jungleshop.junglejims.com/classes-c31.aspx")
classesxml <- classeshtml %>% html_nodes("a.nextProdName")  # same as html_nodes(classeshtml,"a.nextProdName")
class(classesxml)
classesvector <- html_text(classesxml) # Display Current Classes
class(classesvector)
## one of these isn't a class!
dates <- suppressWarnings(mdy(word(classesvector,1))) # will fail on the one that isn't a date
classesvector <- classesvector[!is.na(dates)]
dates <- dates[!is.na(dates)]
classesdataframe <- data.frame(date=dates,classes=classesvector)
classesdataframe

### --- Example 3: Access XML File. --------
### OPTIONAL - but it's in the module

## sequencing.xml file content
# <data>
#   <sequence id = "ancestralSequence"> 
#   <taxon id="test1">Taxon1
# </taxon>       
#   GCAGTTGACACCCTT
# </sequence>
#   <sequence id = "currentSequence"> 
#   <taxon id="test2">Taxon2
# </taxon>       
#   GACGGCGCGGAccAG
# </sequence>
#   </data>

# Set Your Path to the folder where you saved the "sequencing.xml" 
setwd("C:/users/Michael Joner/Google Drive/BU CS 688/Instructor/4 Module/") 
library(XML)
# read XML File 
x <- xmlParse("sequencing.xml")
x

# returns a *list* of text nodes under "sequence" tag
nodeSet <- xpathApply(x,"//sequence/text()")
nodeSet

# loop over the list returned, and get and modify the node value:
zz<-sapply(nodeSet,function(G){
  text = paste("Ggg",xmlValue(G),"CCaaTT",sep="")  # add Ggg to beginning and CCaaTT to end
  text = gsub("[^A-Z]","",text)  # remove the lower case
  # text = toupper(text)  # make the lower case into upper case
  xmlValue(G) = text
})

nodeSet
x



### --- Example 4: Search Wikipedia web pages. --------
# Save these 3 files separately in the same folder (Related to HW#4)
# Example: Shiny app that search Wikipedia web pages
# File: ui.R 
library(shiny)
titles <- c("Web_analytics","Text_mining","Integral", "Calculus", 
            "Lists_of_integrals", "Derivative","Alternating_series",
            "Pablo_Picasso","Vincent_van_Gogh","Leo_Tolstoy","Web_crawler")
# Define UI for application 
shinyUI(fluidPage(
  # Application title (Panel 1)
  titlePanel("Wiki Pages"), 
  # Widget (Panel 2) 
  sidebarLayout(
    sidebarPanel(h3("Search panel"),
                 # Where to search 
                 selectInput("select",
                             label = h5("Choose from the following Wiki Pages on"),
                             choices = titles,
                             selected = titles, multiple = TRUE),
                 # Start Search
                 submitButton("Results")
    ),
    # Display Panel (Panel 3)
    mainPanel(                   
      h1("Display Panel",align = "center"),
      plotOutput("distPlot")
    )
  )
))


# Example: Shiny app that search Wikipedia web pages
# File: server.R 
library(shiny)
library(tm)
library(stringi)
# library(proxy)
source("WikiSearch.R")

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({ 
    
    # Progress Bar while executing function
    withProgress({
      setProgress(message = "Mining Wikipedia ...")
      result <- SearchWiki(input$select)
    })

    plot(result, labels = input$select, sub = "",main="Wikipedia Search")
  })
})


# Example: Shiny app that search Wikipedia web pages
# File: WikiSearch.R

# Wikipedia Search
library(tm)
library(stringi)
library(WikipediR)

SearchWiki <- function (titles) {
  articles <- lapply(titles,function(i) page_content("en","wikipedia", page_name = i,as_wikitext=TRUE)$parse$wikitext)
  
  docs <- VCorpus(VectorSource(articles)) # Get Web Pages' Corpus
  remove(articles)
  
  # Text analysis - Preprocessing 
  transform.words <- content_transformer(function(x, from, to) gsub(from, to, x))
  temp <- tm_map(docs, transform.words, "<.+?>", " ")
  temp <- tm_map(temp, transform.words, "\t", " ")
  temp <- tm_map(temp, content_transformer(tolower)) # Conversion to Lowercase
  temp <- tm_map(temp, PlainTextDocument)
  temp <- tm_map(temp, stripWhitespace)
  temp <- tm_map(temp, removeWords, stopwords("english"))
  temp <- tm_map(temp, removePunctuation)
  temp <- tm_map(temp, stemDocument, language = "english") # Perform Stemming
  remove(docs)
  
  # Create Dtm 
  dtm <- DocumentTermMatrix(temp)
  dtm <- removeSparseTerms(dtm, 0.4)
  dtm$dimnames$Docs <- titles
  docsdissim <- dist(as.matrix(dtm), method = "euclidean") # Distance Measure
  h <- hclust(as.dist(docsdissim), method = "ward.D2") # Group Results
}

### --- Example 5: using shinydashboard --------
# A dashboard has three parts: a header, a sidebar, and a body. 
# You can view it at the R console by using the shinyApp() function.
# Or if you save this into it's own folder, you can use "Run App" just like you do for ui.R/server.R
# File: app.R
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "My first dashboard"), 
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 300, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(100)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
}

shinyApp(ui, server)
# End of app.R

### --- Example 6: Search Reuters documents. --------
### Optional, time permitting. It is a review of things we talked about last week.
# Reuters Files Location
reut21578 <- system.file("texts", "crude", package = "tm") 
# Get Corpus
reuters <- VCorpus(DirSource(reut21578), 
                   readerControl = list(reader = readReut21578XMLasPlain)) 
# Text analysis - Preprocessing with tm package functionality
temp  <- reuters 
transform.words <- content_transformer(function(x, from, to) gsub(from, to, x))
temp <- tm_map(temp, content_transformer(tolower)) # Conversion to Lowercase
temp <- tm_map(temp, stripWhitespace)
temp <- tm_map(temp, removeWords, stopwords("english"))
temp <- tm_map(temp, removePunctuation)
# Create Document Term Matrix 
dtm <- DocumentTermMatrix(temp)

### Example 7: Confusion Matrix (Example 4.1)
spam <- data.frame(Spam=c(11,16),Not.Spam=c(10,63))
row.names(spam) <- c("Spam","Not.Spam")
spam  # Rows 1 and 2 indicate what the algorithm is predicting; Columns 1 and 2 indicate what is reality
sum(spam)  # Number of documents
rowSums(spam)  # algorithm predictions of the e-mails
colSums(spam)  # actual reality of the e-mails
Precision <- spam[1,1]/rowSums(spam)[1]
Precision
Recall <- spam[1,1]/colSums(spam)[1]
Recall
F <- 2*Precision*Recall/(Precision+Recall)
F
F <- (Precision*Recall)/mean(c(Precision,Recall))
F













