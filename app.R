# make this into data at reed tutorial 

# install and load packages -----------------------------------------------
pkg <- c("XLConnect", "googleVis", "stringr", "ggplot2", "ggthemes", "shiny", "dygraphs", "zoo", "timeSeries")

new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}

library(XLConnect)
library(googleVis)
library(stringr)
library(ggplot2)
library(ggthemes)
if (!require("DT")) devtools::install_github("rstudio/DT")
library(DT)
library(dygraphs)
library(shiny)
library(zoo)
library(timeSeries)



# read in data that is saved across multiple excel worksheets -------------

# load excel workbook
excel <- loadWorkbook("allProjections.xlsx")

# get sheet names
sheet_names <- getSheets(excel)
names(sheet_names) <- sheet_names

# put sheets into a list of data frames
sheet_list <- lapply(sheet_names, function(.sheet){readWorksheet(object=excel, .sheet)})

# limit sheet_list to sheets with at least 1 dimension (removes sheet 1 which contains only "data notes")
sheet_list2 <- sheet_list[sapply(sheet_list, function(x) dim(x)[1]) > 0]

# code to read in each excel worksheet as individual dataframes
# for (i in 2:length(sheet_list2)){assign(paste0("df", i), as.data.frame(sheet_list2[i]))}


# clean each of the  56 data frames in list of data frames and rbind them into a single data frame  ---------------

# define function to clean data in each data frame
cleaner <- function(df){
  # drop rows with missing values 
  df <- df[rowSums(is.na(df)) == 0,] 
  # remove serial comma from all variables 
  df[,-1] <- as.numeric(gsub(",", "", as.matrix(df[,-1])))
  # create region variable
  df$region <- colnames(df[2]) # make colname region variable 
  df$region <- tolower(gsub("[[:punct:]]", " ", df$region))  # replace '.' in region with ' ' and make all chars lowercase
  df$region <- str_trim(gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", df$region, perl=TRUE), 'both') # capitolize first word and words directly after a space in region
  # rename columns
  colnames(df) <- c('year', 'total', 'native_public',  'asian_public',  'black_public',  'hispanic_public',	
                    'white_public',	'public_total',	'non_public_total', 'sum', 'region')
  # replace string code for low n with numeric 0     
  df[df=='Low N'] <- 0
  # create numeric version of year variable for graphing 
  df$Year <- as.numeric(substr(df$year, 1, 4))
  # return cleaned df      
  return(df)
}

# clean sheets and create one data frame that includes each cleaned sheet (either of these methods work)
# data <- do.call(rbind,lapply(seq_along(sheet_list2), function(x) cleaner(sheet_list2[[x]])))
data <- do.call(rbind,lapply(names(sheet_list2), function(x) cleaner(sheet_list2[[x]])))

# convert data to time series 
#data.ts <- timeSeries(data, 'Year')


# create data frame that has only states for plotting 
#states <- subset(data, data$region %in% state.name)

# create df for table 
k <- c('year', 'region', 'sum', 'public_total',  'non_public_total')
table <- data[k]
table$sum <- format(table$sum, big.mark=",")
colnames(table) <- c('Year', 'Region', 'Total HS Grads', 'Public HS Grads',  'Non-Public HS Grads')


# Shiny Application -------------------------------------------------------

server <- function(input, output) {
  
  
# Colby donation analysis -------------------------------------------------
  
output$region = renderUI({
      selectInput('selected.region', strong('Select Region'), unique(data$region))
})

output$variable = renderUI({
  selectInput("selected.var", strong('Select Variable'), colnames(data))
})


# subset data 
sub.ts <- reactive({
  # subset to only selected region
  t <- subset(data, data$region==input$selected.region)
  # convert to a zoo object, with order given by the `Year`
  #df.zoo <- with(t, zoo(input$selected.var, order.by = Year))
  df.zoo <- with(t, zoo(sum, order.by = Year))
  ts <- as.ts(df.zoo)

})

# table <- reactive({
# subset(table, table$region==input$selected.region)
# })

# data table
output$tbl = DT::renderDataTable({
           #DT::datatable(data)
           DT::datatable(table)
    })

# graph
output$dygraph <- renderDygraph({dygraph(sub.ts()) %>%
                                  dyRangeSelector(height = 20) %>%
                                   dyOptions(drawGrid = input$showgrid)})
                                 
  #dygraph(sub.ts(), main = "Predicted Deaths/Month") %>%
   # dySeries(input$selected.var, label = "Deaths")
    #dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
    #dyOptions(drawGrid = input$showgrid)



}
  

ui <- shinyUI(fluidPage(
  
titlePanel("High School Graduates Analysis"),
  
tabsetPanel(type = "tabs",
tabPanel("Region",  

         
   fluidRow(
   column(12, dygraphOutput("dygraph"))),
         
   fluidRow(
   column(3, uiOutput('region'), checkboxInput("showgrid", label = "Show Grid", value = TRUE)),
   column(9, DT::dataTableOutput('tbl')))


  

))))
  

shinyApp(ui = ui, server = server)

