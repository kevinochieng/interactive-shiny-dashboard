# id - Restaurant ID
# address - Address
# categories - Restauranes Categories
# city - City
# country - </b>Country
# keys - Key Words
# latitude - Latitude
# longitude - Longitude
# menuPageURL - Restaurant page URL
# menus.amountMax - Maximum menu price
# menus.amountMin - Minimum menu price
# menus.currency - Currency accepted
# menus.dateSeen - Date the menu was viewed
# menus.description - Description of the menu, ingredients
# menus.name - Pizza Name
# name - Restaurant Name
# postalCode - Restaurant PostCode
# priceRangeCurrency - Currency Type
# priceRangeMin - Range of minimum pizza variation
# priceRangeMax - Maximum range of pizza variation
# province - State / Province
library("dplyr")
library(readr)
pizadata <- read_csv("C:/Users/User/Desktop/BI TOOLS/pizadata.csv", 
                     col_types = cols(menus.amountMax = col_number(), 
                                      menus.amountMin = col_number()))
#head(pizadata)
#shape
#dim(pizadata)
#select relevant columns using select method
x <- select(pizadata,id,address,city,latitude,longitude,menus.amountMax,menus.amountMin,menus.name,name,
            priceRangeMin,priceRangeMax,province)
#is.na(x)
#sum of missing values
#sum(is.na(x))

#select only id and menus.name
y <- select(x,id,menus.name)
#y
set.seed(123)
menus <- y %>%
  arrange( id,menus.name) %>%
  group_by(id) %>%
  mutate(COUNTER = 1:n()) %>%  ## as per comment, can use row_number()
  ungroup()
#View(menus)
#export the data to excel
#write.csv(menus, "C:/Users/User/Desktop/BI TOOLS/menus.csv") 
#menus %>% 
#  summary(COUNTER)
#arrange based on counter and drop the id
menus_id <- select(menus,menus.name,COUNTER)
df_order <- menus_id %>% 
  arrange(desc(COUNTER))
#df_order
#remove duplicate in a column
duplicated(df_order)
df_order[!duplicated(df_order$menus.name), ]





# #filter when count >59 and plot the result 
df1<-filter(df_order,COUNTER >=59)
#View(df1)
#typeof(df1$COUNTER)
library(ggplot2)
p01 <-top_n(df_order, n=10, COUNTER) %>%
  ggplot(., aes(x=reorder(menus.name, -COUNTER), y=COUNTER,fill= menus.name))+
  geom_bar(stat='identity')+ theme_minimal() + xlab("Pizza types") + ylab("Count") +
  ggtitle("10 most popular pizzas")
#p01

#pizza bugger locations
#load the data
#x

#select raw name pizza bugger
bugger <-filter(x,menus.name == "Pizza Burger")
bugger1 <- select(bugger,city,latitude,longitude)
#View(bugger1)
#Get the world map country border points

#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
#install.packages(c("maps", "mapdata"))

usa <- map_data("usa")
usa <- map_data("usa") # we already did this, but we can do it again
gg1 <-ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group),fill = "violet" ,color="blue") + 
  coord_fixed(1.3)

gg2<-gg1 +
  geom_point(data = bugger1, aes(x = longitude, y = latitude), color = "yellow", size = 4)
#ggplotly(gg2)


#locations of white pizza

white <-filter(x,menus.name == "White Pizza")
white1 <- select(white,city,latitude,longitude)
#View(white1)
usa <- map_data("usa") # we already did this, but we can do it again
gg <-ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group),fill = "blue" ,color="red") + 
  coord_fixed(1.3)
#gg
ggwhite<-gg +
  geom_point(data = white1, aes(x = longitude, y = latitude), color = "black", size = 4)

#ggplotly(ggwhite)

#location of vegeterian pizza
veg <-filter(x,menus.name == "Veggie Pizza")
Veg1 <- select(veg,city,latitude,longitude)
#View(white1)
usa <- map_data("usa") # we already did this, but we can do it again
g <-ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group),fill = "blue" ,color="red") + 
  coord_fixed(1.3)
#g
ggveg<-g +
  geom_point(data = Veg1, aes(x = longitude, y = latitude), color = "yellow", size = 4)

#ggplotly(ggveg)
#location of breakfirst pizza
brek <-filter(x,menus.name == "Breakfast Pizza")
brek1 <- select(brek,city,latitude,longitude)
#View(white1)
#usa <- map_data("usa") # we already did this, but we can do it again
gl <-ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group),fill = "blue" ,color="red") + 
  coord_fixed(1.3)
#g
ggbrek<-gl +
  geom_point(data = brek1, aes(x = longitude, y = latitude), color = "yellow", size = 4)

#ggplotly(ggveg)

#CITIES
#CITIES WITH MORE FLAVOURS OF PIZZAS
#View(x)
#select cities with menus.name
sot <-select(x,city,menus.name)
set.seed(123)
sot1 <- sot %>%
  arrange( city,menus.name) %>%
  group_by(city) %>%
  mutate(COUNTER = 1:n()) %>%  ## as per comment, can use row_number()
  ungroup()
#View(sot1)
#export the data to excel
#write.csv(menus, "C:/Users/User/Desktop/BI TOOLS/menus.csv") 
library(dplyr)
sort2 <-sot1 %>% 
  group_by(city) %>% 
  summarise(COUNTER = sum(COUNTER))
#View(sort2)
fil <- sort2 %>% 
  arrange(desc(COUNTER))

#df_order
fil1 <- filter(fil,COUNTER>=3916)
#View(fil1)
#plot the graph
p<-ggplot(fil1, aes(x=reorder(city, -COUNTER), y=COUNTER, fill=city)) +
  geom_bar(stat="identity")+theme_minimal()
#ggplotly(p)



#most popular pizzas in philelphic
#View(x)
#select menus.name and city phil....
phil <-select(x,city,menus.name)
phil1 <-filter(phil,city =="Philadelphia")
#View(phil1)
#select the menus only 
menusonly <-select(phil1,menus.name)
#View(menusonly)
library(data.table)
table(menusonly$menus.name)
menusonly1 <-as.data.frame(table(menusonly$menus.name)) 

#filter where freq >=3
menusonly1 <- filter(menusonly1,Freq >=2)

#rename the column 
menusonly1 <- menusonly1 %>% rename(flavors = Var1) 
#View(menusonly1)
#now i can plot 
ggphil<-ggplot(menusonly1, aes(x=reorder(flavors,-Freq), y=Freq, fill=flavors)) +
  geom_bar(stat="identity")+theme_minimal()
#ggphil
#ggplotly(ggphil)



# STATES
#States with more pizza options/province
#select states and pizza names
states <- select(x,province,menus.name)
#rename the column  menus.name to flavors
states <- states %>% rename(flavors = menus.name) 
#View(states)
#group flavors by province/states

set.seed(123)
groupedstates <- states %>%
  arrange( province,flavors) %>%
  group_by(province) %>%
  mutate(COUNTER = 1:n()) %>%  ## as per comment, can use row_number()
  ungroup()
#View(groupedstates)
#library(dplyr)
groupedstates1 <-groupedstates %>% 
  group_by(province) %>% 
  summarise(COUNTER = sum(COUNTER))
#View(groupedstates1)
statefilter <- filter(groupedstates1,COUNTER>=34453)
#View(statefilter)
#plot the graph
stategg<-ggplot(statefilter, aes(x=reorder(province,-COUNTER), y=COUNTER, fill=province)) +
  geom_bar(stat="identity")+theme_minimal()
#stategg
#ggplotly(stategg)





#PRICES
#A. MINIMUM PIZZA PRICE PER STATE
#select states/provinces and minimum pizza prices
#View(x)
Minprice <- select(x,province,menus.amountMin)
#View(Minprice)
#replacing 0s with the mean value by first rplacing 0s to NA
Minprice[Minprice == 0] <- NA
Minprice<- Minprice %>%
  mutate_if(is.numeric, zoo::na.aggregate)
#View(Minprice)
Minprice <-Minprice %>% 
  group_by(province) %>% 
  summarise(SUM = sum(menus.amountMin))

Minfilter <- filter(Minprice,SUM<=87)
#View(Minfilter)
#plot the graph
Mingg<-ggplot(Minfilter, aes(x=reorder(province,-SUM), y=SUM, fill=province)) +
  geom_bar(stat="identity")+theme_minimal()
#Mingg
#ggplotly(Mingg)





#price part b
#MAX PIZZA PRICE PER STATE
#select states/provinces and Max pizza prices
#View(x)
Maxprice <- select(x,province,menus.amountMax)
#View(Minprice)
#replacing 0s with the mean value by first rplacing 0s to NA
Maxprice[Maxprice == 0] <- NA
Maxprice<- Maxprice %>%
  mutate_if(is.numeric, zoo::na.aggregate)
#create a counter column group by province 

Maxprice <-Maxprice %>% 
  group_by(province) %>% 
  summarise(SUM = sum(menus.amountMax))
#View(Maxprice)
Maxfilter <- filter(Maxprice,SUM>=3648)
#View(Maxfilter)
#plot the graph
Maxgg<-ggplot(Maxfilter, aes(x=reorder(province,-SUM), y=SUM, fill=province)) +
  geom_bar(stat="identity")+theme_minimal()
#Maxgg
#ggplotly(Maxgg)
carss<- select(x,menus.amountMax,menus.amountMin) 
data <-carss %>% 
  rename(
    Maxx = menus.amountMax,
    Minn = menus.amountMin
  )
#View(data)
data1<- data[data$Maxx <= 40,]



library(maps)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(datasets)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(datasets)
library(dplyr)
library(tidyverse)

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Pizza Restaurant Analysis",
                                    titleWidth = 450),
                    #sidebar content 
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Descriptive starts", tabName = "Descriptivestatistics", icon = icon("bar-chart-o")),
                        menuItem("Charts", tabName = "Charts", icon = icon("dashboard")),
                        menuItem("Insights", tabName = "Insights", icon = icon("table"))
                       # menuItem("Prices",tabName = "Prices", icon = icon("money"))
                      ),
                      #searchbar
                      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                        label = "Search...")
                      
                    ),
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "Descriptivestatistics",
                               navbarPage("Statistics!",
                                          tabPanel("Plot",
                                                   sidebarLayout(
                                                      sidebarPanel(
                                                        radioButtons("plotType", "Plot type",
                                                           c("Scatter"="p", "Line"="l"))
                                                      ),
                                                      mainPanel(
                                                        plotOutput("plot")
                                                      )
                                                     
                                                    )
                                                   ),
                                          tabPanel("Summary",
                                                  fluidPage(
                                                    titlePanel("Shiny text"),
                                                    sidebarLayout(
                                                      #sidebar panel for inputs
                                                      sidebarPanel(
                                                        #input selectors for choosing dataset
                                                        selectInput(
                                                          inputId = "dataset",
                                                          label = "Choose a numeric column:",
                                                          choices = c("Amountmax","Amountmin")
                                                        ),
                                                        numericInput(inputId = "obs",
                                                                     label = "Number of observations to view:",
                                                                     value = 10)
                                                      ),
                                                      #main panel for displaying outputs
                                                      mainPanel(
                                                        #output: verbatim text for data summary...
                                                        verbatimTextOutput("summary"),
                                                        #output html table with requested no of observations
                                                        tableOutput("View")
                                                      )
                                                    )
                                                  ) 
                                                   
                                                   #https://shiny.rstudio.com/gallery/navbar-example.html
                                                  # verbatimTextOutput("summary")
                                                   ),
                                          tabPanel("Table",
                                               fluidPage(
                                                 fluidRow(
                                                   title = "Pizza Restaurant table",
                                                   column(4,
                                                          selectInput("add",
                                                                      "address:",
                                                                      c("All",
                                                                        unique(as.character(x$address))))),
                                                   column(4,
                                                          selectInput("cit",
                                                                      "city:",
                                                                      c("All",
                                                                        unique(as.character(x$city))))),
                                                   column(4,
                                                          selectInput("menus",
                                                                      "menus.name:",
                                                                      c("All",
                                                                        unique(as.character(x$menus.name))))),
                                                   column(4,
                                                          selectInput("nam",
                                                                      "name:",
                                                                      c("All",
                                                                        unique(as.character(x$name))))),
                                                   column(4,
                                                          selectInput("prov",
                                                                      "province:",
                                                                      c("All",
                                                                        unique(as.character(x$province)))))
                                                   
                                                   
                                                 ),
                                                 DT::dataTableOutput("table")
                                               )    
                                                   
                                                   ),
                                          tabPanel("More",
                                                     tabPanel("Table"
                                                             # DT::dataTableOutput("table")
                                                              )
                                                   ) )
                               ),
                              
                       
                        
                        # Second tab content
                        tabItem(tabName = "Charts",
                                navbarPage("Charts",
                                         tabPanel("flavors",
                                            fluidRow(
                                              box(
                                                title = "10 Most popular pizzas", height = 600, status = "primary",
                                                plotOutput("p01")
                                                # verbatimTextOutput("summary"),
                                                # 
                                                # tableOutput("view")
                                                # 
                                              ),
                                              #body parnel to be navigated
                                              box(
                                                title = "Pizza Burger locations",  height = 600, status = "primary",
                                                plotlyOutput("gg2")
                                                # selectInput("dataset", "Choose type of pizza:", 
                                                #             choices = c("rock", "pressure", "cars")),
                                                # 
                                                # numericInput("obs", "Number of observations to view:", 10)
                                              ),
                                              box(
                                                title = "White pizzas location",height = 600 ,
                                                plotlyOutput("ggwhite")
                                                #plotOutput("plot1", height = 300)
                                              ),
                                              box(
                                                title = "Locations of Veggie Flavour pizzas",height = 600,
                                                plotlyOutput("ggveg")
                                                
                                              ),
                                              
                                              box(
                                                title = "Vegeterian pizza Locations",height = 600,
                                                plotlyOutput("ggbrek")
                                                # "This slider controls",br(),
                                                # "No of bins",
                                                # sliderInput("slider", "Slider input:", 1, 100, 50),
                                                # #put a text input
                                                # textInput("text","Text Text input:")
                                              )
                                            )),
                                         tabPanel("Cities",
                                                fluidRow(
                                                  box(
                                                    title = "Cities with more flavours of Pizza",height = 600,
                                                    plotlyOutput("p")
                                                    
                                                  ),
                                                  box(
                                                    title = "Flavors with more options in Philedelphia",height =600,
                                                    plotlyOutput("ggphil")
                                                  )
                                                )),
                                         tabPanel("States",
                                                fluidRow(
                                                  box(
                                                    title ="States with more pizza options", height = 600,
                                                    plotlyOutput("stategg")
                                                  )
                                                )),
                                         tabPanel("Prices",
                                                fluidRow(
                                                  box(
                                                    title = "Minimum Pizza price per state",height = 600,
                                                    plotlyOutput("Mingg")
                                                  ),
                                                  box(
                                                    title = "Max pizza price per state",height = 600,
                                                    plotlyOutput("Maxgg")
                                                  )
                                                )))
                                ),
                               
                        
                        #3rd tab content
                        tabItem(tabName = "Insights" )
                              
                      
                      )
                    )
)
server <- function(input,output) {
  output$plot <- renderPlot({
    #graphics.off()
    #par("mar") 
    #par(mar=c(1,1,1,1))
    #dev.off()
    par(mar = rep(2, 4))
    plot(data1, type=input$plotType)
  
    #plot()
  })
 # output$summary <- renderPrint({
   # summary(data)
 # })
  #return the requested datase
    datasetInput <- reactive({
      switch(input$dataset,
             "Amountmax" = x$menus.amountMax,
             "Amountmin" = x$menus.amountMin
             )
    })
    #generate a summary of the dataset
    output$summary <- renderPrint({
      dataset <- datasetInput()
      summary(dataset)
    
    })
    #show the first n observations....
    output$View <- renderTable({
      head(datasetInput(),n=input$obs)
    })
  output$table <- DT::renderDataTable(DT::datatable({
    data <- x
    if (input$add != "All") {
      data <- data[data$address == input$add,]
    }
    if (input$cit != "All"){
      data <- data[data$city == input$cit,]
    }
    if (input$menus != "All"){
      data <- data[data$menus.name == input$menus,]
    }
    if (input$nam  !="All"){
      data <- data[data$name == input$nam,]
    }
    if (input$prov != "All"){
      data <- data[data$province == input$prov,]
    }
    data
  }))
  output$p01 <- renderPlot({
    
    p01 <-top_n(df_order, n=10, COUNTER) %>%
      ggplot(., aes(x=menus.name, y=COUNTER,fill= menus.name,height = 500))+
      geom_bar(stat='identity')+ theme_minimal() + xlab("Pizza types") + ylab("Count") +
      ggtitle("10 most popular pizzas") 
    p01
    
  })
  
  output$gg2 <- renderPlotly({
    gg2<-gg1 +
      geom_point(data = bugger1, aes(x = longitude, y = latitude), color = "yellow", size = 4)
    
    gg2<-plotly_build(gg2)
    gg2
    
  })
  output$ggwhite <- renderPlotly({
    ggwhite<-gg +
      geom_point(data = white1, aes(x = longitude, y = latitude), color = "black", size = 4)
    
    ggwhite <- plotly_build(ggwhite)
    ggwhite
  })
  output$ggveg <-renderPlotly({
    ggveg<-g +
      geom_point(data = Veg1, aes(x = longitude, y = latitude), color = "yellow", size = 4)
    ggveg <- plotly_build(ggveg)
    ggveg
  })
  output$ggbrek <- renderPlotly({
    ggbrek<-gl +
      geom_point(data = brek1, aes(x = longitude, y = latitude), color = "yellow", size = 4)
    ggbrek <- plotly_build(ggbrek)
    ggbrek
  })
  output$p <-renderPlotly({
    p<-ggplot(fil1, aes(x=reorder(city, -COUNTER), y=COUNTER, fill=city)) +
      geom_bar(stat="identity")+theme_minimal()
    p <- plotly_build(p)
    p
  })
  output$ggphil <- renderPlotly({
    ggphil<-ggplot(menusonly1, aes(x=reorder(flavors,-Freq), y=Freq, fill=flavors)) +
      geom_bar(stat="identity")+theme_minimal()
    ggphil <- plotly_build(ggphil)
    ggphil
  })
  output$stategg <- renderPlotly({
    stategg<-ggplot(statefilter, aes(x=reorder(province,-COUNTER), y=COUNTER, fill=province)) +
      geom_bar(stat="identity")+theme_minimal()
    stategg <- plotly_build(stategg)
    stategg
  })
  output$Mingg <- renderPlotly({
    Mingg<-ggplot(Minfilter, aes(x=reorder(province,-SUM), y=SUM, fill=province)) +
      geom_bar(stat="identity")+theme_minimal()
    Mingg <- plotly_build(Mingg)
    Mingg
  })
  output$Maxgg <- renderPlotly({
    Maxgg<-ggplot(Maxfilter, aes(x=reorder(province,-SUM), y=SUM, fill=province)) +
      geom_bar(stat="identity")+theme_minimal()
    Maxgg <- plotly_build(Maxgg)
    Maxgg
  })
  
  # set.seed(122)
  # #load the dataset
  # histdata <- rnorm(500)
  # output$plot1 <- renderPlot({
  #   #input slider on the hist data
  #   data <- histdata[seq_len(input$slider)]
  #   hist(data)
  # })
  # output$messageMenu <- renderMenu({
  #   # Code to generate each of the messageItems here, in a list. This assumes
  #   # that messageData is a data frame with two columns, 'from' and 'message'.
  #   msgs <- apply(messageData, 1, function(row) {
  #     messageItem(from = row[["from"]], message = row[["message"]])
  #   })
  #   
  #   # This is equivalent to calling:
  #   #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)...was a comment
  #   dropdownMenu(type = "messages", .list = msgs)
  # })
  # #return the requested dataset
  # datasetInput <- reactive({
  #   switch(input$dataset,
  #          "rock"= rock,
  #          "pressure"= pressure,
  #          "cars = cars")
  # })
  # #generate a summary of the dataset
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })
  # #show the first "n" observations
  # output$view <- renderTable({
  #   head(datasetInput(), n= input$obs)
  # })
  
}


shinyApp(ui, server)

