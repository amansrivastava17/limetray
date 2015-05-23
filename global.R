library(shiny)
library(ggplot2)
library(gtable)
library(grid)
library(reshape2)
library(lubridate)
library(scales)
library(googleVis)
library(devtools)
library(plotly)
py<-plotly()
set_credentials_file("amans.limetray", "zwyetvuzsm")
#This script is necessary in the app folder 
#but the user should not have to edit it

#Output Graph Function
graphOutput <- function(inputId, width="100%", height="550px") {
  tagList(
    singleton(tags$head(
      tags$script(src="plotlyGraphWidget.js")
    )),
    tags$iframe(id=inputId, src="https://plot.ly/~playground/7.embed",
                class="graphs", style="border:none;", seamless=TRUE, width=width, height=height)
  )
}

#Function to change ggplot figure into plotly syntax
#gg is the users ggplot
#called in the server script to return data and layout info separately 
gg2fig <- function(gg) {
  fig <- gg2list(gg)
  data <- list()
  for(i in 1:(length(fig)-1)){data[[i]]<-fig[[i]]}
  #data$kwargs$filename <- 'facet_grid'
  layout <- fig$kwargs$layout
  result <- list(data=data,layout=layout)
  return(result)
} 

renderGraph <- function(expr, env=parent.frame(), quoted=FALSE) {
  ## This gets called when inputs change --
  ## Place data wrangling code in here
  ## and pass the result to the client
  ## to be graphed.
  
  
  installExprFunction(expr, "func", env, quoted)
  
  function(){
    data = func();
    ## data is the state of the widgets: see server.R
    ## this function returns a list of named lists that descripe
    ## valid postMessage commands to be sent to the embedded iframe.
    ## see binding.renderValue for the receiving JS side of this function
    ## and https://github.com/plotly/Embed-API for more about the postMessage
    ## graph messages
    return(data)
    
  }
}


city <- c()
city$Delhi <- c("","Gurgaon","Ghaziabad","New Delhi"," New Delhi","Gurgaon ","Chittaranjan Park,"," Sainik Farms","Gurgaon<br>","Noida ","Noida","Delhi","East Delhi","Kaushambi","Gurgaon,Haryana","Greater Noida","South Delhi","NOIDA","East Delhi","North Delhi","Indirapuram","West Delhi","Ghaziabad","Central Delhi")
city$Bangalore <- c("Bangalore","Bengaluru","Koramangala","Banglore","BANGALORE","Bangalore ","BTM")
city$Pune <- c("Pune")
city$Mumbai <- c("Mumbai","Goa")

vn_name <- paste("./Data/vn_2015-04-22",".csv",sep="")
vn1 <- read.csv(vn_name)
vn1$Day <- as.Date(vn1$call_details_added)
vn1 <- vn1[is.na(vn1$Day)==F,]
vn1$Hour <- as.factor(hour(vn1$call_details_added))
vn1$Week <- as.integer(strftime(vn1$Day,"%W"))+1
vn1$user_new_old <- as.factor(vn1$user_new_old)
vn1$Wday <- wday(vn1$Day,label=T,abbr=F)
vn1 <- vn1[vn1$cloud_site_name!="Cafe Galley",]
vn1 <- vn1[vn1$call_status!="__undefined__",]
vn1$City <- "New City"
vn1[vn1$city %in% city[[1]],]$City <- names(city[1])
vn1[vn1$city %in% city[[2]],]$City <- names(city[2])
vn1[vn1$city %in% city[[3]],]$City <- names(city[3])
vn1[vn1$city %in% city[[4]],]$City <- names(city[4])
vn2 <- vn1
vn1 <- vn1[vn1$Day>="2015-01-01",]
vn1$cloud_site_name<-as.character(vn1$cloud_site_name)
visits <- read.csv("./Data/GA_Channel.csv")
colnames(visits)<- tolower(colnames(visits))
colnames(visits)<- c("client","date","channel","user.type","device","sessions","bounces","transactions","transaction.revenue","session.duration")
visits$date <- as.Date(visits$date)
visits$week <- as.integer(strftime(visits$date,"%W"))+1

oo_name <- paste("./Data/oo_2015-04-22",".csv",sep="")
oo1 <- read.csv(oo_name)
oo1$Day <- as.Date(oo1$order_added)
oo1 <- oo1[is.na(oo1$Day)==F,]
oo1$Hour <- as.factor(hour(oo1$order_added))
oo1$Week <- as.integer(strftime(oo1$Day,"%W"))+1
oo1$user_new_old <- as.factor(oo1$user_new_old)
oo1$Wday <- wday(oo1$Day,label=T,abbr=F)
oo1 <- oo1[oo1$cloud_site_name!="Cafe Galley",]
oo1$City <- "New City"
oo1$cloud_site_name<-as.character(oo1$cloud_site_name)
oo1[oo1$city_name %in% city[[1]],]$City <- names(city[1])
oo1[oo1$city_name %in% city[[2]],]$City <- names(city[2])
oo1[oo1$city_name %in% city[[3]],]$City <- names(city[3])
oo1[oo1$city_name %in% city[[4]],]$City <- names(city[4])
oo2 <- oo1
oo1 <- oo1[oo1$Day>="2015-01-01",]

adwords <- read.csv("./Data/Adwords.csv")
clients <- read.csv("./Data/AdwordsClient.csv")
adwords$Day <- as.Date(adwords$Day)
adwords$Week <- as.integer(strftime(adwords$Day,"%W"))+1

user <- read.csv("./Data/Users.csv")
client_name_city <-oo1['cloud_site_name']
