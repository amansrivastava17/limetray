library(shiny)
library(ggplot2)
library(gtable)
library(grid)
library(reshape2)
library(lubridate)
library(scales)
library(googleVis)
library(devtools)


city <- c()
city$Delhi <- c("","Gurgaon","Ghaziabad","New Delhi"," New Delhi","Gurgaon ","Chittaranjan Park,"," Sainik Farms","Gurgaon<br>","Noida ","Noida","Delhi","East Delhi","Kaushambi","Gurgaon,Haryana","Greater Noida","South Delhi","NOIDA","East Delhi","North Delhi","Indirapuram","West Delhi","Ghaziabad","Central Delhi")
city$Bangalore <- c("Bangalore","Bengaluru","Koramangala","Banglore","BANGALORE","Bangalore ","BTM")
city$Pune <- c("Pune")
city$Mumbai <- c("Mumbai","Goa")

visits <- read.csv("./Data/GA_Channel.csv")
colnames(visits)<- tolower(colnames(visits))
colnames(visits)<- c("client","date","channel","user.type","device","sessions","bounces","transactions","transaction.revenue","session.duration")
visits$date <- as.Date(visits$date)
visits$week <- as.integer(strftime(visits$date,"%W"))+1

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
oo1[oo1$city_name %in% city[[1]],]$City <- names(city[1])
oo1[oo1$city_name %in% city[[2]],]$City <- names(city[2])
oo1[oo1$city_name %in% city[[3]],]$City <- names(city[3])
oo1[oo1$city_name %in% city[[4]],]$City <- names(city[4])
oo2 <- oo1
oo1 <- oo1[oo1$Day>="2015-01-01",]

user <- read.csv("./Data/Users.csv")

adwords <- read.csv("./Data/Adwords.csv")
clients <- read.csv("./Data/AdwordsClient.csv")
adwords$Day <- as.Date(adwords$Day,format="%d-%m-%Y")
adwords$Week <- as.integer(strftime(adwords$Day,"%W"))+1

shinyUI(fluidPage(
	title = 'LimeTray DashBoard',

	conditionalPanel(
		hr(),
		conditionalPanel(
			'input.dataset === "Raw Dump"'

		),
		fluidRow(
			column(5,offset=0,
						 selectInput("city",
						 						"City:",
						 						c("All"
						 							,names(city)[1],names(city)[2],names(city)[3],names(city)[4])

						 )),
			column(5,offset=0,
						 selectInput("client",
						 						"Client:",
						 						c("All"
						 							,names(sort(table(oo1$cloud_site_name),decreasing=T)))

						 )),
			column(5,offset=0,
						 selectInput("range",
						 						"Weekly/Daily:",
						 						c("Week","Day")))
			,
			column(6,
						 selectInput("usertype",
						 						"User Type:",
						 						c("All",
						 							"New"='1',"Old"='2')))
			,
			dateRangeInput('dateRange2',
										 label = paste('Select Date Range'),
										 start = min(vn1$Day)+1, end = max(vn1$Day),
										 min =  min(vn1$Day), max =  max(vn1$Day)+1,
										 separator = " - ", format = "dd/mm/yy",
										 startview = 'year', language = 'en', weekstart = 1)
		),
		mainPanel(
			tabsetPanel(
				id = 'dataset',

				tabPanel("Online Order",plotOutput('plot_oo'),
								 conditionalPanel(
								 	hr(),


								 	fluidRow(
								 		column(5,offset=0,
								 					 h4("Online Order"),
								 					 sliderInput('weekselect', 'Weeks',
								 					 						min=min(oo1$Week), max=max(oo1[oo1$Day>as.Date("2015-01-01"),]$Week)+1,
								 					 						value=c(min(oo1$Week),max(oo1[oo1$Day>as.Date("2015-01-01"),]$Week)+1),
								 					 						animate=T,round=0)
								 		),
								 		br(),
								 		column(5,offset=2,
								 					 selectInput('color_oo', 'Color',
								 					 						c(None='.', "User Type"='user_new_old',"Day of The Week"="Wday","City"="City","Hour"="Hour")),
								 					 selectInput('facet_row_oo', 'Facet Row',
								 					 						c(None='.', "User Type"='user_new_old',"Location Name"='location_name',"City"="City")),
								 					 selectInput('facet_col_oo', 'Facet Column',
								 					 						c(None='.', "User Type"='user_new_old',"Day of The Week"="Wday","Location Name"='location_name',"City"="City","Hour"="Hour"))
								 		)

								 	))),tabPanel("Virtual Number",plotOutput('plot'),conditionalPanel(
								 		hr(),
								 		fluidRow(
								 			column(5,offset=0,
								 						 h4("Virtual Number"),
								 						 sliderInput('weekselect', 'Weeks',
								 						 						min=min(vn1$Week), max=max(vn1[vn1$Day>="2015-01-01",]$Week)+1,
								 						 						value=c(min(vn1$Week),max(vn1[vn1$Day>="2015-01-01",]$Week)),
								 						 						animate=T,round=0)
								 			),
								 			br(),
								 			column(5,offset=2,
								 						 selectInput('color_vn', 'Color',
								 						 						c(None='.', "User Type"='user_new_old',"Week of The Day"="Wday","City"="city","Call Status"='call_status')),
								 						 selectInput('facet_row_vn', 'Facet Row',
								 						 						c(None='.', "User Type"='user_new_old',"Call Status"='call_status',"City"="city")),
								 						 selectInput('facet_col_vn', 'Facet Column',
								 						 						c(None='.', "User Type"='user_new_old',"Week of The Day"="Wday","Call Status"='call_status',"City"="city"))
								 			)

								 		))),
				tabPanel("Adwords",plotOutput('plot_adwords'),	conditionalPanel(
					hr(),fluidRow(
						column(6,
									 selectInput("network",
									 						"Network Type:",
									 						c("All",
									 							unique(as.character(adwords$Network))))
						),column(6,
										 selectInput("bar",
										 						"BAR:",
										 						c("Cost","Impressions","Clicks","Conversions"))
						),column(6,
										 selectInput("line",
										 						"Line:",
										 						c("Impressions","Clicks","Conversions","Cost"))
						)
					))),tabPanel("Client Level",plotOutput('pieoo'),
											 plotOutput('basicoo'),plotOutput('locationoo'),
											 plotOutput('wdayoo'),plotOutput('timeoo'),
											 plotOutput('locationtimeoo'),plotOutput('pievn'),
											 plotOutput('basicvn'),plotOutput('basicvncall'),plotOutput('wdayvn'),
											 plotOutput('timevn'),
											 conditionalPanel(
											 	hr(),
											 	br()


											 )),
				tabPanel('GA',htmlOutput("dashboard")),
				tabPanel('User Health Analysis', plotOutput('user_base')
								 ,plotOutput('user_rep'),plotOutput('user_rep2'))
			)
		)
	)
)
)
