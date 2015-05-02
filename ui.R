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
						 							,names(sort(table(client_name_city),decreasing=T)))

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
