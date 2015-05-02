

shinyServer(function(input, output,session) {

	output$dashboard<- renderGvis({


		aa <- dcast(visits,client+date~.,sum,value.var="sessions")
		bb <- dcast(visits,client+date~.,sum,value.var="transactions")
		cc <- merge(aa,bb,by=c("client","date"))
		names(cc) <- c("Client","Date","Sessions","Transactions")
		Motion=gvisMotionChart(cc,
													 idvar="Client",
													 timevar="Date")
		Motion
	})
  

	# a large table, reative to input$show_vars
	city1<-reactive({
      if(input$city=="All"){
        client_name_city <-oo1['cloud_site_name']
      }
      else{
	    client_name_city <<- subset(oo1,City==as.character(input$city))['cloud_site_name']}
	    #print(table(client_name_city))
	    updateSelectInput(session,"client",
	                      label="Client:",
	                      choices=c("All",names(sort(table(client_name_city),decreasing=T))))
	  })
  change_event<-function(df1){
	  
	  df.change <- df1[df1$Day >=input$dateRange2[1]&df1$Day <=input$dateRange2[2],]
	  if(input$city!="All")
	  {
	    city1()
	    c <- city[names(city)==input$city]
	    df.change <- df.change[df.change$city %in% c[[1]],]  
	  }
	  
	  if (input$city!="All"&input$client != "All"&input$usertype != "All"){
	    
	    df.change <- df.change[df.change$cloud_site_name == as.character(input$client)&df.change$user_new_old == as.character(input$usertype),]
	  }
	  if (input$city!="All"&input$client != "All"&input$usertype == "All"){
	    df.change <- df.change[df.change$cloud_site_name == as.character(input$client),]
	  }
	  if (input$city!="All"&input$usertype != "All"&input$client == "All"){
	    df.change <- df.change[df.change$user_new_old == as.character(input$usertype),]
	  }
	  
	  if(input$city=="All")
	  {	city1()
      if (input$client != "All"&input$usertype != "All"){
	    df.change <- df.change[df.change$cloud_site_name == as.character(input$client)&df.change$user_new_old == as.character(input$usertype),]
	  }
	  if (input$client != "All"&input$usertype == "All"){
	    df.change <- df.change[df.change$cloud_site_name == as.character(input$client),]
	  }
	  if (input$usertype != "All"&input$client == "All"){
	    df.change <- df.change[df.change$user_new_old == as.character(input$usertype),]
	  }
	  }
    print(df.change[1:1,])
	  df.change
	}
  output$plot <- renderPlot({
		vn12 <- change_event(vn1)

		data <- vn12[vn12$Week>=input$weekselect[1]&vn12$Week<=input$weekselect[2],]
		if(input$range=="Week")
		{
			p <- ggplot(data, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders")
		}
		if(input$range=="Day")
		{
			p <- ggplot(data, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders")
		}

		if(input$color_vn!='.')
		{if(input$color_vn=='user_new_old')
		{p <- p + aes(fill=user_new_old) + scale_fill_manual(values=c("black", "red"),
																												 name="User Type",breaks=c("1", "2"),
																												 labels=c("New User", "Old User"))
		}
		if(input$color_vn!='user_new_old')
		{str(input$color_vn)

		 p <- p + aes_string(fill=input$color_vn)
		}
		}
		facets <- paste(input$facet_row_vn, '~', input$facet_col_vn)
		if (facets != '. ~ .')
			p <- p + facet_grid(facets)
      p
	})


  output$plot_oo <- renderPlot({
		
    oo12<-change_event(oo1)
		data <- oo12[oo12$Week>=input$weekselect[1]&oo12$Week<=input$weekselect[2],]
		if(input$range=="Week")
		{
			p <- ggplot(data, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders")
		}
		if(input$range=="Day")
		{
			p <- ggplot(data, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders")
		}

		if(input$color_oo!='.')
		{if(input$color_oo=='user_new_old')
		{p <- p + aes(fill=user_new_old) + scale_fill_manual(values=c("black", "red"),
																												 name="User Type",breaks=c("1", "2"),
																												 labels=c("New User", "Old User"))
		}
		if(input$color_oo!='user_new_old')
		{str(input$color_oo)

		 p <- p + aes_string(fill=input$color_oo)
		}
		}
		facets <- paste(input$facet_row_oo, '~', input$facet_col_oo)
		if (facets != '. ~ .')
			p <- p + facet_grid(facets)

      p
	})
	output$plot_adwords <- renderPlot({
		adwords2 <- adwords
		campaign <- "All"
		if(input$client!="All")
		{	campaign <- clients[clients$Client %in% as.character(input$client),]$Campaign}
		if (campaign != "All"&input$network != "All"){
			adwords2 <- adwords[adwords$Campaign %in% as.character(campaign)&adwords$Network == as.character(input$network),]
		}
		if (campaign != "All"&input$network == "All"){
			adwords2 <- adwords[adwords$Campaign %in% as.character(campaign),]
		}
		if (input$network != "All"&campaign == "All"){
			adwords2 <- adwords[adwords$Network == as.character(input$network),]
		}

		data2 <- adwords2[adwords2$Day >= input$dateRange2[1]&adwords2$Day <= input$dateRange2[2],]
		data <- data2[data2$Week>=input$weekselect[1]&data2$Week<=input$weekselect[2],]
		print(head(adwords))
		# 		p <- ggplot(data, aes(x=factor(Week),y=Cost)) + geom_bar(stat="identity",position=position_dodge(),
		# 																													 colour="black") + xlab("Weeks") + ylab("Total Spend")
		#
		# 		p <- p + aes(fill=Network) + scale_fill_manual(values=c("#999999", "green"))
		#
		#
		# 		facets <- paste(input$facet_row, '~', input$facet_col)
		# 		if (facets != '. ~ .')
		# 			p <- p + facet_grid(facets)
		#
		aa <- dcast(data,Week+Network~.,sum,value.var=paste(input$bar))
		bb <- dcast(data,Week+Network~.,sum,value.var=paste(input$line))
		df <- merge(aa,bb,by=c("Week","Network"))
		if(input$range!="Week")
		{
			aa <- dcast(data,Day+Network~.,sum,value.var=paste(input$bar))
			bb <- dcast(data,Day+Network~.,sum,value.var=paste(input$line))
			df <- merge(aa,bb,by=c("Day","Network"))

		}
		names(df) <- c(paste(input$range),"Network",paste(input$bar),paste(input$line))

		grid.newpage()

		# two plots
		p1 <- ggplot(df, aes_string(input$range, input$bar)) + geom_bar(stat="identity") + theme_bw()
		p1 <- p1 + aes(fill=Network) + scale_fill_manual(values=c("#999999", "green"))
		p2 <- ggplot(df, aes_string(input$range, input$line)) + geom_line(colour = "red") + theme_bw() %+replace%
			theme(panel.background = element_rect(fill = NA))

		# extract gtable
		g1 <- ggplot_gtable(ggplot_build(p1))
		g2 <- ggplot_gtable(ggplot_build(p2))

		# overlap the panel of 2nd plot on that of 1st plot
		pp <- c(subset(g1$layout, name == "panel", se = t:r))
		g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
												 pp$l, pp$b, pp$l)

		# axis tweaks
		ia <- which(g2$layout$name == "axis-l")
		ga <- g2$grobs[[ia]]
		ax <- ga$children[[2]]
		ax$widths <- rev(ax$widths)
		ax$grobs <- rev(ax$grobs)
		ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
		g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
		g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

		# draw it
		grid.draw(g)

	})
	output$pieoo <- renderPlot({
		oo12 <- oo1[oo1$Day >=input$dateRange2[1]&oo1$Day <=input$dateRange2[2],]
		oo12 <- oo12[oo12$cloud_site_name == as.character(input$client),]
		try(pie(table(oo12$user_new_old),labels=c("New User","Old User"), main="User Distribution Online Order"))
	})
	output$basicoo <- renderPlot({
		oo12 <- oo1[oo1$Day >=input$dateRange2[1]&oo1$Day <=input$dateRange2[2],]
		oo12 <- oo12[oo12$cloud_site_name == as.character(input$client),]
		if(input$range=="Week")
		{
			p <- ggplot(oo12, aes(x=factor(Week))) + geom_bar(colour="green") + xlab("Weeks") + ylab("Total Orders") + ggtitle("Weekwise Distribution - Online Orders")
		}
		if(input$range=="Day")
		{
			p <- ggplot(oo12, aes(x=factor(Day))) + geom_bar(colour="green") + xlab("Days") + ylab("Total Orders") + ggtitle("Weekwise Distribution - Online Orders")
		}

		p <- p + aes(fill=user_new_old) + scale_fill_manual(values=c("green", "grey"),name="User Type",breaks=c("1", "2"),
																												labels=c("New User", "Old User"))
		try(print(p))
	})
	output$locationoo <- renderPlot({
		oo12 <- oo1[oo1$Day >=input$dateRange2[1]&oo1$Day <=input$dateRange2[2],]
		oo12 <- oo12[oo12$cloud_site_name == as.character(input$client),]
		if(length(unique(oo12$location_name))>1)
		{
			if(input$range=="Week")
			{
				p <- ggplot(oo12, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders") + ggtitle("Locationwise Weekly Distribution - Online Orders")
			}
			if(input$range=="Day")
			{
				p <- ggplot(oo12, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders") + ggtitle("Locationwise Weekly Distribution - Online Orders")
			}

			facets <- paste("location_name",'~.')
			p <- p + aes(fill=user_new_old) + scale_fill_manual(values=c("green", "grey"),name="User Type",breaks=c("1", "2"),
																													labels=c("New User", "Old User")) + facet_grid(facets)
			try(print(p))
		}
	})
	output$wdayoo <- renderPlot({
		oo12 <- oo1[oo1$Day >=input$dateRange2[1]&oo1$Day <=input$dateRange2[2],]
		oo12 <- oo12[oo12$cloud_site_name == as.character(input$client),]
		if(input$range=="Week")
		{
			p <- ggplot(oo12, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders") + ggtitle("Day of The Week - Online Orders")
		}
		if(input$range=="Day")
		{
			p <- ggplot(oo12, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders") + ggtitle("Day of The Week - Online Orders")
		}
		facets <- paste('.~',"Wday",sep="")
		p <- p + aes(fill=Wday) + facet_grid(facets)
		try(print(p))
	})
	output$timeoo <- renderPlot({
		oo12 <- oo1[oo1$Day >=input$dateRange2[1]&oo1$Day <=input$dateRange2[2],]
		oo12 <- oo12[oo12$cloud_site_name == as.character(input$client),]
		if(input$range=="Week")
		{
			p <- ggplot(oo12, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders") + ggtitle("Time of Orders - Online Orders")
		}
		if(input$range=="Day")
		{
			p <- ggplot(oo12, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders") + ggtitle("Time of Orders - Online Orders")
		}

		facets <- paste("Time",'~.')
		p <- p + aes(fill=user_new_old) + scale_fill_manual(values=c("green", "grey"),name="User Type",breaks=c("1","2"),
																												labels=c("New User", "Old User")) + facet_grid(facets)
		try(print(p))
	})
	output$locationtimeoo <- renderPlot({
		oo12 <- oo1[oo1$Day >=input$dateRange2[1]&oo1$Day <=input$dateRange2[2],]
		oo12 <- oo12[oo12$cloud_site_name == as.character(input$client),]
		if(length(unique(oo12$location_name))>1)
		{
			if(input$range=="Week")
			{
				p <- ggplot(oo12, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders") + ggtitle("Location &  Timewise - Online Orders")
			}
			if(input$range=="Day")
			{
				p <- ggplot(oo12, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders") + ggtitle("Location &  Timewise - Online Orders")
			}

			facets <- paste("location_name",'~',"Time",sep="")
			p <- p + aes(fill=user_new_old) + scale_fill_manual(values=c("green", "grey"),name="User Type",breaks=c("1", "2"),
																													labels=c("New User", "Old User")) + facet_grid(facets)
			try(print(p))
		}
	})
	output$pievn <- renderPlot({
		vn12 <- vn1[vn1$Day >=input$dateRange2[1]&vn1$Day <=input$dateRange2[2],]
		vn12 <- vn12[vn12$cloud_site_name == as.character(input$client),]
		try(pie(table(vn12$user_new_old),labels=c("New User","Old User"), main="User Distribution Virtual Number"))
	})

	output$basicvn <- renderPlot({
		vn12 <- vn1[vn1$Day >=input$dateRange2[1]&vn1$Day <=input$dateRange2[2],]
		vn12 <- vn12[vn12$cloud_site_name == as.character(input$client),]
		if(input$range=="Week")
		{p <- ggplot(vn12, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders") + ggtitle("Weekwise Distribution - Virtual Numbers")
		}
		if(input$range=="Day")
		{p <- ggplot(vn12, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders") + ggtitle("Weekwise Distribution - Virtual Numbers")
		}

		p <- p + aes(fill=user_new_old) + scale_fill_manual(values=c("green", "grey"),name="User Type",breaks=c("1", "2"),
																												labels=c("New User", "Old User"))
		try(print(p))
	})
	output$basicvncall <- renderPlot({
		vn12 <- vn1[vn1$Day >=input$dateRange2[1]&vn1$Day <=input$dateRange2[2],]
		vn12 <- vn12[vn12$cloud_site_name == as.character(input$client),]
		if(input$range=="Week")
		{p <- ggplot(vn12, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders") + ggtitle("Weekwise Distribution(Call Type) - Virtual Numbers")
		}
		if(input$range=="Day")
		{p <- ggplot(vn12, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders") + ggtitle("Weekwise Distribution(Call Type) - Virtual Numbers")
		}

		p <- p + aes(fill=call_status)
		try(print(p))
	})
	output$wdayvn <- renderPlot({
		vn12 <- vn1[vn1$Day >=input$dateRange2[1]&vn1$Day <=input$dateRange2[2],]
		vn12 <- vn12[vn12$cloud_site_name == as.character(input$client),]
		if(input$range=="Week")
		{
			p <- ggplot(vn12, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders") + ggtitle("Week of the Day Distribution - Virutal Numbers")
		}
		if(input$range=="Day")
		{
			p <- ggplot(vn12, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders") + ggtitle("Week of the Day Distribution - Virutal Numbers")
		}

		facets <- paste('.~',"Wday",sep="")
		p <- p + aes(fill=Wday) + facet_grid(facets)
		try(print(p))
	})
	output$timevn <- renderPlot({
		vn12 <- vn1[vn1$Day >=input$dateRange2[1]&vn1$Day <=input$dateRange2[2],]
		vn12 <- vn12[vn12$cloud_site_name == as.character(input$client),]
		if(input$range=="Week")
		{p <- ggplot(vn12, aes(x=factor(Week))) + geom_bar(colour="black") + xlab("Weeks") + ylab("Total Orders") + ggtitle("Time of Order - Virtual Numbers")
		}
		if(input$range=="Day")
		{p <- ggplot(vn12, aes(x=factor(Day))) + geom_bar(colour="black") + xlab("Days") + ylab("Total Orders") + ggtitle("Time of Order - Virtual Numbers")
		}

		facets <- paste("Time",'~.')
		p <- p + aes(fill=user_new_old) + scale_fill_manual(values=c("green", "grey"),name="User Type",breaks=c("1", "2"),
																												labels=c("New User", "Old User")) + facet_grid(facets)
		try(print(p))
	})
	output$user_rep <- renderPlot({
		REP.df <- data.frame()
		for(i in max(oo1$Week):(max(oo1$Week)-5))
		{	c <- c(i:max(oo1$Week))
			cc <- unique(oo2$Week)[!(unique(oo2$Week) %in% c)]
			oo3 <- oo2[oo2$cloud_site_name==as.character(input$client)&oo2$Week %in% cc,]
			u_user <- unique(oo3$user_id)
			for(j in 1:length(u_user))
			{	rep <- oo3[oo3$user_id==u_user[j],]
				rep_rate <- 0
				if(nrow(rep)>1)
				{
					for(k in 1:(nrow(rep)-1))
					{
						rep_rate <- sum(rep_rate,as.integer(rep$Day[k]-rep$Day[k+1]))
					}
					rep_rate <- rep_rate/(nrow(rep)-1)
				}
				REP2.df <- data.frame(Week=i,Client=input$client,User_ID=u_user[j],
															User_Source=rep$user_source_id[1],Rep_rate=rep_rate)
				REP.df <- rbind(REP.df,REP2.df)
			}
		}
		REP.df$Bucket <- "Once"
		REP.df[REP.df$Rep_rate<=10&REP.df$Rep_rate>0,]$Bucket <- "10 Days"
		REP.df[REP.df$Rep_rate<=30&REP.df$Rep_rate>10,]$Bucket <- "30 Days"
		REP.df[REP.df$Rep_rate>30,]$Bucket <- "30+ Days"

		REP.df$Repeat <- 0
		REP.df[REP.df$Bucket!="Once",]$Repeat <- 1
		try(ggplot(REP.df,aes(x = factor(Week),fill = factor(Repeat))) + geom_bar(position = "fill") +
					scale_y_continuous(labels = percent) + scale_fill_manual(values=c("Blue", "Red"),name="Repeat Rate",breaks=c("1","0"),
																																	 labels=c("Repeated Users", "Non Repeated Users")))
	})
	output$user_rep2 <- renderPlot({
		REP.df <- data.frame()
		for(i in max(oo1$Week):(max(oo1$Week)-5))
		{	c <- c(i:max(oo1$Week))
			cc <- unique(oo2$Week)[!(unique(oo2$Week) %in% c)]
			oo3 <- oo2[oo2$cloud_site_name==as.character(input$client)&oo2$Week %in% cc,]
			u_user <- unique(oo3$user_id)
			for(j in 1:length(u_user))
			{	rep <- oo3[oo3$user_id==u_user[j],]
				rep_rate <- 0
				if(nrow(rep)>1)
				{
					for(k in 1:(nrow(rep)-1))
					{
						rep_rate <- sum(rep_rate,as.integer(rep$Day[k]-rep$Day[k+1]))
					}
					rep_rate <- rep_rate/(nrow(rep)-1)
				}
				REP2.df <- data.frame(Week=i,Client=input$client,User_ID=u_user[j],
															User_Source=rep$user_source_id[1],Rep_rate=rep_rate)
				REP.df <- rbind(REP.df,REP2.df)
			}
		}
		REP.df$Source <- "Others"
		REP.df[REP.df$User_Source==0|REP.df$User_Source==1,]$Source <- "Subscriber"
		try(REP.df[REP.df$User_Source==2,]$Source <- "Online Order")
		try(REP.df[REP.df$User_Source==5,]$Source <- "Virtual Number")
		REP.df$Bucket <- "Once"
		REP.df[REP.df$Rep_rate<=10&REP.df$Rep_rate>0,]$Bucket <- "10 Days"
		REP.df[REP.df$Rep_rate<=30&REP.df$Rep_rate>10,]$Bucket <- "30 Days"
		REP.df[REP.df$Rep_rate>30,]$Bucket <- "30+ Days"
		try(ggplot(REP.df,aes(x=factor(Week)))+geom_bar() + aes(fill=(Source)) + facet_grid(".~Bucket"))



	})
	output$user_base <- renderPlot({
		user1 <- user[user$cloud_site_name==as.character(input$client),]
		user1$Day <- as.Date(user1$added,format="%Y-%m-%d")
		user1$Week <-  as.integer(strftime(user1$Day,"%W"))+1
		user1 <- user1[is.na(user1$Week)==F,]
		user2 <- user1
		user1 <- user1[user1$Day>="2015-01-01",]
		USER.df <- data.frame()
		for(i in max(user1$Week,na.rm=T):(max(user1$Week)-5))
		{	c <- c(i:max(user1$Week))
			cc <- unique(user2$Week)[!(unique(user2$Week) %in% c)]
			user3 <- user2[user2$Week %in% cc,]
			USER2.df <- dcast(user3,user_source_id~.)
			USER2.df <- cbind(Week=i,USER2.df)
			USER.df <- rbind(USER.df,USER2.df)
		}
		USER.df$Source <- "Others"
		try(USER.df[USER.df$user_source_id==0|USER.df$user_source_id==1,]$Source <- "Subscriber")
		try(USER.df[USER.df$user_source_id==2,]$Source <- "Online Order")
		try(USER.df[USER.df$user_source_id==5,]$Source <- "Virtual Number")
		try(ggplot(USER.df,aes(x=factor(Week),y=.))+geom_bar(stat="identity") + aes(fill=Source))
	})

})
