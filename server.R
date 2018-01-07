library(adagio)
library(dplyr)

simulaUnb <- function( x)
{
	caps <- x
	w2 <- w[ w < caps ] 
	p2 <- rep(1, length(w2))#p[ w< caps]
	foo <- mknapsack(  p2, w2  ,  caps)
	index <- which(foo$ksack==1)
	media <- mean(w2[index])
	maxi <- max(w2[index])
	return (c(dinero = x, n = foo$value, media = media, max =maxi))
}

# server.R

#library(ggvis)

#data reading and manipulations
#setwd("/home/fou")
#milbases <- read.csv("data/milbases.csv")
#reports <- read.csv("data/ufo_reports.csv")
#mergeddata <- merge(reports, milbases, by="State")
#mergeddata$id <- 1:nrow(mergeddata)

#function to connect a data point to its other variabless in data set for tooltip
#all_values <- function(x) {
#	if(is.null(x)) return(NULL)
#	row <- mergeddata[mergeddata$id == x$id, ]
#	paste0("State", ": ", row$State,"<br />", "UFO Reports", ": ", row$UFOReports,"<br />", "Total Bases", ": ", row$Total, "<br />", "Air Force Bases", ": ", row$AirForce,"<br />")
#}

data <- read.csv('C:\\Users\\fou-f\\Desktop\\KnapsackApp\\MAV_CONSOLIDADOMODELO - copia.csv')
d <- data %>% select(monto_sol_dev ,ejercicio, periodo)

table(d$periodo)
ano <- unique(d$periodo)
a <- vector(mode="list", length = length(ano))


for (mes_n in 1:length(ano))
{
	montos <- subset(d, periodo %in% (ano[mes_n]))
	w <- montos$monto_sol_dev
	w <- w[w>0] #solo positivos
	p <- rep(1, length(w))
	tope <-sum(montos$monto_sol_dev)
	#tope2 <-2**31-10
	pocodinerito <- seq(from = 5,by = 2000, length = 60 )
	mediodinerito <- round(seq(from = max(pocodinerito), to =tope , length = 120))
	dinero <- c(pocodinerito, mediodinerito)
	resUnb <- data.frame(dinero = dinero ) # sum(d$monto_sol_dev)
	mes <- mapply(simulaUnb, resUnb$dinero )
	mes <-as.data.frame(t(mes))
	mes$periodo<- as.character(ano[mes_n])
	a[[mes_n]] <- mes
}
library(data.table)
stats <- rbindlist(a)
stats2 <- stats 
stats2$periodo <- factor(stats2$periodo, labels  = c('Octubre', 'Noviembre', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre'))

vis_names <- function(x)
{
	if(is.null(x)) return(NULL)
	paste0('media: ',round(x['media'],2), '<br /> max: ',round(x['max'],2),
		   collapse="<br />")
	#http://stackoverflow.com/questions/27992078/displaying-information-using-ggvis
}


shinyServer(
	function(input, output, session) {
		
		#interactive controls
		input_size <- reactive(input$size)
		input_model <- reactive(input$model)
		library(ggvis)
		library(shiny)
		point_obj <- subset(stats2, n>0) %>% ggvis( x= ~dinero, y = ~n, fill = ~ periodo, size = ~media, opacity = ~max ) %>% 
			layer_points() %>%    hide_legend("size") %>%
			add_axis("x", title = "monto total (suma)", title_offset = 50) %>%
			add_axis("y", title = "max. n?mero de tramites", title_offset = 50) %>%
			add_axis("x", orient = "top",  title = "Costo vs n?mero de beneficiados",
					 properties = axis_props( axis = list(stroke = "white"),
					 						 labels = list(fontSize = 0)))
		
		
		##
		point_obj %>% add_tooltip(vis_names,"hover") %>% 	bind_shiny("ggvis", "ggvis_ui")
		
		
		
		#ggvis plot build
	#	mergeddata %>% 
	#		ggvis( x = ~Total, y = ~UFOReports) %>% 
	#		layer_points(fill := "blue", size := input_size, key := ~id) %>%
	#		add_tooltip(all_values, "hover") %>%
	#		layer_model_predictions(model = input_model, se = TRUE) %>%
	#		add_axis("x", title = "Number of Military Bases", orient = "bottom", title_offset = 50) %>%
	#
	#	add_axis("y", title = "Num of UFO Reports", orient = "left", title_offset = 50) %>%
		
		
		
		
		
		
		
		
		
		
		
		
		
		}
)

##############
shinyApp(ui = shinyUI, server = shinyServer)


