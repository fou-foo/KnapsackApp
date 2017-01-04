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


data <- read.csv('d:\\Users\\GARA903U\\Desktop\\1.5\\MAV_CONSOLIDADOMODELO.csv')
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

cotas <- data %>% group_by(periodo) %>%
  summarise( n =n(),
    dinero = sum(monto_sol_dev), 
                 media = mean(monto_sol_dev),
                  max = max(monto_sol_dev)
                                         )


#saca <- function(x)
#{
#  return(a[[x]])
#}
library(data.table)
stats <- rbindlist(a)
stats2 <- stats 
stats2$periodo <- factor(stats2$periodo, labels  = c('Octubre', 'Noviembre', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre'))
# 
# library(ggplot2)
# ggplot(stats, aes(dinero, n, 
#                   size = media, alpha = max, colour = periodo 
#        )) + geom_point() +
#   theme_bw() + xlab("monto total (suma)")+
#   ylab("max. número de tramites")+ggtitle("Costo vs número de beneficiados")+
#   geom_point(data = cotas, aes(colour="red"))

library(ggvis)
library(shiny)
point_obj <- subset(stats2, n>0) %>% ggvis( x= ~dinero, y = ~n, fill = ~ periodo, size = ~media, opacity = ~max ) %>% 
  layer_points() %>%    hide_legend("size") %>%
  add_axis("x", title = "monto total (suma)", title_offset = 50) %>%
  add_axis("y", title = "max. número de tramites", title_offset = 50) %>%
  add_axis("x", orient = "top",  title = "Costo vs número de beneficiados",
           properties = axis_props( axis = list(stroke = "white"),
             labels = list(fontSize = 0)))


##
vis_names <- function(x)
{
  if(is.null(x)) return(NULL)
  paste0('media: ',round(x['media'],2), '<br /> max: ',round(x['max'],2),
         collapse="<br />")
  #http://stackoverflow.com/questions/27992078/displaying-information-using-ggvis
}
point_obj %>% add_tooltip(vis_names,"hover")





#write.csv(resUnb, file = "d:\\Users\\GARA903U\\Desktop\\1.5\\cotas.csv")
#simulaUnb(10000000)
