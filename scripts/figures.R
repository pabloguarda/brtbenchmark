# 0) THEMES ------------------------------------------------------------------
# # Sin el cuadrito que rodea al histograma ni los ejes
# theme1 = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#       panel.background = element_blank(),
#       axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
#       ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 90)
#       ,axis.title.x=element_text(size=24,face="bold",vjust = -0.5,hjust = 0.5)
#       ,axis.title.y=element_text(size=24,face="bold",vjust = 2,hjust = 0.5)
#       ,panel.margin = unit(0, units="mm")
#       ,plot.margin = (unit(c(.5, .5, 2, 2), "cm")) #Tamano de la imagen en que esta contenido el grafico            
#       ,axis.ticks.margin=unit(1, units="mm")
#       ,axis.ticks.length=unit(3, units="mm")
#       
# )


#Con los ejes
theme2 = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(),
               axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
               ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 90)
               ,axis.title.x=element_text(size=20,face="bold",vjust = -0.5,hjust = 0.5)
               ,axis.title.y=element_text(size=20,face="bold",vjust = 2,hjust = 0.5)
               ,panel.margin = unit(0, units="mm")
               ,plot.margin = (unit(c(.5, .5, 2, 2), "cm")) #Tamano de la imagen en que esta contenido el grafico            
               ,axis.ticks.margin=unit(1, units="mm")
               ,axis.ticks.length=unit(3, units="mm")
               ,axis.line = element_line(color = 'black'))          

#Con los ejes
theme3 = theme(
                panel.grid.major = element_blank(), panel.grid.minor = element_blank()
               ,panel.background = element_blank()
               ,panel.border = element_rect(colour = "black", fill=NA)
               ,axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
               ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 90)
               ,axis.title.x=element_text(size=20,face="bold",vjust = -0.5,hjust = 0.5)
               ,axis.title.y=element_text(size=20,face="bold",vjust = 2,hjust = 0.5)
               ,panel.margin = unit(0, units="mm")
               ,plot.margin = (unit(c(.5, .5, 2, 2), "cm")) #Tamano de la imagen en que esta contenido el grafico            
               ,axis.ticks.margin=unit(1, units="mm")
               ,axis.ticks.length=unit(3, units="mm")
               ,axis.line = element_line(color = 'black'))          


#Cambios en la legenda
theme_lines1 = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(),
               axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
               ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 90)
               ,axis.title.x=element_text(size=17,face="bold",vjust = -0.5,hjust = 0.5)
               ,axis.title.y=element_text(size=17,face="bold",vjust = 2,hjust = 0.5)
               ,plot.title = element_text(size = 20,vjust = 4,hjust = 0.5) 
               ,panel.margin = unit(0, units="mm")
               ,plot.margin = (unit(c(2, 1, 1, 1.5), "cm")) #Tamano de la imagen en que esta contenido el grafico, the order is (uppon,right,below,left)            
               ,axis.ticks.margin=unit(1, units="mm")
               ,axis.ticks.length=unit(3, units="mm")
               ,axis.line = element_line(color = 'black')
               ,legend.position="bottom"
               ,legend.margin=unit(10, units="mm")
               ,legend.key = element_blank(),legend.key.width=unit(2,"cm"),legend.key.size=unit(0,"cm")
               ,legend.title=element_text(size=c(16),face="bold"),legend.text=element_text(size=c(16))
)

#Para bars plots
theme_bars1 = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(),
               axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
               ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 90)
               ,axis.title.x=element_text(size=16,face="bold",vjust = -0.5,hjust = 0.5)
               ,axis.title.y=element_text(size=16,face="bold",vjust = 2,hjust = 0.5)
               ,plot.title = element_text(size = 20,vjust = 4,hjust = 0.5) 
               ,panel.margin = unit(0, units="mm")
               ,plot.margin = (unit(c(2, 1, 1, 1.5), "cm")) #Tamano de la imagen en que esta contenido el grafico, the order is (uppon,right,below,left)            
               ,axis.ticks.margin=unit(1, units="mm")
               ,axis.ticks.length=unit(3, units="mm")
               ,axis.line = element_line(color = 'black')
               ,legend.position="bottom"
               ,legend.text=element_text(size=16),legend.margin=unit(10, units="mm")
               ,legend.key = element_blank(),legend.key.width=unit(0.5,"cm"),legend.key.size=unit(0,"cm")
               ,legend.title=element_text(size=c(16),face="bold")
)


#Cambios en la legenda
theme_time = theme(
               panel.background = element_blank()
#                ,panel.grid.major = element_line(size = 5, linetype = 'solid')
               ,axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
               ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 90)
               ,axis.title.x=element_text(size=18,face="bold",vjust = -0.5,hjust = 0.5)
               ,axis.title.y=element_text(size=18,face="bold",vjust = 2,hjust = 0.5)
               ,plot.title = element_text(size = 22,vjust = 4,hjust = 0.5) 
               ,panel.margin = unit(0, units="mm")
               ,plot.margin = (unit(c(2, 1, 1, 1.5), "cm")) #Tamano de la imagen en que esta contenido el grafico, the order is (uppon,right,below,left)            
               ,axis.ticks.margin=unit(1, units="mm")
               ,axis.ticks.length=unit(3, units="mm")
               ,axis.line = element_line(color = 'black')
               ,legend.position="bottom",legend.title=element_blank()
               ,legend.text=element_text(size=18),legend.margin=unit(10, units="mm")
               ,legend.key = element_blank(),legend.key.width=unit(2,"cm"),legend.key.size=unit(0,"cm")
               
)


# 1) Tukey Test --------------------------------------------------------------


# a) Confidence Intervals for Difference in Productivity among BRT Classification  ------------------------------

significance = 0.05

#También se puede hacer este gráfico para las elasticidades. Utilizando el comando IRR se 
# puede obtener los intervalos de confianza para los IRR y por ende, para los fc

# intervals = factor(c("Gold-Basic BRT","Gold-Bronze","Gold-Silver","Silver-Bronze","Silver-Basic BRT","Bronze-Basic BRT"))
# intervals = factor(c("Gold-Basic BRT","Gold-Bronze"))
model = TukeyHSD(aov(productivity ~ factor(classification),scores),conf.level=1-significance,ordered=TRUE,which=intervals)


model = as.data.frame(model$`factor(classification)`)
model$ranking = as.character(row.names(model))
model = model[order(model$diff,decreasing=TRUE),]


# data_plot = subset(model,`p adj`<significance & str_detect(ranking,"Basic BRT"))
# data_plot = subset(model,`p adj`<significance & str_detect(ranking,"Gold"))
# data_plot$significant = FALSE
# data_plot$significant[model[`p adj`<significance & str_detect(ranking,"Gold")]


# data_plot = subset(model,`p adj`<significance)
data_plot = model
data_plot$significant = FALSE
data_plot$significant[data_plot$`p adj`<=significance] = TRUE
row.names(data_plot)=NULL

# label1 = "Gold-Basic BRT"
# label2 = "Gold-Bronze"
# label3 = "Silver-Gold"
opposite_label = "Basic BRT-Bronze"
good_label = "Bronze-Basic BRT"
data_plot$diff[data_plot$ranking==opposite_label] = -data_plot$diff[data_plot$ranking==opposite_label]
data_plot$lwr[data_plot$ranking==opposite_label] = -data_plot$lwr[data_plot$ranking==opposite_label]
data_plot$upr[data_plot$ranking==opposite_label] = -data_plot$upr[data_plot$ranking==opposite_label]

temp = data_plot$upr[data_plot$ranking==opposite_label]
data_plot$upr[data_plot$ranking==opposite_label] = data_plot$lwr[data_plot$ranking==opposite_label]
data_plot$lwr[data_plot$ranking==opposite_label] = temp
data_plot$ranking[data_plot$ranking==opposite_label] = good_label
# #Labels
# data_plot$ranking[data_plot$ranking==label1] = "Basic BRT"
# data_plot$ranking[data_plot$ranking==label2] = "Bronze"
# data_plot$ranking[data_plot$ranking==label3] = "Silver"

data.estimates = c()
data.estimates$var = data_plot$ranking
data.estimates$mean = 0; data.estimates$mean= data_plot$diff
data.estimates$upper = data_plot$upr
data.estimates$lower = data_plot$lwr
data.estimates$group = data_plot$significant
data.estimates = as.data.frame(data.estimates)
data.estimates$var = as.character(data.estimates$var)
order_labels = c("Gold-Basic BRT","Gold-Bronze","Gold-Silver","Silver-Bronze","Silver-Basic BRT","Bronze-Basic BRT")
data.estimates$var = factor(data.estimates$var,levels = order_labels)

# l1_y = round(min(data.estimates$lower)/1000,0)*1000
l1_y = -10000
# l2_y = round(max(data.estimates$upper)/1000,0)*1000
l2_y = 20000
interval_y = 5000
# l1_x = min(data.estimates$lower)
# l2_y = max(data.estimates$upper)

(ggplot(data.estimates, aes(var,mean, size=10,colour = group)) + theme_bw(base_size=10)
+geom_point(show_guide = FALSE) +geom_errorbar(aes(x = var, ymin = lower, ymax = upper, size=2), width = 0.2)
+ xlab("Comparison between BRT Classifications") + ylab("Difference in Productivity [pax/km]")
# +ggtitle("Difference in productivity among BRT classifications")
# +scale_x_continuous(limits=c(l1_y-interval_y,100),breaks=abs(rev(seq(-100,-round(l1_y,0)+interval_y,interval_y)))) 
+scale_y_continuous(limits=c(l1_y-interval_y,l2_y+interval_y),breaks=c(0,seq(l1_y-interval_y,round(l2_y,0)+interval_y,interval_y)),labels = labels1000K
                    
                    )
+scale_color_manual(values=c("red","black"))
+theme(
  
  axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
  ,axis.text.y=element_text(color = "black", size=14,hjust = 0.5, vjust = 0.4, angle = 90)
  ,axis.title.x=element_text(size=14,face="bold",margin=margin(20,0,0,0))
  ,axis.title.y=element_text(size=14,face="bold",margin=margin(0,20,0,0))
  ,panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ,panel.background = element_rect(fill = 'white', colour = 'black')
  ,plot.title = element_text(size = 20,vjust = 2,hjust = 0.5,face="bold") 
  ,plot.margin = (unit(c(1, 1, 1, 1), "cm")) #Tamaño de la imagen en que está contenido el gráfico            
  ,legend.position="none" #Para eliminar la legenda del gráfico. 
)

)
ggsave("figures/Productivity_BRTclass.jpg", width=40, height=20, unit="cm", dpi=600)

