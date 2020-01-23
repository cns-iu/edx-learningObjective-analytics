library(reshape2)
library(FactoMineR)

#### Clustering Analaysis - Learning Objective Transitions ####

## Student Learning Objective Transition Cluster Analysis and Visualization

#Grade Point count totals
grades_i <- ddply(grades, ~ user_id, summarise,
                  earned_points = sum(earned_points)) 

grades_i$percentile <- ecdf(grades_i$earned_points)(grades_i$earned_points)


#Dwell Time Totals
dwell_i <- ddply(edges,~user_id,summarise,
                 dwell=sum(period))

#Events in log
events_i <- edges %>% ddply(~user_id,summarise,
                            events = length(period))

#Edge Transition Event counts - Self Loops Removed
edges_i <- edges[edges$sl==0,1:7] %>% dcast(user_id ~ from + to, fun.aggregate = length, value.var="period")

#edges_i[,c(2:ncol(edges_i))] <- edges_i[,c(2:ncol(edges_i))] %>% scale()



# Join fields together
edges_i <- join(events_i,edges_i,by="user_id")
edges_i <- join(dwell_i,edges_i,by="user_id")
edges_i <- join(grades_i,edges_i,by="user_id")
row.names(edges_i) <- edges_i$user_id

# Remove ID column
edges_i <- edges_i[,c(2:ncol(edges_i))]


res.pca <- PCA(edges_i, quanti.sup = c(1:4), ncp = 8, scale.unit = TRUE)
res.pca$eig

res.pca$var

data <- t(as.matrix(res.pca$var$cor))
heatmap(data)

data <- t(as.matrix(res.pca$var$cos))
heatmap(data)

data <- t(as.matrix(res.pca$var$contrib))
heatmap(data)


plot.PCA(res.pca, axes=c(1, 3), choix="ind", habillage=1,
         label="none")
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=2,
         label="quanti.sup")
plot.PCA(res.pca, axes=c(1, 2), choix="varcor", habillage=1,
         label="quanti.sup")
plot.PCA(res.pca, axes=c(1, 3), choix="varcor", habillage=1,
         label="var")

dimdesc(res.pca, axes=c(1:5))


res.hcpc <- HCPC(res.pca)
res.hcpc$data.clust
res.hcpc$desc.axes
res.hcpc$desc.ind
res.hcpc$desc.var

plot.HCPC(res.hcpc, axes=c(1,2), choice="3D.map") 
plot.HCPC(res.hcpc, axes=c(1,2), choice="tree") 
plot.HCPC(res.hcpc, axes=c(1,2), choice="map",ind.names = F,draw.tree=F) 

clusters <- as.data.frame(res.hcpc$data.clust)[ncol(as.data.frame(res.hcpc$data.clust))]




#### Visualization ####
## Learning Objective Analysis
# GGPLOT 2 Bar Chart and Heatmaps
# Panel A: Average Student Dwell Time (Minutes) per Learning Objective
dwell_s$objective <- factor(dwell_s$objective,levels(dwell_s$objective)[c(8:1)])
dwell_s$errorEvents <- qt(0.975,df=dwell_s$students-1) * dwell_s$sdEvents/sqrt(dwell_s$students) 
dwell_s$errorDwell <- qt(0.975,df=dwell_s$students-1) * dwell_s$sdDwell/sqrt(dwell_s$students) 
a <- ggplot(data=dwell_s, aes(y=dwell_s$avgDwell,x=dwell_s$objective)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymax=dwell_s$avgDwell + dwell_s$errorDwell,
                    ymin=dwell_s$avgDwell - dwell_s$errorDwell),
                width = .4, size=.75) +
  labs(tag = "A",
       y="Average Student Dwell Time (Minutes)",
       caption="") +
  coord_flip() + scale_y_reverse() +
  theme(axis.title.x = element_text(hjust=.93, color="grey20"                                              ,
                                    margin=unit(c(3,0,0,0),"mm")),
        axis.ticks.x.bottom = element_line(color="grey60", linetype = "solid"),
        axis.line.x = element_line(color="grey60", linetype = "solid", size = .75),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color="grey60"),
        panel.grid.minor.x = element_line(linetype="dotted",color="grey80"),
        panel.grid.major.y = element_blank()
  )
# Panel Mid Labels
g.mid <- ggplot(dwell_s,aes(x=1,y=objective))+
  geom_text(aes(label=objective), size=3 , color="grey20")+
  geom_segment(aes(x=0.93,xend=0.95,yend=objective), color="grey60")+
  geom_segment(aes(x=1.05,xend=1.066,yend=objective), color="grey60")+
  labs(y=NULL, caption = "")+
  scale_x_continuous(expand=c(0,0),limits=c(0.93,1.066))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,1,6,1), "mm"))

# Panel B: Heatmap of Adjacency Matice
b <- ggplot(edge_s,aes(x=from,y=to)) + 
  geom_tile(aes(fill=weight_l)) +
  geom_text(aes(label = round(uniStu/max(uniStu)*100, 2)),
            colour="grey10") +
  labs(tag = "B",
       x="Learning Objectives",
       caption = "Labels show the percentage of students making a transition in the heatmap.",
       fill="Weighted\nNode\nAdjacency\n(Log Scaled)") +
  scale_fill_gradientn(colours=magma(100, alpha = .8, begin = .3, end = 1, direction = 1)) +
  guides(fill = guide_colorbar(label.theme = element_text(colour="grey30"),
                               frame.colour = "grey60")
  ) +
  theme(axis.title.x = element_text(hjust=.065, colour="grey20",
                                    margin=unit(c(3,0,0,0),"mm")),
        axis.ticks.x.bottom = element_line(colour="grey60", linetype = "solid"),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
  )

# Final Grid Arrangement with Bar Graph of Avg Student Dwell Time and 
# Learning Objective Adjacency Network Heatmap with Percent of Students Making Transtion Labeled
png(filename=paste0(path_output,"/panel-bar+heatmap-divg-logNormed-labsAvgTrans.png"),
    width = 12, height = 6, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
grid.arrange(a,g.mid,b,ncol=3,widths=c(4.5/12,.5/12,6.5/13))
dev.off()

