## ====================================================================================== ##
# Title:  edX Learning Objective Grade and Interaction Analysis and Visualization
# 
#     Copyright 2019-2020 Michael Ginda
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#     
#     http://www.apache.org/licenses/LICENSE-2.0
#     
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
#
# Authors:      Michael Ginda
# Affiliation:  Indiana University
# 
# Description: This R scripts translates learner grade performance and 
#              interaction data (event logs) generated while engaging with an edX course's
#              modules into learning objective analysis based on learning objective analysis
#              of edX course subsection content modules (e.g. sequential modules).
#              engagement level analysis edX course data for learners
#
# File input stack: 
#            1) A processed edX Course structure and content module list:
#               - {org}+{course}+{term}-module-lookup.csv;
#               - extracted by script, edX-0-courseStructureMeta.R;
#            2) A list of student userIDs from an edX course:
#               - {org}-{course}-{term}-auth_user-students.csv;
#               - extracted by script, edX-1-studentUserList.R;
#            3) A "studentevents" directory containing one or more student
#               CSV event log file(s):
#               - {userID}.csv;
#               - extracted by script, edX-2-eventLogExtractor.R
#
# Output files:                        
#            1) A set of processed data tables of event action logs for each student:
#               - {userID}.csv;
#               - Used in scripts:
#                 * edX-4-learnerTrajectoryNet.R
#            2) A list capturing student userIDs who are active in the course,
#				        and have usable actions:
#               - {org}-{course}-{term}-auth_user-students-active.csv;
#               - Used in scripts:
#                 * edX-4-learnerTrajectoryNet.R;
#            3) A list capturing student userIDs who are active in the course,
#				        but have no usable actions:
#               - {org}-{course}-{term}-auth_user-students-unusableActivity.csv;
#            4) A list capturing student userIDs who are inactive in the course,
#				        and have no actions in their event logs:
#               - {org}-{course}-{term}-auth_user-students-inactive.csv; 
#
# Package dependencies: magrittr, stringr, plyr, tcltk reshape2
#
# Change log:
# 05.2019 - Initial code creating learning objective grade and time analysis &
#           box-plot and parallel coordinate chart visualiations.
# 12.2019 - Updated script to clarify data sets used; re-order arguments for consistency; 
#           clarified inline documentation; 
#
## ====================================================================================== ##
#### Environmental Setup ####
rm(list=ls()) 

#Load required packages 
library(tidyr)      # for data preparation
#library(stringr)    # for string manipulation
library(plyr)       # for data aggregations
library(dplyr)      # for filtering
library(reshape2)   # for data reshaping
library(FactoMineR) # for PCA analysis and HCPC clustering
library(ggplot2)    # for visualization
library(viridis)    # for color scales
library(gridExtra)  # for arranging figures

#### Grade, Dwell Time and Edge Data Analysis 
grade_s <- dwell_s <- data.frame()

# LO ~ Grade Descriptives Statistics - Cohort Level
for(i in 1:nrow(lo_id)){
  data <- grades[grades$objective==lo_id[i,1],]
  students <- length(unique(data$user_id))
  earnedPoints <- sum(data$earned_points)
  possiblePoints <- mean(data$possible_points)
  avgPoints <- mean(data$earned_points)
  varPoints <- var(data$earned_points)
  sdPoints <- sd(data$earned_points)
  sePoints <- sd(data$earned_points)/length(unique(data$user_id))
  mdPoints <- median(data$earned_points)
  avgPer <- mean(data$per_earned)
  varPer <- var(data$per_earned)
  sdPer <- sd(data$per_earned)
  sePer <- sd(data$per_earned)/length(unique(data$user_id))
  mdPer <- median(data$per_earned)
  data <- cbind(students,earnedPoints,possiblePoints,avgPoints,varPoints,sdPoints,sePoints,mdPoints,avgPer,varPer,sdPer,sePer,mdPer)
  grade_s <- rbind(grade_s,data)
}
rm(students,earnedPoints,possiblePoints,avgPoints,varPoints,sdPoints,sePoints,mdPoints,avgPer,varPer,sdPer,sePer,mdPer)
grade_s<-cbind(lo_id,grade_s)

# LO ~ Events and Dwell Time Descriptives Statistics - Cohort Level Analysis
for(i in 1:nrow(lo_id)){
  data <- dwell[dwell$objective==lo_id[i,1],]
  students <- length(unique(data$user_id))
  avgEvents <- mean(data$events)
  varEvents <- var(data$events)
  sdEvents <- sd(data$events)
  seEvents <- sd(data$events)/length(unique(data$user_id))
  mdEvents <- median(data$events)
  avgDwell <- mean(data$dwell)
  varDwell <- var(data$dwell)
  sdDwell <- sd(data$dwell)
  seDwell <- sd(data$dwell)/length(unique(data$user_id))
  mdDwell <- median(data$dwell)
  data <- cbind(students,avgEvents,varEvents,sdEvents,seEvents,mdEvents,avgDwell,varDwell,sdDwell,seDwell,mdDwell)
  dwell_s <- rbind(dwell_s,data)
}
rm(students,avgEvents,varEvents,sdEvents,seEvents,mdEvents,avgDwell,varDwell,sdDwell,seDwell,mdDwell)
dwell_s<-cbind(lo_id,dwell_s)

# LO Transition Descriptives - Full Cohort
edges_s <- ddply(edges, ~ from + to, summarise,
                 weight = length(user_id),
                 weight_l = log(length(user_id)),
                 weight_avg = length(user_id)/length(unique(user_id)),
                 uniStu = length(unique(user_id)))
edges_s$from <- as.character(edges_s$from)
edges_s$to <- as.character(edges_s$to)
edges_s$weight_l <- as.numeric(edges_s$weight_l)
edges_s$weight_avg <- as.numeric(edges_s$weight_avg)
edges_s$uniStu <- as.numeric(edges_s$uniStu)

## Analysis Results Output
write.csv(grade_s, file=paste0(path_output,"/grade-stats.csv"),row.names = F)
write.csv(dwell_s, file=paste0(path_output,"/dwell-stats.csv"),row.names = F)
write.csv(edges_s,file=paste0(path_output,"/edge-weighted.csv"), row.names = F)


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
  geom_text(aes(label = round(weight_avg, 0)),
            #label = round(uniStu/max(uniStu)*100, 2)),
            colour="grey10") +
  labs(tag = "B",
       x="Learning Objectives",
       caption = "Labels show the average number transitions between learning objectives made by students in the heatmap.",
       #caption = "Labels show the percentage of students making a transition in the heatmap.",
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




## Join Clusters to Edges
clusters$user_id <- row.names(clusters)
clust_count <- ddply(clusters,~clust, summarise,
                     ind = length(user_id))

edges <- join(edges,clusters,by="user_id")

# LO Transition Descriptives - Cohort Level
edges_c <- ddply(edges,clust ~ from + to, summarise,
                 weight = length(user_id),
                 weight_l = log10(length(user_id)),
                 uniStu = length(unique(user_id)))

#Calculating Cluster level percentage of unique student and average weight calculations
edges_c$weight_avg <- edges_c$uniStuMax <- edges_c$uniStuPer <- NA
for(i in 1:length(levels(edges_c$clust))){
  edges_c[edges_c$clust==i,]$uniStuMax <- max(edges_c[edges_c$clust==i,]$uniStu)
}
edges_c$uniStuPer <- edges_c$uniStu/edges_c$uniStuMax
edges_c$weight_avg <- edges_c$weight/edges_c$uniStuMa

#Setting Data Types
edges_c$uniStu <- as.numeric(edges_c$uniStu)


#Cluster Labels
for(i in 1:length(levels(edges_c$clust))){
  levels(edges_c$clust)[i] <- paste0("Cluster ",levels(edges_c$clust)[i],", ",clust_count[clust_count$clust==i,]$ind," Ind.")
}

c <- ggplot(edges_c,aes(x=from,y=to)) + 
  geom_tile(aes(fill=weight_l)) +
  geom_text(aes(label = round(edges_c$weight_avg, 1)),
            colour="grey10",
            size=2) +
  labs(tag = "C",
       x=paste0("Learning Objectives - From ","\U2192"),
       y=paste0("Learning Objectives - To ","\U2192"),
       caption = "Labels show the average number transitions between learning objectives made by students in the heatmap.",
       fill="Weighted\nNode\nAdjacency\n(Log Scaled)") +
  scale_fill_gradientn(colours=magma(100, alpha = .8, begin = .3, end = 1, direction = 1)) +
  guides(fill = guide_colorbar(label.theme = element_text(colour="grey30"),
                               frame.colour = "grey60")
  ) + 
  facet_grid(cols = vars(clust)) +
  theme(axis.title.x = element_text(hjust=1, colour="grey20",
                                    margin=unit(c(3,0,0,0),"mm")),
        axis.ticks.x.bottom = element_line(colour="grey60", linetype = "solid"),
        axis.line.x = element_blank(),
        axis.title.y = element_text(hjust=.96, colour="grey20",
                                    margin=unit(c(0,3,0,0),"mm")),
        axis.ticks.y = element_line(colour="grey60", linetype = "solid"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_rect(fill = "white" ), 
        strip.text = element_text(colour="grey20", hjust=1)
  )


# Final Grid Arrangement with Bar Graph of Avg Student Dwell Time and 
# Learning Objective Adjacency Network Heatmap with Percent of Students Making Transtion Labeled
png(filename=paste0(path_output,"/panel-bar+heatmap-divg-logNormed-labsAvgTrans.png"),
    width = 12, height = 8, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
grid.arrange(grid.arrange(a,g.mid,b,ncol=3,widths=c(4.5/10,.5/10,4.5/10)),
             grid.arrange(c,ncol=1,widths=1/1),
             ncol=1,nrow=2,heights=c(4.75/8,3.25/8))
dev.off()


#### Finishing Details ####
#Indicate completion
message("\n**** Complete! ****\n")
## Script processing time feedback
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print((proc.time()[3] - start[3])/60)
## Clear environment variables
rm(list=ls())