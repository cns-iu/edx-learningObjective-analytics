## ====================================================================================== ##
# Title:       Network Analysis of Learner Trajectory through Learning Objectives
# Project:     edX learner trajectory analysis
# 
#     Copyright 2019 Michael Ginda
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
# Description: This visual analytics script maps learner(s) course activities (learner 
#              event logs) in a edX course and their grades associated course content 
#              modules to learning objective data set identified and aligned to course 
#              content by instructional designers to generate a transition matrice 
#              between learning objectives, and calculations of learning objective dwell
#              time and average grade performance for 1 or more student in a course.
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
# 12.2019 - Initial code creating learning objective transition network with grade and 
#           dwell time analysis as a network heatmap visualiation.
#
## ====================================================================================== ##

#### Environmental Setup ####
rm(list=ls()) 

# Load required packages
library(tcltk2)     # for OS independent GUI file and folder selection
library(tidyr)      # for data preparation
#library(stringr)    # for string manipulation
library(plyr)       # for data aggregations
library(dplyr)      # for filtering
library(reshape2)   # for data reshaping
library(igraph)     # for creating an adjacency matrix
library(FactoMineR) # for PCA analysis and HCPC clustering
library(ggplot2)    # for visualization
library(viridis)    # for color scales
library(gridExtra)  # for arranging figures

#library(gplots)     # for heatmap visualization - heatmap.2 function

#### Paths #### 
# Generic Set up
# Assigns a path to directory with original edX course data files used in data processing
path_data = tclvalue(tkchooseDirectory())
#Assigns a path to open prior script outputs and to save new processing output files.
path_output = tclvalue(tkchooseDirectory())

#### Load Data ####
# Course Module Learning Objective Analysis Results
lo <- read.csv(file=list.files(full.names = TRUE, recursive = FALSE, 
                                 path = paste0(path_output,"/learningObjectives/"),
                                 pattern = "module-lo-lookup-cond-sci.csv$"), header=T)

# Student course subsection grades - measured at the sequencial group level of course structure
subGrades <- read.csv(file=list.files(full.names = TRUE, recursive = FALSE, 
                                      path = paste0(path_data,"/state/"),
                                      pattern = "grades_persistentsubsectiongrade.csv$"), header=T)[c(2:7)]

# Create paths to student logs data
# Load list of users to create list of log file paths
users <- read.csv(list.files(full.names = TRUE, recursive = FALSE, 
                                 path = paste0(path_output,"/userlists/"),
                                 pattern = "-auth_user-students-pass.csv$"),header=T)

names(users) <- "id"
filePaths <- paste0(path_output,"/studentevents_processed/",users$id,".csv")

#### Data Processing ####
# Save the time (to compute elapsed time of script)
start <-  proc.time() 

### Preparing Learning Objectives Data
# Reshape Learning Objective matrix to a list, NAs removed from data set (hidden modules or instructions for course)
lo <- lo %>% reshape2::melt(id.vars=c(1:13),variable.name="objective",na.rm=T)
# Updates objective factors
lo$objective <-  gsub("\\.", " ",lo$objective)

## Learning Objective identifier data frame
lo_id <- as.data.frame(unique(lo$objective))
names(lo_id) <- "objective"

## Learning Objectives to Sequential Modules
# Subset to only values 1, indicating a learning objective was observed, and tree levels of 2 (sequential modules), 
# then refactor identifiers
lo_s <- lo[lo$value==1 & lo$treelevel==2,c("mod_id","mod_type","objective")] %>% 
  mutate(mod_id = factor(mod_id)) %>% mutate(mod_type = factor(mod_type))

## Learning Objectives to Content Modules
# Subset to only values 1, indicating a learning objective was observed, and tree levels of 4 (content modules), 
# then refactor identifiers
lo_c <- lo[lo$value==1 & lo$treelevel==4,c("mod_id","mod_type","seqModPar","objective")] %>% 
        mutate(mod_id = factor(mod_id)) %>% mutate(mod_type = factor(mod_type))

## Learning Objective Grades
# Preparing Sequential Module Grade Data (edX persistent subsection grades)
subGrades <- subGrades[,c("usage_key","earned_graded","possible_graded","user_id")] %>% 
             mutate(mod_id = stringr::str_split_fixed(usage_key, "\\@", 3)[,3])

# Some sequential modules are removed if they are not mapped to learning objectives (e.g. prequestionnaires, hidden content, etc.)
grades <- join(subGrades[,c("mod_id","earned_graded","possible_graded","user_id")],
              lo_s[,c("mod_id","objective")],by="mod_id",type = "left") %>% 
          na.exclude() %>%  
          ddply(.,~ objective+user_id, summarise,
               earned_points = sum(earned_graded),
               possible_points = sum(possible_graded),
               per_earned = sum(earned_graded)/sum(possible_graded))
grades <- grades[grades$user_id %in% users$id,]
rm(lo_s,subGrades)

## Learning Objective Dwell Times
dwell  <- data.frame(objective=as.character(),                     # Learning Objective
                     dwell=as.numeric(),                           # Dwell Time (Min)
                     events=as.numeric(),                          # Event Count
                     created=as.POSIXct(as.character()),  # First Event Log Created
                     modified=as.POSIXct(as.character()), # Last Modification to Event Log
                     user_id=as.integer(),                         # Student Identifier
                     stringsAsFactors=FALSE)

# Learning Objective Network Edge List Setting
# Self loops are kept in this network because they are relevant sequences where students work
# on the same objective for a period of time.
selfLoopKeep <- T 

# Learning Objective Edge List for Adjacency Network
edges <- data.frame(user_id=as.integer(),                          # Student Identifier
                    from=as.character(),                           # Learning Objective - Source
                    f.mod_id=as.character(),                       # Content Module - Source
                    to=as.character(),                             # Learning Objective - Target 
                    t.mod_id=as.character(),                       # Content Module - Target
                    time=as.POSIXct(as.character()),               # Date Time Stamp
                    stringsAsFactors=FALSE)

## Analyzing Student Logs length(filePaths)
# Loop individual student logs to create an edge list of transitions between learning objectives
for(i in 1:length(filePaths)){
  message("Processing log file ", i, " of ", length(filePaths))
  print(proc.time() - start)
  ## Load data set
  data <- read.csv(filePaths[i])[,c("course_id","user_id","mod_hex_id","time","period",
                                     "attempts","grade","max_grade","success")]
  # Renames field to join with Learning Objectives 
  names(data)[3] <- "mod_id"
  
  #Updates Time field to R compliant format
  data$time  <- as.POSIXct(data$time,origin="1970-01-01") 

  # Left join learning objectives for content modules (lo_c) to student log (data)
  data <- join(as.data.frame(data),as.data.frame(lo_c[,c("mod_id","objective")]),by="mod_id")
  
  # Remove all events that are not associated with an objective
  data <- data[!is.na(data$objective),]

  # Learning Objective Dwell Time and Event Count Calculations
  tmp <- data %>% ddply(.,~objective,summarise,
                        events = length(period),
                        dwell = sum(period),
                        create = min(time),
                        modified = max(time),
                        user_id = unique(user_id)) %>% 
                  join(lo_id,.,by="objective") %>%
                  replace_na(list(dwell=0,
                                  events=0,
                                  user_id=unique(data$user_id))) 

  # Learning Objecitve Problem Attempts and Problem Dwell Time
  tmp <- data %>% filter(attempts == !is.na(attempts)) %>%                      # Filter to all problem modules
                  ddply(.,~objective,summarise,                                 # Calculates attempts at a problem mod
                        attempts = length(attempts),                            # Problem dwell time using period field in logs
                        prb_dwell = sum(period),
                        user_id = unique(user_id)) %>%
                  join(tmp,.,by=c("objective","user_id")) %>%
                  replace_na(list(attempts=0,
                                  prb_dwell=0)) %>%
                  select(1:5,7:8,6)
  dwell <- rbind(dwell,tmp)
  
  #Adjacency Edge List
  if(nrow(data)>1){
    ## Edge List Created
    tmp <- data[,c("user_id","objective","mod_id","time","period")] 
    #Creates transitions in targets in edge file
    tmp <- cbind(tmp[1:(nrow(tmp)-1),c(1:3,5)],tmp[2:nrow(tmp),c(2:4)])
    #Renames fields
    names(tmp) <- c("user_id","from","f.mod_id","period","to","t.mod_id","time")
    tmp <- tmp[,c(1:3,5:7,4)]
    #Adds self loops flag, and reorganizes the columns
    tmp$sl <- 0
    if(selfLoopKeep==T){
      if(length(tmp[tmp$to==tmp$from,]$sl)>=1){
        tmp[tmp$to==tmp$from,]$sl <- 1
      }
    } else {
      tmp <- tmp[tmp$to==tmp$from,]
    }
  }
  edges <- rbind(edges,tmp)
}

#Clearn up environment
rm(i,tmp,data,selfLoopKeep,filePaths,users)



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
# write.csv(grade_s, file=paste0(path_output,"/grade-stats.csv"),row.names = F)
# write.csv(grades, file=paste0(path_output,"/grade.csv"),row.names = F)
# write.csv(dwell_s, file=paste0(path_output,"/dwell-stats.csv"),row.names = F)
# write.csv(dwell, file=paste0(path_output,"/dwell.csv"),row.names = F)
# write.csv(edges[,c(1,2,4,6)], file=paste0(path_output,"/edges.csv"),row.names = F)
# write.csv(edges_s,file=paste0(path_output,"/edge-weighted.csv"), row.names = F)

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


#heatmap(ppois(ha,5))
res.




#### Finishing Details ####
#Indicate completion
message("\n**** Complete! ****\n")
## Script processing time feedback
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print((proc.time()[3] - start[3])/60)
## Clear environment variables
rm(list=ls())