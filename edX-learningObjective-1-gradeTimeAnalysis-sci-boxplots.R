## ====================================================================================== ##
# Title:        Student's engagement & performance on learning objective for edX courses
# Project:      edX learner objective - student time and grade analysis
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
# Description: 
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
#           clarified inline documentation; generalized script for any edX course; split
#           out visualizations into a separate script.
#
## ====================================================================================== ##
#### Environmental Setup ####
rm(list=ls()) 
#rm(LOs,stdLOevent,stdLOsGrades,stdLOs,stdLOtemp,crsGrades,certGrplabs,certGrpLabs,tmp)

#Load required packages 
require("tcltk2")     #for OS independent GUI file and folder selection
require("plyr")       #for aggregation of data sets
require("reshape2")   #for reshaping data sets
require("magrittr")   #for piping 
require("stringr")    #for string parsing
require("ggplot2")    #for visualization
require("GGally")

#### Paths #### 
# Generic Set up
#Checks if a user has previously assign a path with a prior script 
#If false, lets user assign path to directory to read in a course'
#data from the edX data package
if(exists("path_data")==FALSE){
  path_data = tclvalue(tkchooseDirectory())
}

#Checks if a user has previously assign a path with a prior script 
#If false, lets user assign path to previous processing output files of an
#edX course using the a previous processing scripts from this pipeline.
if(exists("path_output")==FALSE){
  path_output = tclvalue(tkchooseDirectory())
}

#### Load Data ####
# Course Module with Learning Objective Mapping Analysis
mods <- read.csv(file=list.files(full.names = TRUE, recursive = FALSE, 
                                 path = paste0(path_output,"/learningObjectives/"),
                                 pattern = "module-lo-lookup-sci.csv$"), header=T)
# Student course grade
crsGrades <- read.csv(file=list.files(full.names = TRUE, recursive = FALSE, 
                                    path = paste0(path_output,"/analysis/studentActivity"),
                                    pattern = "selectedActivity.csv$"),header=T)[c(1,6,7,11,12)]
# Student course subsection grades - measured at the sequencial group level of course structure
subGrades <- read.csv(file=list.files(full.names = TRUE, recursive = FALSE, 
                                      path = paste0(path_data,"/state/"),
                                      pattern = "grades_persistentsubsectiongrade.csv$"), header=T)[c(2:7)]
# A list of student learner course module analysis results files, contains module dwell time and final module grade (if applicable)
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = paste0(path_output,"/networks/nodes/"),
                       pattern = "-nodes.csv$")

#### Data Preparation ####

### Student Learning Objective Stats
## Student pass/fail data to stdLOStats
stdLOstats <- join(stdLOstats,crsGrades[,c(1,4)],by="user_id")

## Std LO time calculations
stdLOtemp <- dcast(stdLOstats,user_id~learningObjective, value.var="totalTime", sum)
tabCols <- ncol(stdLOtemp)
stdLOtemp <- join(stdLOtemp,crsGrades,by="user_id")
stdLOtemp <- stdLOtemp[,c(1,(tabCols+1):(tabCols+ncol(crsGrades)-1),2:tabCols)]

# ## Std LO event calculations
# stdLOevent <- dcast(stdLOstats,user_id~learningObjective, value.var="events", sum)
# tabCols <- ncol(stdLOevent)
# stdLOevent <- join(stdLOevent,crsGrades,by="user_id")
# stdLOevent <- stdLOevent[,c(1,(tabCols+1):(tabCols+ncol(crsGrades)-1),2:tabCols)]

## Std LO Grades from problem modules and open assessments
stdLOgrade <- dcast(stdLOstats,user_id~learningObjective, value.var="percentPts", max)
tabCols <- ncol(stdLOgrade)
stdLOgrade <- join(stdLOgrade,crsGrades,by="user_id")
stdLOgrade <- stdLOgrade[,c(1,(tabCols+1):(tabCols+ncol(crsGrades)-1),2:tabCols)]

### Saves data used for Box Plots
write.csv(stdLOs, file=paste0(path_output,"/analysis/",subDir[1],"/results/",courseID,"-LearningObjectives-studentAnalysis-long.csv-sci"), 
          row.names=F) 

### Saves data used for Parallel Coordinate Charts
# Student Learning Objective Temp. Duration Analysis Results
write.csv(stdLOtemp,  file=paste0(path_output,"/analysis/learningObjectives/results/",courseID,"-LearningObjectives-tempsAnalysis-wide-sci.csv"), 
          row.names=F) 
# # Student Learning Objective Event Analysis Results
write.csv(stdLOevent, file=paste0(path_output,"/analysis/learningObjectives/results/",courseID,"-LearningObjectives-eventAnalysis-wide-sci.csv"), 
          row.names=F) 
# Student Learning Objective Grade Analysis Results
write.csv(stdLOgrade, file=paste0(path_output,"/analysis/learningObjectives/results/",courseID,"-LearningObjectives-gradeAnalysis-all-wide-sci.csv"), 
          row.names=F) 

# Not all LOs may have graded content, this loop checks for these columns for removal
## Checks if tmp has data
# temp data frame for field list
tmp <- NA
# Tests for fields having grade data, if field is all NA, it is removed
for(i in 1:ncol(stdLOgrade)){
  # Checks to see if field is numeric 
  if(is.numeric(stdLOgrade[,i])==T){
    # Checks if field has non-NA values
    if(length(stdLOgrade[is.na(stdLOgrade[,i])==F,i])==0){
      # Checks to see if tmp data frame has been used
      if(is.na(unique(tmp[1]))==T){
        tmp <- i
      } else {
        tmp <- append(tmp, i) 
      }
    }
  }
}
if(is.na(unique(tmp[1]))==F){
  stdLOgrade <- stdLOgrade[,-tmp]
}

#### Visualization ####
strip <- c("#DCDCDC") #Grey
theme_set(theme_light())

### Box plots for LO distributions
## Events
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/boxplots-lo-events-colLOgrp.png"),
    width = 13, height = 7, units = "in", pointsize = 8, res=300,
    type="cairo", bg = "white")
ggplot(data=stdLOs[as.character(stdLOs$group)!=c("LO 7") & 
                     as.character(stdLOs$group)!=c("LO 8") &
                     as.character(stdLOs$group)!=c("LO 9"), ],
       aes(learningObjective,events)) +
    geom_boxplot(aes(fill=group)) +
    ylim(0,750) + 
#    facet_grid(cert_status~.) +
    xlab("Learning Objectives") + ylab("Events (Student click actions)") +
    guides(fill=guide_legend(title = "Learning Objective Group")) + theme(legend.position="bottom")
dev.off()

## Events per module
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/boxplots-lo-eventMod-colLOgrp.png"),
    width = 13, height = 7, units = "in", pointsize = 8, res=300,
    type="cairo", bg = "white")
ggplot(data=stdLOs[as.character(stdLOs$group)!=c("LO 7") & 
                     as.character(stdLOs$group)!=c("LO 8") &
                     as.character(stdLOs$group)!=c("LO 9"), ],
       aes(learningObjective,eventMod)) +
    geom_boxplot(aes(fill=group)) +
    ylim(0,75) + 
#    facet_grid(cert_status~.) +
    xlab("Learning Objectives") + ylab("Events per Module Used (Student click actions)") +
    guides(fill=guide_legend(title = "Learning Objective Group")) 
dev.off()

## Total Time Duration
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/boxplots-lo-totaldur-colLOgrp.png"),
    width = 13, height = 7, units = "in", pointsize = 8, res=300,
    type="cairo", bg = "white")
ggplot(data=stdLOs[as.character(stdLOs$group)!=c("LO 7") & 
                     as.character(stdLOs$group)!=c("LO 8") &
                     as.character(stdLOs$group)!=c("LO 9"), ],
       aes(learningObjective,totalTime)) +
  geom_boxplot(aes(fill=group)) +
  ylim(0,1000) + 
  #    facet_grid(cert_status~.) +
  xlab("Learning Objectives") + ylab("Total Duration (Min.)") +
  guides(fill=guide_legend(title = "Learning Objective Group")) + theme(legend.position="bottom")
dev.off()

## Duration per Event
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/boxplots-lo-duration-colLOgrp.png"),
    width = 13, height = 7, units = "in", pointsize = 8, res=300,
    type="cairo", bg = "white")
ggplot(data=stdLOs[as.character(stdLOs$group)!=c("LO 7") & 
                     as.character(stdLOs$group)!=c("LO 8") &
                     as.character(stdLOs$group)!=c("LO 9"), ],
       aes(learningObjective,timeEvent)) +
    geom_boxplot(aes(fill=group)) + 
    ylim(0,15) +
#    facet_grid(cert_status~.) +
    xlab("Learning Objectives") + ylab("Event Duration (Min.)") +
    guides(fill=guide_legend(title = "Learning Objective Group")) + theme(legend.position="bottom")
dev.off()

## Percent Grade Earned 
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/boxplots-lo-pergrade-colLOgrp.png"),
    width = 13, height = 7, units = "in", pointsize = 8, res=300,
    type="cairo", bg = "white")
ggplot(data=stdLOs[as.character(stdLOs$group)!=c("LO 7") & 
                     as.character(stdLOs$group)!=c("LO 8") &
                     as.character(stdLOs$group)!=c("LO 9"), ],
       aes(learningObjective,perEarned)) +
    geom_hline(yintercept=65, color="dark red", linetype = 3, size = 1) +
    geom_boxplot(aes(fill=group)) + 
    ylim(0,101) +
    xlab("Learning Objectives") + ylab("Grades (Percentage)") +
    guides(fill=guide_legend(title = "Learning Objective Group")) + theme(legend.position="bottom")
dev.off()

# Faceted
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/boxplots-lo-pergrade-colLOgrp-facetCertStat.png"),
    width = 13, height = 7, units = "in", pointsize = 8, res=300,
    type="cairo", bg = "white")
ggplot(data=stdLOs[as.character(stdLOs$group)!=c("LO 7") & 
                     as.character(stdLOs$group)!=c("LO 8") &
                     as.character(stdLOs$group)!=c("LO 9"), ],
       aes(learningObjective,perEarned)) +
    geom_hline(yintercept=65, color="dark red", linetype = 3, size = 1) +
    geom_boxplot(aes(fill=group)) + 
    ylim(0,101) +
    facet_grid(cert_status~.) +
    xlab("Learning Objectives") + ylab("Grades (Percentage)") +
    guides(fill=guide_legend(title = "Learning Objective Group")) + 
    theme(legend.position="bottom",
          strip.background = element_rect(fill=strip),
          strip.text=element_text(color = "Black"))
dev.off()

### Parallel Coordinate Charts of students across LO variables
### LO and Grades - W Boxplots
## Colour is year of birth
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-grade-certgrp-stdscl-colyob-boxplot.png"),
    width = 11, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOgrade, columns=c(8:33), groupColumn = 3, scale="std", boxplot=T, alphaLines = .33) + 
  #facet_grid(cert_status~.) + 
  geom_hline(yintercept=0, color="dark red", linetype = 2, size = 1.25) +
  xlab("Learning Objectives") + ylab("Grades (Scaled Std. Dev)") + 
  guides(colour=guide_legend(title = "Year of Birth")) +
  theme(legend.position="bottom",
        strip.background = element_rect(fill=strip),
        strip.text=element_text(color = "Black"))
dev.off()

### LO and Grades
## Colour is year of birth
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-grade-certgrp-stdscl-colyob.png"),
    width = 11, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOgrade, columns=c(8:33), groupColumn = 3, scale="std", alphaLines = .5) + 
  #facet_grid(cert_status~.) + 
  xlab("Learning Objectives") + ylab("Grades (Scaled Std. Dev)") + 
  guides(colour=guide_legend(title = "Year of Birth")) + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill=strip),
        strip.text=element_text(color = "Black"))
dev.off()

### LO and Grades - Boxplot
## Colour is percentile Grade
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-grade-certgrp-stdscl-colpercentile-boxplot.png"),
    width = 11, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOgrade, columns=c(8:33), groupColumn = 6, scale="std", boxplot=T, alphaLines = .25) + 
  #facet_grid(cert_status~.) + 
  xlab("Learning Objectives") + ylab("Grades (Scaled Std. Dev)") + 
  guides(colour=guide_legend(title = "Percentile Grade")) + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill=strip),
        strip.text=element_text(color = "Black"))
dev.off()

### LO and Grades
## Colour is percentile Grade
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-grade-certgrp-stdscl-colpercentile.png"),
    width = 11, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOgrade, columns=c(8:33), groupColumn = 6, scale="std", alphaLines = .5) + 
  #facet_grid(cert_status~.) + 
  xlab("Learning Objectives") + ylab("Grades (Scaled Std. Dev)") + 
  guides(colour=guide_legend(title = "Percentile Grade"))+ 
  theme(legend.position="bottom",
        strip.background = element_rect(fill=strip),
        strip.text=element_text(color = "Black"))
dev.off()



### LO and Events 
## with BoxPlots
## Colour by percentile grade
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-event-certgrp-unvar-colpercentile-boxplot.png"),
    width = 12, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOevent, columns=c(8:40), groupColumn = 6, scale="uniminmax", alphaLines = .15, boxplot = T) + 
  #facet_grid(cert_status~.)+
  xlab("Learning Objectives") + ylab("Grades (Scaled Uni Var, Min/Max)") + 
  guides(colour=guide_legend(title = "Percentile Grade"))+ 
  theme(legend.position="bottom")
dev.off()

## without BoxPlots
## Colour by percentile grade
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-event-certgrp-unvar-colpercentile.png"),
    width = 12, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOevent, columns=c(8:40), groupColumn = 6, scale="uniminmax", alphaLines = .5) + 
  #facet_grid(cert_status~.)+
  xlab("Learning Objectives") + ylab("Events (Scaled Uni Var, Min/Max)") +
  guides(colour=guide_legend(title = "Percentile Grade")) + 
  theme(legend.position="bottom")
dev.off()




### LO and temporal duration ###
## with BoxPlots
## Colour by percentile grade
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-duration-certgrp-unvar-colpercentile-boxplot.png"),
    width = 12, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOtemp, columns=c(8:40), groupColumn = 6, scale="uniminmax", alphaLines = .25,boxplot = T) + 
  #facet_grid(cert_status~.)+ 
  xlab("Learning Objectives") + ylab("Duration (Scaled Uni Var, Min/Max)") +
  guides(colour=guide_legend(title = "Percentile Grade")) + 
  theme(legend.position="bottom")
dev.off()

## without BoxPlots
## Colour by percentile grade
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-duration-certgrp-unvar-colpercentile.png"),
    width = 12, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOtemp, columns=c(8:40), groupColumn = 6, scale="uniminmax", alphaLines = .5) + 
  #facet_grid(cert_status~.)+
  xlab("Learning Objectives") + ylab("Duration (Scaled Uni Var, Min/Max)") +
  guides(colour=guide_legend(title = "Percentile Grade")) + 
  theme(legend.position="bottom")
dev.off()

## with BoxPlots
## Colour by year of birth
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-duration-certgrp-unvar-colyob-boxplot.png"),
    width = 12, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOtemp, columns=c(8:40), groupColumn = 3, scale="uniminmax", alphaLines = .25,boxplot = T) + 
  #facet_grid(cert_status~.)+
  xlab("Learning Objectives") + ylab("Duration (Min.) (Scaled Uni Var, Min/Max)") +
  guides(colour=guide_legend(title = "Year of Birth")) + 
  theme(legend.position="bottom")
dev.off()

## without BoxPlots
## LO and temporal duration
png(filename=paste0(path_output,"/analysis/",subDir[1],"/visualizations/parcoord-lo-duration-certgrp-unvar-colyob.png"),
    width = 12, height = 7, units = "in", pointsize = 10, res=300,
    type="cairo", bg = "white")
ggparcoord(stdLOtemp, columns=c(8:40), groupColumn = 3, scale="uniminmax", alphaLines = .5) + 
  #facet_grid(cert_status~.)+ 
  xlab("Learning Objectives") + ylab("Duration (Min.) (Scaled Uni Var, Min/Max)") +
  guides(colour=guide_legend(title = "Year of Birth")) + 
  theme(legend.position="bottom")
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