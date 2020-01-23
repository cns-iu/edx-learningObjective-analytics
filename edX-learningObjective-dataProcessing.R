## ====================================================================================== ##
# Title: Processing edX Learner Performance and Interaction Data to Learning Objectives
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
# Description: This R data processing scripts translates learner grade performance and 
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

# Load required packages
library(tidyr)      # for data preparation
#library(stringr)    # for string manipulation
library(plyr)       # for data aggregations
library(dplyr)      # for filtering
library(reshape2)   # for data reshaping
library(igraph)     # for creating an adjacency matrix

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

## Setting Learning Objective Identifiers
lo_id <- as.data.frame(unique(lo$objective))
names(lo_id) <- "objective"

## Mapping Learning Objectives to Sequential Modules
# Subset to only values 1, indicating a learning objective was observed, and tree levels of 2 (sequential modules), 
# then refactor identifiers
lo_s <- lo[lo$value==1 & lo$treelevel==2,c("mod_id","mod_type","objective")] %>% 
  mutate(mod_id = factor(mod_id)) %>% mutate(mod_type = factor(mod_type))

## Mapping Learning Objectives to Content Modules
# Subset to only values 1, indicating a learning objective was observed, and tree levels of 4 (content modules), 
# then refactor identifiers
lo_c <- lo[lo$value==1 & lo$treelevel==4,c("mod_id","mod_type","seqModPar","objective")] %>% 
  mutate(mod_id = factor(mod_id)) %>% mutate(mod_type = factor(mod_type))

#### Learning Objective Grades ####
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

#### Learning Objective Dwell Times ####
dwell  <- data.frame(objective=as.character(),                     # Learning Objective
                     dwell=as.numeric(),                           # Dwell Time (Min)
                     events=as.numeric(),                          # Event Count
                     created=as.POSIXct(as.character()),  # First Event Log Created
                     modified=as.POSIXct(as.character()), # Last Modification to Event Log
                     user_id=as.integer(),                         # Student Identifier
                     stringsAsFactors=FALSE)

#### Creating Student Learning Objective Transition Network Edge List ####
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

## Converting Student Event Logs to create Learning Objectives Transition Networks 
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
  tmp <- data %>% filter(attempts == !is.na(attempts)) %>%        # Filter to all problem modules
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

## Analysis Results Output
write.csv(grades, file=paste0(path_output,"/grade.csv"),row.names = F)
write.csv(dwell, file=paste0(path_output,"/dwell.csv"),row.names = F)
write.csv(edges[,c(1,2,4,6)], file=paste0(path_output,"/edges.csv"),row.names = F)

#### Finishing Details ####
#Indicate completion
message("\n**** Complete! ****\n")
## Script processing time feedback
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print((proc.time()[3] - start[3])/60)
## Clear environment variables
rm(list=ls())