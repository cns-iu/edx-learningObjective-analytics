# edX Learning Objective Visual Analytics Workflow
The edX Learning Objective Visual Analytics R Scripts is a processing pipeline used to analyze edX course and student level data in the context of learning objectives identified by instructional designer. The scripts leverage course level data provided in an edX Data Package archive, and a learning objective analysis of an edX course structure.

## Related Publications and Protocol
Analysis and visualization results are described in the publication:

> CITATION

A protocol describing how to use data processing, analysis and visualizations scripts in the workflow is available at Protocols.io:

> [edX Learning Objective Visual Analytics Workflow, V.1, dx.doi.org/....](). 

## Scripts Description
The edX Learning Objective Visual Analytics Worklflow supports analysis of edX course and student level data in the context of learning objectives identified by instructional designer. The scripts leverage edX course database and user logs provided in an edX Data Package archive, and a learning objective analysis of an edX course structure.

*	**edX-learningObjective-1-dataProcessing.R** ((https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/edX-learningObjective-1-dataProcessing.R)[https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/edX-learningObjective-1-dataProcessing.R]) script associates student grade performance data and pre-processed student content interactions (event logs) with learning objectives that were coded to an edX courses' course structure to analyze student interaction (dwell times & event counts) and grade performance towards a course's learning objectives
* **edX-learningObjective-2-gradeTime-boxplots.R** ((https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/edX-learningObjective-2-gradeTime-boxplots.R)[https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/edX-learningObjective-2-gradeTime-boxplots.R]) script creates boxplot visualizations of student grade performance and activity data across learning objectives associated with an edX course.
* **edX-learningObjective-3-bipartitenetwork.R script** ((https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/edX-learningObjective-3-bipartitenetwork.R)[https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/edX-learningObjective-3-bipartitenetwork.R]) creates a bipartite network between chapter modules and learning objective for an edX course, specifically, a node and edge list that may be used in secondary analytics tools, such as Gephi. The script also exports a color palette set of web color hexadecimal codes used in the final visualization of this network.

## Sample Data
Sample data is provided in the project 'data' directory, and includes anonymized edX course data, and data processing results that were generated by the scripts found in this workflow. Data sets provided, include:

* **Data A** ([https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataA.csv](https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataA.csv)) provides an example processed edX course structure that was modified and used for coding learning objectives to content modules.
* **Data B** ([https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataB.csv](https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataB.csv)) provides an example of learning objectives coded to the modified edX course structure described above.
* **Data C** ([https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataC.csv](https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataC.csv)) is an example of a list of anonymized student identifiers that replace original edX user identifiers.
* **Data D** ([https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataD.csv](https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataD.csv)) is an example of the edX course grades_persistentsubsectiongrade.csv data set; note, that user identifiers have been anonymized and matched to the idenitifiers found in Data C.
* **Data E** ([https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataE.zip](https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataE.zip)) is a Zip file that contains processed edX event logs for each active student from the edX course analyzed in this study; note, that user identifiers have been anonymized and matched to the identifiers found in filenames of Data C. 
* **Data F** ([https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataF.csv](https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataF.csv)) is an example of the analysis results produced by the script using the script, [edX-learningObjective-1-dataProcessing.R](https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/edX-learningObjective-1-dataProcessing.R)
* **Data G** ([https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataG.zip](https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/data/dataG.zip)) is a ZIP file that archives three data sets produced by results produced by the script using the script, [edX-learningObjective-3-bipartitenetwork.R](https://github.com/cns-iu/edx-learningObjective-analytics/blob/master/edX-learningObjective-3-bipartitenetwork.R), and the resulting Gephi network visualization:
   * *DataG-nodes.csv* is a list of nodes found in the network, their node group (chapters and learning objective group), a label, x and y coordinates that provide initial graph layout, and a node ID that references the edge list source and targets.
   * *DataG-edges.csv* is an edge list that details relationships between nodes; the file contains the source (chapters) and targets (learning objectives); a value weight that is set to 1; and a learning objective group label.
   * *DataG-palette.csv* is a set of web hexadecimal codes that are associated with the node groups; learning objective color assignments match the colors used in figures 1 and 2.
   * *DataG-network.gexf* is an network generated by Gephi that represents the relationships between an edX course's chapter modules and learning objectives. 

## A Note on Using the edX Learning Objective Visual Analytics Workflow R Scripts
The processing scripts are provided under Apache License 2.0. Contributors provide permission for commercial use, modification, distribution, patent use, and private use.  Licensed works, modifications, and larger works may be distributed under different terms and without source code. The scripts are provided with a limited liability and warranty; use these data processing scripts at your own discretion, and make preservation copies of any source data prior to use.

This workflow requires that a user initially process their edX course data using scripts found in the GitHub repository: **[edX Learner and Course Analytics and Visualization Pipeline](https://github.com/cns-iu/edx-learnertrajectorynetpipeline)**. Use of this repository is detailed in a protocol available at Protocols.io: **[edX Learner and Course Analytics and Visualization Pipeline, V.5, dx.doi.org/10.17504/protocols.io.be9ijh4e](https://www.protocols.io/view/edx-learner-and-course-analytics-and-visualization-be9ijh4e)**

This data processing pipeline is further described in the publication:

> **Ginda M, Richey MC, Cousino M, Börner K. (2019). Visualizing learner engagement, performance, and trajectories to evaluate and optimize online course design.  PLOS ONE  14(5): e0215964.**
> **[https://doi.org/10.1371/journal.pone.0215964](https://doi.org/10.1371/journal.pone.0215964)**

Additional modifications are likely needed to make use of these workflows when processing your course datasets that use the edX Data Package format specification. Organizational implementation of the edX learning management systems may use customized event log tracking systems, courses may use different types of edX block modules, and logs may include types of events that were not encountered in this project (e.g. error events, or edX discussion forums). An exploratory analysis of the course structure and event logs is advisable at the outset of a project.
