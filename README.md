# edX Learning Objective Visual Analytics Workflow
The edX Learning Objective Visual Analytics R Scripts is a processing pipeline used to analyze edX course and student level data in the context of learing objectives identified by instructional designer. The scripts leverage edX Data Package data sets and a learning objective analysis matrix. 

## Protocol
A generalize protocol is available at Protocols.io, [edX Learning Objective Visual Analytics Workflow, V.1](), [dx.doi.org/....](). The protocol covers how to use data processing, analysis and visualizations scripts in the workflow. 

## Sample Data


## A Note on Using the edX Learning Objective Visual Analytics Workflow R Scripts
The processing scripts are provided under Apache License 2.0. Contributors provide permission for commercial use, modification, distribution, patent use, and private use.  Licensed works, modifications, and larger works may be distributed under different terms and without source code. The scripts are provided with a limited liability and warranty; use these data processing scripts at your own discretion, and make preservation copies of any source data prior to use.

This workflow relies initially processing course data using data processing protocol available at Protocols.io

> [edX Learner and Course Analytics and Visualization Pipeline, V.5, dx.doi.org/10.17504/protocols.io.be9ijh4e](https://www.protocols.io/view/edx-learner-and-course-analytics-and-visualization-be9ijh4e)

The protocol is further described in the publication:

> Ginda M, Richey MC, Cousino M, Börner K. (2019). Visualizing learner engagement, performance, and trajectories to evaluate and optimize online course design.  PLOS ONE  14(5): e0215964.
> [https://doi.org/10.1371/journal.pone.0215964](https://doi.org/10.1371/journal.pone.0215964)

Additional modifications are likely needed to make use of these workflows when processing your course datasets that use the edX Data Package format specification. Organizational implementation of the edX learning management systems may use customized event log tracking systems, courses may use different types of edX block modules, and logs may include types of events that were not encountered in this project (e.g. error events, or edX discussion forums). An exploratory analysis of the course structure and event logs is advisable at the outset of a project.