# edX Learning Objective Trajectory Network Analysis and Visualization R Scripts
The edX Learner and Course Analytics and Visualization Pipeline is a R script processing pipeline used to with course and student level data from an edX course database or edX Data Package. The pipeline was designed to:

* extract and process course structures, learners’ event logs, demographic and performance data;
* create learner trajectory networks (see [Visualizations of learning trajectories in online courses (cns-iu/learning-trajectories)](https://github.com/cns-iu/learning-trajectories) for example visualization implementation);
* analyze the learner use of course content modules; and
* analyze overall performance and interaction measurements for a given course.

## Protocol
A generalize protocol is available at Protocols.io, [edX Learner and Course Analytics and Visualization Pipeline, V.3](https://www.protocols.io/view/edx-learner-and-course-analytics-and-visualization-zckf2uw) [dx.doi.org/10.17504/protocols.io.zfhf3j6](dx.doi.org/10.17504/protocols.io.zfhf3j6).

The protocol covers how to use the scripts in this workflow

## Script Workflow Description


## Sample Data
Sample data sets are provided in the **[Data](https://github.com/cns-iu/edx-learnertrajectorynetpipeline/tree/master/data)** directory, which were created by the processing and analysis scripts described above. A short index of these files is available to review at the Rmarkdown documentation site **[Sample Data Index](https://cns-iu.github.io/edx-learnertrajectorynetpipeline/index.html)**.


## Analysis and Visualization Documentation Notebooks


## A Note on Using the Learner Objective Trajectory Network Analysis and Visualization R Scripts
The processing scripts are provided under Apache License 2.0. Contributors provide permission for commercial use, modification, distribution, patent use, and private use.  Licensed works, modifications, and larger works may be distributed under different terms and without source code. The scripts are provided with a limited liability and warranty; use these data processing scripts at your own discretion, and make preservation copies of any source data prior to use.

Additional modifications are likely needed to make use of this pipeline when processing other course datasets that use the edX Data Package format specification. Organizational implementation of the edX learning management systems may use customized event log tracking systems, courses may use different types of edX block modules, and logs may include types of events that were not encountered in this project (e.g. error events, or edX discussion forums). An exploratory analysis of the course structure and event logs is advisable at the outset of a project.
