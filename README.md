# nash-zero
> Facilitating landfill neutrality by 2050

# Quick navigation
[Background](#background)  
[Data](#data)  
[Models](#models)  
[Timeline](#timeline-and-deliverables)  
[Logistics](#project-logistics)  
[Resources](#resources)  
[Contributing](#contributing)  
[Contact Info](#contact-info)

# Goal
Metro Nashville has set a goal to be solid landfill neutral by 2050. As a rapidly growing city, one of the major sources of solid waste is through development (i.e., construction) and re-development of targeted areas, and renovation of existing housing and infrastructure. The objective of this project is to predict the volume of solid waste based development, teardowns permits, and in the future - based on economic growth - gentrification. This will enable examination of the impact of various operational decisions for informed policy-making. Specifically:
* Predict the tonnage of construction and demolition waste for a fiscal year (July 1 - June 30)
* Predict the tonnage of C&D waste for each material for a fiscal year
* [Will need additional data] Use model to investigate the effect of policies and current recycling capabilities
* [Reach] Create calculator which will assistant contractors in estimating how much recycling may be produced by their project in the context of financial results

# Background  
In 2017, Nashville Public Works initiated a Zero Waste plan to minimize the amount of waste generated in Nashville, and maximize the recycling and reuse of recyclable materials.  The term "zero waste" refers to the objective of diverting 90% of materials from landfills, and instead, viewing waste as a resource to be recycled, reduced, reused, composted, or converted to other biomaterials such as fuel.  One of the current major contributors to waste is due to Nashville's positive growth in infrastructure, population, and construction; however, this introduces an opportunity to positively effect the zero waste objective by forming policies to facilitate recycling and reuse during these activities.  Strategies around encouraging recycling of materials have been proposed, and a quantitative model which can simulate the effect of enacting policies will help to elucidate good policies to meet the objective of zero waste Nashville by 2050.  More information about Nashville's objective to achieve zero waste can be found in the [Solid Waste Master Plan](https://www.nashville.gov/Portals/0/SiteContent/pw/docs/recycle/MasterPlan/SWMP%20ES_Final.pdf), from the Solid Waste Region Board [website](https://www.nashville.gov/Government/Boards-and-Committees/Committee-Information/ID/96/Solid-Waste-Region-Board.aspx), from the Nashville Development Services [website](https://www.nashville.gov/Government/Development-Services/Development-Process.aspx), and on the Nashville Zero Waste [website](recycle.nashville.gov).

# Data

The data will include publicly accessible and custom generated construction permit data from Nashville. One source of data is the Building Permits Issued dataset from Nashville [Open Data Portal](https://data.nashville.gov/Licenses-Permits/Building-Permits-Issued/3h5w-q8b7). Additionally, construction, waste, landfill, and recycling information collected from similar cities will be used for modeling as the data for Nashville is collected.  The data is currently located on a shared Box directory named `nash-zero`.  If you haven't received an invitation, please request access from the PIs as soon as possible.

## Data security

There are no anticipated security concerns.

## Counts

See Box folder 20-series for more information about the counts of the current data.  Permit data exists for Nashville and peer cities of Austin and San Francisco.  Austin and San Francisco additionally have the outcome data which will be adapted to be represenative of Nashville projected tonnage data.

# Models

The model here will be contained within a final deliverable framework for interactivity and forecasting.  Two sets of models are employed: first for the prediction of future number of permits (ARIMA models and machine learning models through the modelTime package), and secondly, for the prediction of waste tonnage (linear and nonlinear models) through tidymodels.

# Timeline and Deliverables

The deliverable for this project is the repository and code, cleaned and organized data, the model, and a Shiny dashboard for simulation and interactivity with the model.

Start of Project: June 7th, 2021

Project Deadline: August 13th, 2021

# Repo Structure

The repo is structured as follows: All *0- (e.g., 10-, 20-, 30-) files contain finalized work for the purpose described (e.g., "process-data"). Subfiles related to the task (e.g., 11-, 12-) should be created in order to explore and document relevant or interesting subtasks.

All files which appear in the repo should be able to run, and not contain error or blank cell lines, even if they are relatively midway in development of the proposed task. All notebooks relating to the analysis should have a numerical prefix (e.g., 31-) followed by the exploration (e.g. 31-text-labeling). Any utility notebooks should not be numbered, but be named according to their purpose. All notebooks should have lowercase and hyphenated titles (e.g., 10-process-data not 10-Process-Data). All notebooks should adhere to literate programming practices (i.e., markdown writing to describe problems, assumptions, conclusions) and provide adequate although not superfluous code comments.

# Project logistics

**Sprint planning**:  Tuesdays from 9:30am - 11:00am at [Zoom Link](https://vanderbilt.zoom.us/j/99186861219?pwd=TTdSc1kyTlFMbWh1dnB4eHU0ZzZ0Zz09)  
**Demo**:  Fridays from 2:00 to 3:00 at [Zoom Link](https://vanderbilt.zoom.us/j/93077638038?pwd=K09YMWVnWGtpSzFUNCt0UXFlWG5RQT09&from=addon)

**Coder meeting**: Thursdays from 2:00 to 3:00 at [Zoom Link](https://vanderbilt.zoom.us/j/99186861219?pwd=TTdSc1kyTlFMbWh1dnB4eHU0ZzZ0Zz09)

**Data location**:  nash-zero Box (please email one of the PIs or DS lead for access)  

**Slack channel**: https://app.slack.com/client/T6C2NS7HC/C023T9H0URH (please email DS lead for access) 

**Zoom link**:  https://vanderbilt.zoom.us/j/99186861219?pwd=TTdSc1kyTlFMbWh1dnB4eHU0ZzZ0Zz09    

# Resources

* **R Basics**: [RStudio Primers](https://rstudio.cloud/learn/primers/)
* **Data Science with R and Tidyverse**: [R for Data Science, Hadley Wickham](https://r4ds.had.co.nz/)
* **ACCRE how-to guides**: [DSI How-tos](https://github.com/vanderbilt-data-science/how-tos)
* **Git tutorials**: [Simple Guide](https://rogerdudler.github.io/git-guide/), [Learn Git Branching](https://learngitbranching.js.org/?locale=en_US)
* **Brainstorm Slide Deck for Deliverable:**: [Strategy for generating predictive models](https://docs.google.com/presentation/d/177_R1UemhpJLxi2yHq0Qx19diFxgFcupaf4DIuT9LlI/edit?usp=sharing)
* **City Columns/Variable standardization table**: On Box as `~/Box/nash-zero/city_variable_crosswalk.xlsx`  
* **San Francisco permits information:** [All structual permits information](https://data.sfgov.org/Housing-and-Buildings/Building-Permits/i98e-djp9), [Interface for obtaining permits](https://sf.gov/topics/building-permits), [Form number meanings](https://sf.gov/learn-about-our-building-permit-review-processes), [Department of builidng inspection FAQ](https://sfdbi.org/frequently-asked-questions), [Resubmit plans for a building permit application]( https://sf.gov/resubmit-plans-building-permit-application), [Steps to get building permit](https://sf.gov/step-by-step/get-building-permit)
* **Green Halo Instructions for Projects:** [C&D Recycling Plan Step-by-Step Instructions Using Green Halo](https://www.toaks.org/home/showpublisheddocument?id=27644)
* **San Francisco Recovery Plan and Information:** [Material Reduction and Recovery Plan](https://sfdbi.org/sites/default/files/IS%20GB-02.pdf), [Demolition Permits](https://sfdbi.org/sites/default/files/IS-S-04.pdf)

# Contributing
To contribute to this repo, make sure to contact the Metro collaborators below for access to the Box directory where the data is located.  For orientation on the project design, visit [`CONTRIB.md`](CONTRIB.md) in this repo.

# Contact Info


Allie Omens  
Construction and Demolition Reuse and Recycling Associate, Metro Nashville Public Works  
allie.omens@nashville.gov  

Jen Harmann  
Waste Reduction Program Manager, Metro Nashville Public Works  
jenn.harrman@nashville.gov  

Jesse Spencer-Smith  
Chief Data Scientist, Data Science Institute  
jesse.spencer-smith@vanderbilt.edu


Charreau Bell  
Senior Data Scientist, Data Science Institute  
charreau.s.bell@vanderbilt.edu


Umang Chaudhry  
Data Scientist, Data Science Institute  
umang.chaudhry@vanderbilt.edu


Adam Szewciw  
Data Science for Social Good (DSSG) Intern, Data Science Institute  
adam.o.szewciw@vanderbilt.edu  


Miguel Moravec  
Data Science for Social Good (DSSG) Intern, Data Science Institute  
miguel.moravec@vanderbilt.edu  


Samuel Sliman  
Data Science for Social Good (DSSG) Intern, Data Science Institute  
samuel.m.sliman@vanderbilt.edu  


Preston Abraham  
Data Science Team Intern, Data Science Institute  
preston.d.abraham@vanderbilt.edu  


JunYi Zhu  
Data Science Project Assistant, Data Science Institute  
junyi.zhu@vanderbilt.edu  

