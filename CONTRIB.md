# Contributing to this Repo
> An orientation to the repo and overview of components

# Getting Started
The purpose of this repo is to deliver projections on the anticipated tonnage of debris in Nashville for the next 5 years.  This is delivered in the form of a Shiny app, and all code for this effort is present in this repository.  All of the efforts of this work are described in **Issues**, with relevant details for each task.  In the case of confusion, issues can be referenced to provide insight.

All code is submitted through **Pull Requests**.  These additionally contain descriptions of what has been done, what is left to do, and what can be improved upon.

To get started, read through the `Readme` to gain an understanding of the project, peruse the Milestones to understand the components of the project, delve into the issues and pull requests to understand design decisions and limitations, and review the code.  Make sure to request access to the Box where all data files are stored.

# General approach
This work is composed of three major parts to generate and display in Shiny as shown in the diagram below:
![Project Approach Overview](https://user-images.githubusercontent.com/52971901/129377866-389a8aca-4b6d-4971-88d7-dbd5271c8af1.png)

* **Projections the number of permits**: Estimation of the number of total permits for 4 combinations of interest: commercial vs residential (noted in the code as `comm_v_res`) and construction vs demolition (noted in the code as `project_type`).  The current method for generating these estimates is through leveraging the historical number of permits in these categories with an ARIMA/SARIMA model to provide projections.
* **Simulation of future permits**: Using the number of permits expected for each of these combinations, bootstrap the current Nashville dataset based on the stratifications of the above combinations
* **Modeling of debris generated**: Based on the joint permit data and debris tonnage data from Austin and San Francisco, build the current a model to predict debris tonnage from the permits generated

This work coalesces into the final user interface for discovering tonnage data, permits, and expected debris generated for individual council districts.

# Details of implementation
The following sections provide insight into the behavior of individual components of this work.  For more information, visit the code, issues, and pull requests.

## Data
All cities data were inspected and groomed prior to usage in the prediction framework.  The `10` and `20` series document the efforts towards reading and understanding the data, and providing insight into the counts present in the data and any unusual behavior.  The `30` series takes this a step further by removing all duplicates to generate de-duplicated datasets for use in modeling and prediction.

### Unusual Behavior
All unusual behavior is documented in the code and the issues, where the following are the most relevant:

- All cities contain duplicate entries for permit numbers.  These were assembled to form a single permit.  This includes choosing start dates, end dates, and generally preferring the most recently updated entry for other fields.
- San Francisco and Austin have outliers in terms of tonnage, including tonnages up to 4 million.  These appeared to have no indication as to why this should be as these projects appear to be similar to others with significantly lower tonnages.  Projects with tonnages greater than 40,000 tons were removed from the dataset.
- The `start`, `end`, `completed`, `issued`, and `expired` dates don't always seem to be cohesive for a single permit.  For example, the close date can occur before the issued date.

### Crosswalk
Nashville, Austin, and San Francisco have permit and debris data which differ in the types, names, fields, and descriptions of the data within.  These were standardized using the crosswalk file on Box (`~/nash-zero/city-variable-crosswalk.xlsx`).  Details can be found in `30` and `35`.  To effectively use this file, keep in mind the following principles:

-  `columns_conversion` sheet contains all the columns that will be included in the final dataset as well as others.  This is used to recode columns for a given city into the name given in the `Final Column Name` column
   -  `keep_in_ds` and `use_in_model` fields are used to keep these fields in the final dataset to be used across the framework.  Currently `use_in_model` is not implemented for selection of variables for use in the models, but is more of an indicator to the user that this is a field of interest in the modeling.  However, setting this to 0 when `keep_in_ds` is also 0 will remove it from the final generated dataframe.
   -  `is_calc` is used to determine whether special releveling on the column values is necessary (see the sheets `col_*` below for more information) for a given city.  When it specific for specific cities, it occurs as comma-separated values (with no space separator) with the given abbreviation.  Entries as `n=s=a` with the equals sign indicates that this column can be generated _after_ putting all of the datasets together.  Note that any `=` in this column will cause this execution to execute after the full dataframe is assembled, and behaviors such as `n=s` or `n=s,a` are not currently supported.
   -  `pre_calc` is not explicitly supported and is not currently looked for in the code.  Anything with city markings is assumed to occur in the preprocessing functions provided in the 30 notebooks.
   -  `dtype` uses notation from the R `readr` package to indicate the type the variable should have prior to being assembled into the full, joined dataset
-  `col_*` sheets contain the releveling (i.e., changing the notation `active` to `issued` for the renamed Austin `status` column to match the desired `level_name` `issued`) to match levels across cities.  Note that to add new calculated levels, the sheet must be named `col_{final_column_name}`, and will be calculated based on the `n,s,a` notation in the `is_calc` column of `columns_conversion`.
   -  `include` is a boolean reflecting whether we keep this level or allow it to be recoded as other.  Anything not explicitly specified will be recoded as `other`.
   -  `level_name` is the desired final level name.  We keep these the same as Nashville as Nashville data will be the final dataset.
   -  The cities columns contain the names of these levels to be converted into `level`.  These are separated by `, ` (a comma followed by a space) to help readability and the code depends on this specific type of separation.

#### Adding new columns which do not vary in implementation across cities
This spreadsheet specifically helps with fixing things that are already in the datasets or have been decided upon during the course of this particular project.  However, to add additional fields which can be calculated on the entirety of the dataset (i.e., if you want to join information based on zip code, which is present in the final dataset), then this can be done as a function at the end of the `generate_structured_dataset`.  You can see an example of this with `add_debris_generated`.

#### Adding new columns with DO vary in implementation across cities
An example of how this works is shown in `col_use_type`.  Follow the format of the other `col_*` sheets.  Decide which levels to include and indicate them with a 1.  Determine the level names of interest.  Write the levels which should be tied to this level name for each given city, using the separator `, ` to separate values.  Once finished, add the variable name to the spreadsheet for `Final Column Name` (e.g., here, this would be `use_type`).  In each of the city names, add the name of the column from which this variable can be calculated.  In `is_calc`, add the abbreviation for the cities for which this should be calculated separated by commas.

If one or more of the cities needs a bit of assistance getting to this point (e.g., see `permit_type` for Nashville, used to calculate `comm_v_res`), add the new column name you need to make in `Final Column Name`, indicate this with the city abbreviation in `pre_calc`, and add the code required in the pre-processing function for the city.  

## Forecasting the Number of Permits
The forecasts currently use an ARIMA model to forecast the number of permits for each of the combinations of interest.  See notebook `41` for details.

## Generation of Synthetic Data
`42` contains the development of the code to generate the synthetic permit sets.  Note that the outputs also contain the historical number of permits and maintain other (non-simulated) fields to pass to the model.  This was done in order to maintain an acceptable number of permits available for bootstrapping (so as not to narrow down the available permits to an individual permit).

## Training the Debris Tonnage Model
Training and experimentation around the development of the model is found in `43` and `44`.  The code contains wrappers for modeling functionality such that uniform investigation of models can be performed.  Although the data is trained on permit data from Austin and San Francisco, testing is done in aggregate using the overall number of Nashville data.  See `44` for implementation details.

# Other Considerations
Future work can be found in the issues under the label "Enhancement" or in the "Turn the Crank" milestone.  Priorities include:

* Ironing out the meaning of the fields (columns) for each city to make sure the meanings of the fields match correctly
* Adding a column such that Single Family Residental can be specifically investigated as the interest of our Metro collaborators
* Adding additional outcome variables to the existing model or generating new models for `recycling_debris` and `landfill_debris`.


 





