# **Codes and Data for: Causal Forest Approach for Site-specific Input Management via On-farm Precision Experimentation**

## General Information
+ Corresponding Author Contact Information
	* Shunkei Kakimoto
		- Institution: Department of Agricultural Economics, University of Nebraska-Lincoln
		- Address: 102 Filley Hall 1625 Arbor Drive Lincoln, NE 68583, USA 
		- E-mail: skakimoto3@huskers.unl.edu

+ Co-authors Contact Information
	* Taro Mieno
		- Institution: Department of Agricultural Economics, University of Nebraska-Lincoln
		- Address: Lincoln, NE 68583, USA
		- E-mail: tmieno2@unl.edu
	
	* Takashi S. T. Tanaka
		- Institution: Artificial Intelligence Advanced Research Center, Faculty of Applied Biological Sciences, Gifu University
		- Address: Gifu 501-1193, Japan
		- E-mail: takashit@gifu-u.ac.jp
	
	* David S Bullock
		- Institution: Department of Agricultural and Consumer Economics, University of Illinois
		- Address: Urbana, IL 61801, USA
		- E-mail: dsbulloc@illinois.edu


## DATA & FILE OVERVIEW

This dataset includes the R and Python codes and data for analysis presented in: "Causal Forest Approach for Site-specific Input Management via On-farm Precision Experimentation". 

### Steps to Reproduce
+ Before you start, create a new R project in a folder including "Data" and "Codes" folders. 
+ The simulation series starts from the code **1\_1\_generate_field.R** in the order of the number assigned in front of the file names. By running the files in sequence, you can reproduce the simulations results, tables, and figures presented in the article.

### List of Files:
Data folder:
+ **`field_boundary.rds`**
	* A polygon boundary data of a field, which is used to generate a field polygon in **1\_1\_generate\_field.R**. 
+ **`ctree_data.rds`**
	* Data to be used in creating an example causal tree in **PrepareResults.rmd**.

Code folder:
+ **0\_1\_functions\_gen\_analysis\_data.R**
	* This code file contains functions to generate field characteristics (e.g., *α*, *β*, *ymax*, etc.) and yield datasets.

+ **0\_2\_functions\_main\_sim.R**
	* This code file contains functions to conduct Random Forest (RF), Boosted Random Forest (BRF), and Causal Forest (CF) analysis to predict yields and EONRs site-specifically.

+ **1\_1\_generate_field.R**
	* This code file creates various size of grids (i.e., "plots", "subplots", and "cells") within a field.
	* Final output: `analysis_field.rds`

+ **1\_2\_generate\_coefficients.R**
	* This code file generates field characteristics (e.g., *α*, *β*, *ymax*,..., etc.) data using unconditional Gaussian geostatistical simulation based on the spherical variogram model.
	* Final output: `coefficients_sprange_400.rds`

+ **1\_3\_generate\_analysis\_data.R**
	* This code file simulates on-farm experiment data through assigning experimental nitrogen rates across the plots in the field, and calculating cell-level yield with the Mitscherlich-Baule production function. Then, the datasets are aggregated by subplot-level. 
	* Final output: 
		- Cell-level output: `reg_raw_data.rds`, `test_raw_data.rds`
		- Subplot-level output: `reg_data.rds`, `test_data.rds`
			+ NOTE: Cell-level outputs will be used for CNN analysis, and subplot-level outputs will be used for RF, BRF and CF analysis. 

+ **2\_1\_MCsim\_Forest.R**
	* This code file conducts one thousand simulations of RF, BRF and CF analysis to predict yields (only for RF and BRF) and EONRs site-specifically. 
	* Final output: `forest_SimRes_alpha_beta_ymax.rds`, `forest_SimRes_alpha_beta_ymax_theta_1_theta_2.rds`, `forest_SimRes_alpha1_alpha2_beta1_beta2_ymax1_ymax2.rds`, `forest_SimRes_alpha1_alpha2_beta1_beta2_ymax1_ymax2_theta_1_theta_2.rds`

+ **2\_2\_1\_MCsim\_CNN\_aby.py**, **2\_2\_2\_MCsim\_CNN\_abytt.py**, **2\_2\_3\_MCsim\_CNN\_aabbyy.py**, and **2\_2\_4\_MCsim\_CNN\_aabbyytt.py**
	* Each of these code files conducts one thousand simulations of yield prediction using Convolutional Neural Network (CNN) by modeling scenario. 
	* Final output: `output_..._.csv` 
		- NOTE: `...` refers to the simulation number. 

+ **3\_1\_SimDataAnalysis.R**
	* This code file calculates RMSE of predicted EONRs and yields by ML method and modeling scenario. 
	* Final output: `allML_summary_bySim.rds`

+ **3\_2\_CompTeEstimation.R**
	* This code file estimates treatment effect by treatment type and ML method under the "aabbyytt" scenario in one of the one thousand simulation rounds. The output data is used to create Figure 6: "True treatment effects vs estimated treatment effects (scenario: aabbyytt)"
	* Final output: `dt_TEcomparison.rds`

+ **4\_1\_Appendix\_generate\_coefficients.R**, **4\_2\_Appendix\_generate\_analysis\_data.R**, **4\_3\_Appendix\_MCsim\_Forest.R**, and **4\_4\_Appendix\_SimDataAnalysis.R**
	* These code files conduct the same series of simulations ("1\_2\_generate\_coefficients.R" to "3\_1\_SimDataAnalysis.R") but with different yield error sizes. Please see the Appendix G in the manuscript for details.

+ **5\_prepare\_results.R**
	* This code file prepares data to be used as an example of spatial distributions of field characteristics, a trial design, and generated yield. 

+ **PrepareResults.rmd**
	* This code file creates all the figures and tables which are used in the manuscript. 
