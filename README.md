This project analyzes the characteristics of individuals who enrolled in private health insurance in the United States. This folder contains every document needed to analyze the project. There are:

=== 1. Scripts ===

This folder comprises all scripts used in this project. There are 4 R-scripts that I wrote as part of this project. 

a. Insurance Data Preparation.R: This script aims to clean datasets for analysis. 

Details:
The codes work by cleaning for states not included for analysis, for example, Hawaii and Alaska. Then the codes will select some variables of ACS data based on five different income levels and insurance types, private and public. Then, it calculates the proportion of insurance types using the loop function. Finally, the clean version of data which is machine readable for creating graphs is stored as insurance_data_tr.csv.

b. Insurance Shiny App.R to generate the Shiny (Run the whole script) 

Details:
The codes are divided into three parts: user interface, server-containing functions, and output. In the first part, codes set the interface of the Shinyapp enabling input using radio buttons, a dropdown menu, and text inputs. In the server section, codes read the input data sets, filter based on the input status, and generate two data frames, which are data_sf for the map and data_plot for the graph. For the last section, codes create the choropleth map based on the input (private or public health insurance, and the income level groups). The other codes generate bar graphs based on the two states' input and type of insurance.

c. Regression Model.R: This script fits the logistic model obtained from selection models I did in another project.

Details:
The codes clean the ACS 2022 data by changing the SEX, RACE, and HCOVPRIV (private insurance enrollment) variables into binary, creating categorical variables for MARST (marriage status) and EDUC. The model had Private Insurance ownership as the dependent variable and other components as independent. Then, the code runs a logistic regression using the glm function.

d. Web Scrap.R: This is R-script used for web scraping articles from White House website. 
Details:
Section 1 contains codes to scrap the website pages from page 1 until all pages by extracting the article's data, such as titles, dates, and hyperlinks. In Section 2, the codes filter for selected phrases. Then Section 3, I scrapped the selected articles based on the filtered hyperlinks and stored all the articles. Finally, the codes in Section 4 combine the filtered article (HCCover_dta) and the text (content_dta).

-----------------------------------------------------------------------------------------------------------------------

=== 2. Data Files ===

This folder is to save data files that I utilize for analysis and consists of several groups of datasets.

a. acs_2022.csv and nhgis_insurance.csv - These data are obtained from the IPUMS website and it is used to map the prevalence of private health insurance in the United States. The data is also used for model fitting that I did in the model.R script. There are two sources from where we got the data, IPUMS ACS (https://usa.ipums.org/usa/) and IPUMS NHGIS (https://www.nhgis.org). 

b. cb_2018_us_state_500k (and its variance) - This data group is the basis of developing the map in the shiny.R. These shapefiles data is downloaded from IPUMS NHGIS.

c. insurance_income.csv - It is a cleaned version of nhgis_insurance.csv data which is used to create the dynamic map using Shiny.

d. acs_insurance_2023_2022.csv - The data is a ten-year data (2013 to 2022) of private health enrollment status in all states. I obtained this data from IPUMS ACS.

e. 1976-2020-president.csv - This data contains presidential election results in all states with the detailed parties. The data is downloaded from MIT Election Lab (https://electionlab.mit.edu/data).

f. White_House_Statements.csv - I obtained this data from web scraping the White House website (https://www.whitehouse.gov/briefing-room/) for text sentiment analysis.

IMPORTANT:

Users are required to change the path name on the codes containing "path".

- path_data and file_path : A path to read the data sets downloaded from IPUMS (ACS and NHGIS), the shapefiles for maps, and the cleaned data.

- save_path : A path to save the generated graphs.

-----------------------------------------------------------------------------------------------------------------------

=== 3. Output ===

This folder is hosting output from the analysis. This mainly comprises the output from our analysis, primarily graphs from our sentiment analysis. I include a graph of sentiment analysis using Bing, NRC, AFINN, and Sentimentr methodologies. I also include an edge graph that shows words' connection in our sentiment analysis with insurance word.
