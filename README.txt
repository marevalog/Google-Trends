Source Code
===========

Code provided for analyses detailed in Seasons, Searches, and Intentions
------------------------------------------------------------------------

This folder contains the machinery to automatically retrieve and summarize outputs of google trends.

To retrieve and format the google trends results:
 
	1. Put terms you want in termsList.txt
	2. Run makeTermsList.R

		* `./makeTermsList.R`
		* in R, `source("makeTermsList.R")`

	3. Run termsFromTxt.py

		* `./termsFromTxt.py` in Linux

	4. Run importTrendsResults.R

		* in R, `source("importTrendsResults.R")`

The retrieved and formatted results will now be stored under requestResults with file name sumTable.Rda.

To perform analyses:
	
	5. in R, `source("plotTrendsByIntentions.R")`
	6. in R, `source("cosinorAnalysis.R")`

		* to run cosinorAnalysis, the R seasons package is required.

De-identified survey results collected as part of the study are provided under Final_search_term_data.csv.