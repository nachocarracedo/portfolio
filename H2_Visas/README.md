# h2_visas

The goal of this notebook (h2_visas_analysis.ipynb) is to explore the H2 Visas data set.

**Data**

The analyses below depend on two major datasets from the Department of Labor, the agency responsible for protecting workers and vetting employers seeking visas:

* The Wage and Hour Division's WHISARD database, obtained via a Freedom of Information Act request. The database contains information on employers, violations, fines, and other details corresponding to investigations concluded between October 1, 2001 and March 31, 2015. (Note: The WHD has redacted some tables and columns per FOIA exemption 5.) You can download a copy of the data dictionary here. To decompress the data file, run make data/whd-enforcement-database from this repository's root directory. Once you do so, the data can be found in data/whd-enforcement-database.

* The Office of Foreign Labor Certification's records of visa-certification decisions for the H-2 visa program. (The visa comes in two types: H-2A for agricultural workers and H-2B for non-agricultural unskilled workers.) Obtained from here and here. The raw and processed data can be found in data/oflc-decisions.

Main questions I wanted to answer:
* How application status have change over time
* How has the number of approved workers changed over time?
* How has the number of approved workers changed over time?
* Which states request the most visas and how has this changed over time?
