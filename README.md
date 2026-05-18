# afghanistan-civil-conflict-and-labor-flows

This repository contains code and data necessary to replicate the findings of the paper:

Xiao Hui Tai, Suraj R. Nair, Shikhar Mehra, and Joshua E. Blumenstock. Satellite and Mobile Phone Data Reveal How Violence Affects Seasonal Migration in Afghanistan.

This work is licensed under the Apache License, Version 2.0.

# Instructions
One of the main datasets for the paper contains detailed information on over 20 billion mobile phone transactions in Afghanistan. These data contain proprietary and confidential information belonging to a major telecommunications operator, which we do not have permission to release.

However, all the code required to process the data and produce the results are in the following folders:

- `displacement-metrics`: code to compute migration metrics from the raw call detail records
- `home_location_detector`: helper code (code for the `migration_detector` package (Chi et al. 2020))
- `estimation`: code to create the analysis data set and produce all results

# Demo
The `demo` folder is a self-contained folder that users can run to produce all the figures and tables in the paper. Sample data sets are provided where original data cannot be released, and are indicated as such in the code. Simply run the R scripts corresponding to the figure or table number to load the data and produce the corresponding figures and tables. 


# System requirements 
(to do)  

- Software dependencies and operating systems
- Versions the software has been tested on
- Any required non-standard hardware
