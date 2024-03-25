# afghanistan-civil-conflict-and-labor-flows

This repository contains code and data necessary to replicate the findings of the paper:

Xiao Hui Tai, Suraj R. Nair, Shikhar Mehra, and Joshua E. Blumenstock. Satellite and Mobile Phone Data Reveal How Violence Affects Seasonal Migration in Afghanistan (Mar 2024 Draft).

This work is licensed under the Apache License, Version 2.0.

# Instructions
One of the main datasets for the paper contains detailed information on over 20 billion mobile phone transactions in Afghanistan. These data contain proprietary and confidential information belonging to a major telecommunications operator, which we do not have permission to release.

However, all the code required to process the data and produce the results are in the following folders:

- `displacement-metrics`: code to compute migration metrics from the raw call detail records
- `home_location_detector`: helper code (code for the `migration_detector` package (Chi et al. 2020))
- `estimation`: code to create the analysis data set and produce all results

# Demo
The `demo` folder contains a demonstration of how to run the code and produce the figures in the paper. Small sample data sets are provided and can be downloaded and run by any user. Simply run each of the following R scripts to load the data and produce the corresponding figures. Note that many of the data sets provided are subsets of the original data (and indicated as such in the code) and so the figures produced are only for illustrative purposes.

- `fig2.R`
- `fig3.R`
- `fig4.R`
- `fig5.R`

# System requirements 
(to do)  

- Software dependencies and operating systems
- Versions the software has been tested on
- Any required non-standard hardware
