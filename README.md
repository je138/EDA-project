# EDA-project
Final Project for Environmental Data Analytics

## Summary

In June 2016, the US Army Corps of Engineers began implementing a new quasi-run-of-river (QRR) floodwater management regime for the Roanoke River. The purpose of this analysis is to assess any change in mean daily discharges at the Roanoke Rapids gage station before and after the QRR was implemented. Additionally, I will evaluate whether any changes in discharge relate to changes in downstream water quality indicators (DO, temperature, specific conductance).

The following data is contained in this repository: daily discharge and gage height at the Roanoke Rapids gage station; gage height, dissolved oxygen, water temperature, and specific conductance at the downstream gage stations near Oak City; locations of two gage station sites.

## Investigators

Jack Eynon, student in Environmental Data Analytics (Kateri Salk-Gunderson, spring 2020)

## Keywords

river, flow, QRR, quasi-run-of-river, floodwater management, Army Corps, discharge, water quality, dissolved oxygen, temperature, specific conductance

## Database Information

All gage station data comes from the United States Geological Survey at waterdata.usgs.gov; accessed 04/14/2020.

## Folder structure, file formats, and naming conventions 

The repository contains a folder for all R scripts in the "scripts" folder. It also contains a "Data" folder with subfolders for "processedData" and "rawData". All files in the data folder are in .csv format. All scripts are in .R format.

Data files are named with the following convention:  sitelocation_source_description_raw/processed.csv

## Metadata

### Raw data
**Roanoke Rapids and Oak City**

Column name: agency_cd;
Description: Agency code, all values are "USGS";
Class: character;
Units: none

Column name: site_no;
Description: Site number, unique identifier for USGS stations;
Class: character;
Units: none

Column name: Date;
Description: Date in "yy-mm-dd" format;
Class: date;
Units: none

Column name: X_00010_00003;
Description: Water temperature;
Class: numeric;
Units: degrees C

Column name: X_00010_00003_cd;
Description: Qualification codes, "A" - Approved for publication, "P" - Provisional data subject to revision;
Class: character;
Units: none

Column name: X_00065_00003;
Description: Gage height;
Class: numeric;
Units: feet

Column name: X_00065_00003_cd;
Description: Qualification codes, "A" - Approved for publication, "P" - Provisional data subject to revision;
Class: character;
Units: none

Column name: X_00095_00003;
Description: Specific conductance, water, unfiltered, microsiemens per centimeter at 25 degrees Celsius;
Class: numeric;
Units: uS/cm at 25C

Column name: X_00095_00003_cd;
Description: Qualification codes, "A" - Approved for publication, "P" - Provisional data subject to revision;
Class: character;
Units: none

Column name: X_00300_00003;
Description: Dissolved oxygen, water, unfiltered, milligrams per liter;
Class: numeric;
Units: mg/L

Column name: X_00300_00003_cd;
Description: Qualification codes, "A" - Approved for publication, "P" - Provisional data subject to revision;
Class: character;
Units: none

Column name: X_00060_00003;
Description: Discharge, mean daily, cubic feet per second;
Class: numeric;
Units: ft^3/s

Column name: X_00060_00003_cd;
Description: Qualification codes, "A" - Approved for publication, "P" - Provisional data subject to revision;
Class: character;
Units: none

### Processed Data
**Roanoke Rapids and Oak City**

Column name: site_no;
Description: Site number, unique identifier for USGS stations;
Class: character;
Units: none

Column name: Date;
Description: Date in "yy-mm-dd" format;
Class: date;
Units: none

Column name: mean.daily.discharge;
Description: Discharge, mean daily, cubic feet per second;
Class: numeric;
Units: ft^3/s

Column name: gage.height;
Description: Gage height;
Class: numeric;
Units: feet

Column name: temperature;
Description: Water temperature;
Class: numeric;
Units: degrees C

Column name: specific.cond;
Description: Specific conductance, water, unfiltered, microsiemens per centimeter at 25 degrees Celsius;
Class: numeric;
Units: uS/cm at 25C

Column name: DO;
Description: Dissolved oxygen, water, unfiltered, milligrams per liter;
Class: numeric;
Units: mg/L

Additional information at:
https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&pm_search=&casrn_search=&srsname_search=&format=html_table&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units

## Scripts and code

USGS_dataRetrieval.R contains the script used for pulling relevant river gage data from the USGS website (using the dataRetrieval R package).

USGS_dataProcessing.R contains the script used for processing the raw data. The data was processed by removing irrelevant
columns, selecting time range of interest, renaming columns, and setting correct classes for variables.

## Quality assurance/quality control

All observations not marked "Approved for Publication" by the USGS were removed from the data sets.
