# R Script: 02_Eurostat_teemp_empl_Excel_file_into_R_long_fmt.R

# Importing second Eurostat Indicator for Temporary Employment

# Indicator: Employed persons with temporary contract
# - Part-time employment and temporary contracts – annual data - Online data code:lfsi_pt_a
# - Eurostat Indicator: Employed persons with temporary contract
# - Eurostat reference table: [lfsi_pt_a]. Activity and employment status (EMP_TEMP)>Employed persons with temporary contract

# 1. Load required libraries
library(here)
library(tidyverse)
library(janitor)
library(readxl)