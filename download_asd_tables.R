#This is a script to download asd spectra tables and associated reference data
# Begin by loading all required packages
library(downloader)
library(readr)#required for faster reading of large tables (at 1 nm interval)
dir.create("asd_data", showWarnings=F)
setwd("./asd_data")

download("https://www.dropbox.com/s/dz85942wa8hmpkk/asd.zip?dl=1", "asd.zip", mode="wb")
unzip("asd.zip")

# 1. AfSIS asd at 10 nm interval
# ------------------------
afsis <- read_csv("./asd/AfSIS_ASD10_deriv1.csv")

# 2. AfSIS asd at 1 nm interval
# ------------------------
afsis.1 <- read_csv("./asd/AfSIS_ASD1_deriv1.csv")

# 3. ICRAF_ISRIC asd at 10 nm interval
# ------------------------
isric <- read_csv("./asd/ICRAF_ISRIC10_deriv1.csv")

# 4. TSBF organic resources at 10 nm interval
# ------------------------
tsbf <- read_csv("./asd/TSBF_ASD10_deriv1.csv")

# Variable description in the TSBF table
# Scode = Sample number in TSBF database
# Genus = Genus of plant part
# Species = Species
# Name = Common name
# Part = Plant part
# Lignin = Lignin concentration; units = (%)
# TSPP = Total soluble polyphenols concentration; units = (%)
# N = Nitrogen concentration; units = (%)
# P = Phosporus concentration; units = (%)
# K = Potassium concentration; units = (%)
# Ca = Calcium concentration; units = (%)
# Mg = Magnesium concentration; units = (%)






