# -*- coding: utf-8 -*-
"""
Created on Wed Jul 22 11:11:41 2020

@author: cristian.cruz

This scrict reduces the number of columns in GBIF data so that it can be used in R
"""

# Extract data GBIF to set16 format
import pandas as pd
import os

print(os.getcwd()) ##identify the folder directory

#os.chdir('G:/Cristian_data/Humboldt/Git/Unif_biological_data_-set16-/extract_specific_column_to_GBIF_for_set16.py') ##folder for the code

#Set16 Folder
os.chdir('D:/Set16/datos_originales/GBIF&SiB/raw/Descarga_2023_01_23_directGBIF/0255995-220831081235567/')
                      

print(os.getcwd())
gbib_cn = pd.read_csv('occurrence.txt', nrows=1, sep='\t').columns.tolist()

print(gbib_cn)

columns=[ 'gbifID', 'accessRights', 'institutionCode', 'collectionCode', 'basisOfRecord', 'occurrenceID', 'catalogNumber', 'recordedBy', 'eventDate', 'year', 'month', 'day', 'continent', 'countryCode','stateProvince', 'county', 'municipality', 'locality', 'decimalLatitude', 'decimalLongitude', 'coordinateUncertaintyInMeters', 'scientificName', 'elevation', 'kingdom', 'phylum', 'class', 'order', 'family',  'genus', 'specificEpithet', 'scientificName']

print(columns)
print(gbib_cn)

df = pd.read_csv('occurrence.txt', header = 0, sep='\t', skipinitialspace=False, usecols=columns, low_memory=True, encoding='utf-8')

#export
df.to_csv('filteredOcc.txt', header=True, index=None, sep=';', encoding='utf-8')
