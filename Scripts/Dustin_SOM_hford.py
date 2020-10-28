# -*- coding: utf-8 -*-
"""
Created on Fri Mar  6 07:27:37 2020

@author: ccrosset
"""

#This script reads in Dustins Event Data and clusters using Doug Denu's SOM
#method

import numpy as np
import pandas as pd
from datetime import datetime

#Read in the data
var_data = pd.read_csv('/Users/dustinkincaid/ownCloud/BREE/Watershed Data/1_Projects/FluxRegimes/Data/transformed_vars_hungerford.csv',sep=',')
site = np.asarray(var_data['site'])
event_start = np.asarray(var_data['event_start'])
season = np.asarray(var_data['season'])
rain_total = np.asarray(var_data['rain_event_total_mm'])  #in mm
rain_hrs = np.asarray(var_data['rain_event_hrs'])         #in hrs
rain_int = np.asarray(var_data['rain_int_mmPERmin_max'])  #in mm/min
rain_pre_7d = np.asarray(var_data['rain_preEvent_7d'])
q_event_delta = np.asarray(var_data['q_event_delta'])
q_pre_mean_1d = np.asarray(var_data['q_preEvent_mean_1d'])
time_since_last = np.asarray(var_data['time_sinceLastEvent'])
PET = np.asarray(var_data['PET_mmDay'])   #in mm/day
q = np.asarray(var_data['q_mm'])            #in mm
NO3 = np.asarray(var_data['NO3_kg_km2'])   #in kg/km2
SRP = np.asarray(var_data['SRP_kg_km2'])     #in kg/km2
event_NO3_SRP = np.asarray(var_data['event_NO3_SRP'])
redox_mean_pre_3 = np.asarray(var_data['Redox_mean_preEvent_3'])
VWC_mean_pre_3 = np.asarray(var_data['VWC_mean_preEvent_3'])

#convert event dates into datetime variable in python
event_date=[]
for i in range(len(event_start)):
    event_date.append(datetime.strptime(event_start[i], '%Y-%m-%d %H:%M:%S'))

#%% SOM ANALYSIS
### DOUGS SOM CODE ###
import sys
import os
import pandas as pd
import somutils

# name site
site_name = 'hf'

#latlondim = len(lats_06) * len(lons_06)
nrows = len(event_date) #The number of rows will be associated with the 
                            #number of events we have


#NOW lets loop through all of our variables and stack them in an array so that
#we can have multiple input vars into the SOM
#First we have to set up some space for this data. Here you will have to manually
#identify how many input variables they are so we can make sure that our array
#is the correct size
n_met_vars = 14

#ANd now use that in creating an empty array
norm_met_vars = np.zeros((nrows, n_met_vars))

#NOW we loop through our various met variables all while keeping track of what 
#one we're on with the index call. This allows us to loop through and tell python
#where we want to start our next set of variables.
for idx, met_var in enumerate([q, NO3, SRP, event_NO3_SRP, time_since_last, rain_total, rain_hrs, rain_int, rain_pre_7d, q_pre_mean_1d, q_event_delta, PET, redox_mean_pre_3, VWC_mean_pre_3]):
    #Grab a met_var and make in into a dataframe with date as our row index
    #selected_data_feats_df = pd.DataFrame(data=met_var, index=date_count_90)
    selected_data_feats_df = pd.DataFrame(data=met_var, index=event_date)
    
    #Now normalize that met_var
    
    #selected_feats_df_norm = somutils.normalize(selected_data_feats_df)
    
    #Tell python where in the norm_met_vars array this met_var goes by calling
    #the index from above to determine where we start/stop
    col_start = idx
    col_stop = (idx + 1)
    #Finally plop those values into the norm_met_vars array 
    norm_met_vars[:, col_start:col_stop] = selected_data_feats_df.as_matrix()
    #norm_met_vars[:, col_start:col_stop] = selected_feats_df_norm.as_matrix()
    
###########

#selected_data_feats_df = pd.DataFrame(data=flat_msl,index=date_count_90)

#Normalize Flattened MSL array
# NORMALIZE DATA by min, max normalization approach
#selected_feats_df_norm = somutils.normalize(selected_data_feats_df)
#from sklearn.preprocessing import normalize
#norm_msl = normalize(flat_msl, axis=0, norm='l2')
#norm_msl = selected_feats_df_norm.as_matrix()

# Display statistics on our normalized data
#print(selected_feats_df_norm.describe())

# Initial learning rate for SOM. Will decay to 0.01 linearly
init_learning_rate = 0.05

# The number of rows for the grid and number of columns. This dictates 
# how many nodes the SOM will consist of. Currently not calculated 
# using PCA or other analyses methods.

## We will base the grid size of the SOM on dataset size.
# optimal size of SOM, S = 5 * sqrt(N)
#Sa = abs(sqrt(S))
#Sb = S/Sa 

S = 5 * np.sqrt(len(event_date))
Sa = np.sqrt(S)
Sb = S/Sa

nrows = int(round(Sa,0))
ncols = int(round(Sb,0))
# Create the SOM grid (which initializes the SOM network)
som_grid = somutils.create_grid(nrows, ncols, grid_type='hex')

# Initial neighbourhood radius is defaulted to 2/3 of the longest distance
# Should be set up similar to R package Kohonen
# https://cran.r-project.org/web/packages/kohonen/kohonen.pdf
# Radius will decay to 0.0 linearly
init_radius = somutils.default_radius(som_grid)

# Get the data as a matrix dropping the dataframe wrapper
#data = selected_feats_df_norm.as_matrix()
data = norm_met_vars

# Number of iterations to run SOM
niter = 100000

# Run SOM
som_weights, object_distances = somutils.run_som(data, som_grid, 'hex', niter, 
                                                 init_radius, init_learning_rate)

# It's possible that some data samples were not selected for training, thus do
# do not have a latest bmu
object_distances = somutils.fill_bmu_distances(data, som_weights, object_distances)

# Number of clusters to cluster SOM
nclusters = 4
# Cluster SOM nodes
clustering = somutils.cluster_som(som_weights, nclusters)

# Let's save the clusters corresponding to the samples now
results_path = '/Users/dustinkincaid/ownCloud/BREE/Watershed Data/1_Projects/FluxRegimes/Data/cluster_results_%s_%sclusters_%svars.csv'%(site_name, nclusters, n_met_vars)
somutils.save_cluster_results(selected_data_feats_df, results_path, 
                              clustering.labels_, (nrows, ncols), 
                              object_distances)

# Display the SOM, coloring the nodes into different clusters from 
# 'clustering' above
# Optional: pass in original dataframe to plot 
# the IDs onto their respective nodes
save_fig_path = os.path.join('/Users/dustinkincaid/ownCloud/BREE/Watershed Data/1_Projects/FluxRegimes/Plots/som_figure_%s_%sclusters_%svars.png'%(site_name, nclusters, n_met_vars))
somutils.basic_som_figure(data, som_weights, som_grid, clustering.labels_,
                            'hex', save_fig_path)

print("Finished")



