#!/usr/bin/env python3
# -*- coding: utf-8 -*-

### Author Doug Denu ###

"""Self Organizing Map module modelled after R Kohonen package."""

import os
import math
import sys
import ast

import numpy as np
import pandas
from sklearn.cluster import AgglomerativeClustering

from matplotlib import pyplot as plt
from matplotlib import patches as patches
from matplotlib import colors


def default_radius(som_grid):
    """Compute the starting radius for the SOM.
    
    According to the R Kohonen package the default radius should be a value
    that covers 2/3 of all unit-to-unit distances.
    https://cran.r-project.org/web/packages/kohonen/kohonen.pdf
    
    Parameters:
        'som_grid' (3D numpy array) : a numpy array where the first two 
            dimensions are the row, column of the map and the 3rd 
            dimension is a vector representing a [x,y] cartesian coordinate
            for that row, col cell. These [x,y] coordinates will change 
            depending on SOM map layout hex vs square. e.g. a 2x2 square map:
            [ [ [0,1] , [1,1] ],
              [ [0,0] , [1,0] ] ]            
    
    Returns:
        A float value representing a distance that covers 2/3 of all unit-to-unit
        distances.    
    """
    # Get the dimensions of the SOM grid / map
    grid_shape = som_grid.shape
    
    rows = grid_shape[0]
    cols = grid_shape[1]
    # The number of units is the columns * rows
    num_units = grid_shape[0] * grid_shape[1]
    # Create an empty list to track collection of pairwise distances
    unit_dists = []
    
    # Iterate over each row, col to get pairwise distances. These distances
    # are dependent on the SOM grid / map structure, for instance a hex 
    # map will have different distances then a square map. Therefore the 
    # coordinates to calculate distances from are stored as a vector in each 
    # row, col 
    for x in range(rows):
        for y in range(cols):
            # List to track all distances from current row, col
            cur_dists = []
            cur_xy = som_grid[x, y, :].reshape(2, 1)
            # With current row,col iterate over all other row,col
            # No need to compute each pairwise distance in both directions 
            # since it will be symmetric
            for i in range(x, rows):
                # If on same row as current row, start checking distance
                # with the adjacent column, otherwise on new row and need
                # to check every column
                if x==i:
                    start_j = y + 1
                else:
                    start_j = 0
                for j in range(start_j, cols):
                    check_xy = som_grid[i, j, :].reshape(2, 1)
                    # Euclidean distance
                    tmp_dist = math.sqrt(sum((cur_xy - check_xy) **2))
                    # Add distance to list
                    cur_dists.append(tmp_dist)
            # Add that round of distances to the complete distance list
            unit_dists.append(cur_dists)

    # Flatten unit_dists into a numpy array; probably could do:
    # all_dists = np.array(unit_dists).flatten()
    all_dists = np.array([item for sublist in unit_dists for item in sublist])

    # Get 2/3 value by taking the 2/3 percentile of all the distances
    return np.percentile(all_dists, 67.0)
    

def decay_func(initial_value, end_value, current_step, total_steps):
    """Linear decay function that determines the current decay value.

    Given the current step in the decay process, this function returns 
    the value based on a linear decay. There is room here to add a 
    parameter that could do exponential decay values as well.
    
    Parameters:
        'initial_value' (float) - the initial value to decay from.
        'end_value' (float) - the final value to decay to over 'total_steps'.
        'current_step' (int) - the current step of decay <= total_steps.
        'total_steps' (int) - the number of total steps of decay.

    Returns:
        (float) a value along the decay function    
    """
    # Exponential decay:
    # decay_rate = np.log(end_value / initial_value) / total_steps
    #return initial_value * np.exp(decay_rate * total_steps)
    
    # Return value along linear decay function
    return initial_value - ((initial_value - end_value) / total_steps) * current_step
    
    
def calculate_influence(distance, radius):
    """Calculate exponential decay value for distance.
    
    This function returns an influence value i where 0<i<=1. The 
    value drops off or decays exponentially the further the distance. 
    A distance of 0 should return a 1.0 where as a distance equal to 
    radius should return a value close to 0 but not 0. 

    Parameters:
        'distance' (float) - a distance value less than or equal to the
            radius
        'radius' (float) - the max value 'distance' could be
        
    Returns:
        A float value between 0 and 1 of the decay           
    """
    decay_rate = np.log(0.001 / 1.0) / radius
    return np.exp(decay_rate * radius)
        
def find_bmu(target, som_net):
    """Finds the Best Matching Unit (BMU) for a given vector.
    
    A BMU is the smallest Euclidean distance between two vectors. This function
    finds the BMU between 'target' vector and the SOM maps unit vectors (where
    the unit vectors represent the weights of the SOM map). The 'target' 
    vector should have the same number of elements as the 'som_net' weights.
    
    Parameters:
        'target' (1D numpy array): a vector represented as a numpy array. The 
            length of the vector should be the same as the length of the 3rd 
            dimension of the 'som_net'.
            
        'som_net' (3D numpy array): a numpy array where the first two 
            dimensions are the row, column of the map and the 3rd 
            dimension is a vector representing the unit weights
            for that row, col cell. The unit vector weights should have 
            the same length as the 'target' vector. e.g.
            [ [ [0.5,1.0,0.3] , [1.0,0.2,0.6] ],
              [ [0.1,0.9,0.9] , [0.7,0.6,0.5] ] ]        
    
    Returns: 
        a (bmu, bmu_idx) tuple where bmu is the high-dimensional BMU
        and bmu_idx is the index of this vector in the SOM.
    """
    # The size of the target vector, which should be the size of SOM weight 
    # or unit vectors
    vector_len = len(target)
    # Reshape for consistency below
    target = target.reshape(vector_len, 1)
    
    # A placeholder for the best BMU index in the SOM net.
    bmu_idx = np.array([0, 0])
    # A placeholder for the best BMU weight vector
    bmu = np.zeros((vector_len,1))
    # Set the initial minimum distance to a huge number
    min_dist = np.iinfo(np.int).max    
    # Rows and Cols of the SOM net
    rows = som_net.shape[0]
    cols = som_net.shape[1]
    
    # Calculate the high-dimensional distance between each neuron and the input
    for x in range(rows):
        for y in range(cols):
            weight_vector = som_net[x, y, :].reshape(vector_len, 1)
            # Don't bother with actual Euclidean distance, to avoid expensive sqrt operation
            sq_dist = np.sum((weight_vector - target) ** 2)
            if sq_dist < min_dist:
                # BMU so far, update min distance, BMU index, BMU weights
                min_dist = sq_dist
                bmu_idx = np.array([x, y])
                bmu[:] = weight_vector[:]
    
    # Return the (bmu, bmu_idx) tuple
    return (bmu, bmu_idx)
    
def normalize(data_df, subset=None):
    """Normalize the data by column.
    
        Use the min, max normalization where minimum value is normalized to 0.0
        and maximum values is normalized to 1.0.
        
        Parameters:
            'data_df' (pandas data frame) - a dataframe with numeric columns
            'subset' (list)(optional) - an optional list of named columns 
                in 'data_df' to normalize. Normalize these columns only.
            
        Returns:
            Pandas Data Frame with normalized values    
    """
    if subset is None:
        # Normalize all columns in dataframe
        data_norm_df = (data_df - data_df.min(0)) / (data_df.max(0) - data_df.min(0))
    else:
        # Get the subset of columns to normalize
        data_sub_df = data_df[subset]
        # Normalize subset of columns
        data_sub_norm_df = (data_sub_df - data_sub_df.min(0)) / (data_sub_df.max(0) - data_sub_df.min(0))
        # Copy full dataframe so as to avoid mutation
        data_norm_df = data_df.copy()
        # Update the full dataframe with the normalized values for the subsets
        data_norm_df[subset] = data_sub_norm_df[subset]
        
    return data_norm_df
    
def create_grid(nrows, ncols, grid_type='square'):
    """Create a grid or lattice representation for the SOM.
    
    Create a representation for the SOM map as a 3 dimensional numpy 
    array. Returned matrix will either represent a hexagonal 
    or grid format.
    
    Parameters:
        'nrows' (int) - number of rows for the SOM map (and thus matrix)
        'ncols' (int) - number of columns for the SOM map (and thus matrix)
        'grid_type' (String) - this determines how distances should be 
            calculated for determining neighbors.
            Valid values:
                'hex' - treat the map as nrows x ncols hexagons
                'square' (default) - treat the map as a grid of squares
                
    Returns:
        A 3 dimensional array, where the x,y dimensions indicate the 
        layout for the SOM map and the z dimension holds the physical 
        cartesian coordinates for that space on the map.
        Ex:
            nrows - 2
            ncols - 3
            grid_type - 'hex'
            
            [[[ 1.        ,  0.8660254 ],
              [ 2.        ,  0.8660254 ],
              [ 3.        ,  0.8660254 ]],

             [[ 0.5       ,  1.73205081],
              [ 1.5       ,  1.73205081],
              [ 2.5       ,  1.73205081]]]
    
    """
    # Initialize the dimensions for the SOM lattice
    map_coords = np.zeros((nrows, ncols, 2))
    
    if grid_type == 'hex':
        # Height of hexagon is distance from parallel edges
        hex_height = 1
        # Width of hexagon can be calculated from height
        hex_width_center = math.sqrt(3) * hex_height / 2.0
        # Create the coordinates from the center of the hexagon
        # Starting from 1,1 to keep hexagons and lattice in the upper 
        # right cartesian plane. 
        for i in range(1, nrows+1):
            for j in range(1, ncols+1):
                # Even rows will be the 'offset' left to the odd rows like:
                #  * * * *
                # * * * *
                #  * * * *
                if i%2 == 0:
                    map_coords[i-1,j-1] = np.array([(j - 1/2.) * hex_height, i * hex_width_center])
                else:
                    map_coords[i-1,j-1] = np.array([(j) * hex_height, i * hex_width_center])
                    
    else:
        if grid_type != 'square':
            print("Invalid Grid type: %s, defaulting to 'square'" % grid_type)
       
        for i in range(nrows):
            for j in range(ncols):
                map_coords[i,j] = np.array([i,j])
        
    return map_coords
        
def run_som(data, som_grid, grid_type, niter, radius_param, alpha_param, keep_dist=True):
    """Self Organized Map algorithm.
    
    Run data through a Self Organizing Map (SOM) algorithm. The SOM 
    will return updated weights for the nodes of the map. The algorithm will
    run over 'niter' iterations, randomly sampling from 'data' rows to find 
    the Best Matching Unit (BMU) in 'som_grid', updating it's neighborhood 
    depending on radius and alpha (learning rate)
    
    Parameters:
        'data' (2D numpy array) - rows represent a data sample and every
            column represents a feature. 
            e.g. 3 features, 2 samples: [[0.5, 0.2, 1.0], [0.7, 0.2, 0.0]]
            
        'som_grid' (3D numpy array) - a representation of the SOM map where 
            row and column index represent the conceptual space / location 
            for each node. The 3rd dimension is the actual cartesian 
            x,y location for the node, which is used for calculating 
            distances between nodes. 
            e.g. 2 rows by 3 columns with hexagonal node coordinates
                [[[0.5, 1.732], [1.5, 1.732], [2.5, 1.732]], 
                 [[1.0, 0.866], [2.0, 0.866], [3.0, 0.866]]]
                 
        'grid_type' (string) - 'hex' or 'square' for how the 2D lattice 
            should handle distances and coordinates
                 
        'niter' (int) - the number of iterations to run the algorithm. 
            Should be a number quite larger than number of data 
            samples so that each data sample is seen at least once.
            
        'radius_param' (float or tuple) - the initial value for the radius 
            of neighbors that will by default decay to zero. If tuple, 
            first element is starting radius, second is ending radius.
            
        'alpha_param' (float or tuple) - the initial value for the learning 
            rate that will by default decay to 0.01. If tuple, first 
            element is starting learning rate, second is ending learning rate.
            
        'keep_dist' (boolean) - whether to keep track of the shortest 
            distances for each data sample to its closest node. If 
            True, a list of distances is returned.
            
    Returns:
        Tuple with following elements:
        First element, SOM weights (3D numpy array): 
            a representation of the SOM map where 
            row and column index represent the conceptual space / location 
            for each node. The 3rd dimension is a vector for the SOM 
            unit weights, which have been updated from initially random 
            to organized by the algorithm.
        Second element, object distances (tuple):
            a (distance, bmu_idx) tuple where distance is the distance 
            between the BMU and data sample and bmu_idx is the index of 
            this vector in the SOM.
    """
    # Handle the radius and alpha (learning rate) params
    if isinstance(radius_param, tuple):
        init_radius = radius_param[0]
        end_radius = radius_param[1]
    else:
        init_radius = radius_param
        end_radius = 0.0

    if isinstance(alpha_param, tuple):
        init_learning_rate = alpha_param[0]
        end_learning_rate = alpha_param[1]
    else:
        init_learning_rate = alpha_param
        end_learning_rate = 0.01

    num_segments = data.shape[0]
    num_feats = data.shape[1]
    
    map_rows = som_grid.shape[0]
    map_cols = som_grid.shape[1]
    
    # Initialized SOM unit weights to be random
    som_weights = np.random.random((map_rows, map_cols, num_feats))
    
    # Create a list of None values to store shortest distances
    object_distances = [None] * num_segments
    
    for i in range(niter):
        # Select a training example at random
        rand_samp_idx = np.random.randint(0, num_segments)
        rand_samp = data[rand_samp_idx, :].reshape(num_feats, 1)

        # Find its Best Matching Unit (BMU)
        bmu, bmu_idx = find_bmu(rand_samp, som_weights)
        
        # Track the distances of objects to their corresponding winning unit
        # don't bother with actual Euclidean distance, to avoid expensive sqrt operation
        dist_tmp = np.sum((bmu - rand_samp) ** 2)
        # Saving the BMU for this sample by saving the distance and index
        object_distances[rand_samp_idx] = (dist_tmp, bmu_idx)

        # Decay the SOM parameters
        r = decay_func(init_radius, end_radius, i, niter)
        # Decay the learning rate
        learn_rate = decay_func(init_learning_rate, end_learning_rate, i, niter)
        
        # Now we know the BMU, update its weight vector to move closer to input
        # and move its neighbours in 2-D space closer by a factor proportional 
        # to their 2-D distance from the BMU
        for x in range(map_rows):
            for y in range(map_cols):
                # Get the 2-D distance (again, not the actual Euclidean distance)
                if grid_type != 'hex':
                    w_dist = np.sum((np.array([x, y]) - bmu_idx) ** 2)
                else:
                    # Distance if hexagonal map:
                    w_dist = np.sum((som_grid[x, y] - som_grid[bmu_idx[0], bmu_idx[1]]) ** 2)
                
                # If the distance is within the current neighbourhood radius
                # Since we did not take sqrt of w_dist, square radius r
                if w_dist <= r**2:
                    # Get the node weight to update
                    node_weight = som_weights[x, y, :].reshape(num_feats, 1)
                    # NOTE: Currently keeping influence set at 1, meaning no 
                    # decaying based on distance
                    # Calculate the degree of influence (based on the 2-D distance)
                    #influence = calculate_influence(w_dist, r)
                    influence = 1
                    # Now update the neuron's (unit) weight using the formula:
                    # new w = old w + (learning rate * influence * delta)
                    # where delta = input vector (t) - old weight
                    new_w = node_weight + (learn_rate * influence * (rand_samp - node_weight))
                    # Commit the new weight
                    som_weights[x, y, :] = new_w.reshape(1, num_feats)
                     
    if keep_dist:
        return (som_weights, object_distances)
    else:
        return som_weights
        

def cluster_som(som_weights, n_clusters):
    """Hierarchical clustering algorithm.
    
        Recursively merges the pair of clusters that minimally increases a
        a given linkage distance. 
        
        Parameters:
            som_weights (3D numpy array) - a representation of the SOM map where 
                row and column index represent the conceptual space / location 
                for each node. The 3rd dimension is the actual cartesian 
                x,y location for the node, which is used for calculating 
                distances between nodes. 
                e.g. 2 rows by 3 columns with hexagonal node coordinates
                    [[[0.5, 1.732], [1.5, 1.732], [2.5, 1.732]], 
                     [[1.0, 0.866], [2.0, 0.866], [3.0, 0.866]]]
                     
            n_clusters (int) - the number of clusters to group.
            
        Returns:
            clustering (sklearn.cluster.AgglomerativeClustering object)
                - clustering object fit on the data 'som_weights'
    """
    # Get the number of rows, cols for the som_weight matrix
    rows = som_weights.shape[0]
    cols = som_weights.shape[1]
    # The z-dimension will be the number of features
    num_feats = som_weights.shape[2]
    # Get the clustering model to apply to the som_weights
    clustering = AgglomerativeClustering(
        n_clusters = n_clusters, affinity='euclidean', linkage='complete')
    # Reshape the som_weights into a 2D matrix
    tmp_net = som_weights.reshape((rows*cols, num_feats))
    clustering.fit(tmp_net)
    # Return the fitted clustering object model
    return clustering
    
    
def fill_bmu_distances(data, som_weights, object_distances=None):
    """Complete all or fill missing Best Matching Units distances. 
    
    Find the BMU distances and BMU index for each data sample in 'data'.
    Optionally, if 'object_distances' not None then only find the 
    BMU for any missing sample in 'object_distances'.
    
    Parameters:
        'data' (2D numpy array) - rows represent a data sample and every
            column represents a feature. 
            e.g. 3 features, 2 samples: [[0.5, 0.2, 1.0], [0.7, 0.2, 0.0]]
            
        'som_weights' (3D numpy array): a numpy array where the first two 
            dimensions are the row, column of the map and the 3rd 
            dimension is a vector representing the unit weights
            for that row, col cell. The unit vector weights should have 
            the same length as the 'target' vector. e.g.
            [ [ [0.5,1.0,0.3] , [1.0,0.2,0.6] ],
              [ [0.1,0.9,0.9] , [0.7,0.6,0.5] ] ]
              
        'object_distances' (optional) (list of tuples) - a list of tuples 
            for each data sample. First element in tuple is the 
            Euclidean distance from data sample to SOM weight vector of 
            the closest BMU and the second element in tuple is the 
            index for that BMU. If not None, then this is used to 
            only fill in the (distances, bmu_index) of the missing data 
            samples. 
            
    Returns:
        A list of tuples for the distances and BMU index of the 
        data samples to it's closest SOM weight vector.
    
    """
    
    filled_objects = []
    if object_distances == None:
        num_segments = data.shape[0]
        object_distances = [None] * num_segments
        
    num_samples = data.shape[0]
    num_feats = data.shape[1]
    
    for i, data_samp in enumerate(object_distances):
        sample = object_distances[i]
        if sample is None:
            none_samp = data[i, :].reshape(np.array([num_feats, 1]))

            # find its Best Matching Unit (BMU)
            bmu, bmu_idx = find_bmu(none_samp, som_weights)

            # Track the distances of objects to their corresponding winning unit
            # don't bother with actual Euclidean distance, to avoid expensive sqrt operation
            dist_tmp = np.sum((bmu - none_samp) ** 2)
            filled_objects.append((dist_tmp, bmu_idx))

        else:
            filled_objects.append(data_samp)

    return filled_objects
    
def map_cluster_to_class(class_df_path, cluster_df_path):
    """NOT DEVELOPED"""
    class_df = pandas.read_csv(class_df_path)
    cluster_df = pandas.read_csv(cluster_df_path)
    class_df.columns = class_df.columns.str.lower()
    
    merged_df = class_df.merge(cluster_df, how="left", left_index=True, right_index=True)
    cluster_list = cluster_df['cluster'].tolist()
    unique_clusters = np.unique(cluster_list)
    print(unique_clusters)
    
    cluster_to_class_map = {}
    for clust in unique_clusters:
        result = merged_df[merged_df['cluster']==clust].groupby('class').size().reset_index(name='counts')
        print(clust)
        print(result)
        
    return None
    
def save_cluster_results(dframe, results_path, cluster_labels, grid_dim, object_distances):
    """Determine the cluster assignment for each data sample and save to CSV. 
    
    Parameters:
        'dframe' (pandas df) - dataframe of the data samples.
        'results_path' (string) - path on disk to save the CSV with 
            column name 'cluster' for cluster number assignment to 
            each data sample from 'dframe'.
        'cluster_labels' (numpy array) - array of assigned cluster labels 
            from 'labels_' attribute returned from somutils.cluster_som
        'grid_dim' (tuple) - row, col dimension of the SOM map / lattice
        'object_distances' (list of tuples) -  a list of tuples 
            for each data sample. First element in tuple is the 
            Euclidean distance from data sample to SOM weight vector of 
            the closest BMU and the second element in tuple is the 
            index for that BMU.
        
    Returns:
        Nothing
    
    """
    dframe_copy = dframe.copy()
    dframe_index_list = dframe_copy.index.values
   
    nrows = grid_dim[0]
    ncols = grid_dim[1]
    
    tmp_clust = cluster_labels.reshape((nrows, ncols))
    
    cluster_series = pandas.Series(index=dframe_index_list)
    
    for i, df_index in enumerate(dframe_index_list):
        sample = object_distances[i]
        if sample is not None:
            xy_loc = list(sample[1])
            clust_val = tmp_clust[xy_loc[0], xy_loc[1]]
            cluster_series[df_index] = clust_val
        else:
            print("A BMU not calculated for sample: %s" % i)            
        
    cluster_df = pandas.DataFrame(cluster_series, dframe_index_list, ['cluster'])
    dframe_merge = dframe_copy.merge(cluster_df, how='left', left_index=True, right_index=True)
    dframe_merge.to_csv(results_path)
    
def basic_som_figure(data, som_weights, map_coords, cluster_labels, grid, figure_path, dframe=None, class_name=None):
    """Creates and returns a plot for the SOM.
    
    Creates a plot of the SOM based on the coordinates generated during the 
    creation of the SOM lattice. Will also color the plot based on 
    clustered labels and plot the data sample index if dataframe provided.
    
    Parameters:
        'data' (2D numpy array) - rows represent a data sample and every
            column represents a feature. 
            e.g. 3 features, 2 samples: [[0.5, 0.2, 1.0], [0.7, 0.2, 0.0]]
        'som_weights' (3D numpy array) - a numpy array where the first two 
            dimensions are the row, column of the map and the 3rd 
            dimension is a vector representing the unit weights
            for that row, col cell. The unit vector weights should have 
            the same length as the 'target' vector. e.g.
            [ [ [0.5,1.0,0.3] , [1.0,0.2,0.6] ],
              [ [0.1,0.9,0.9] , [0.7,0.6,0.5] ] ]
        'map_coords' (3D numpy array) - a representation of the SOM map where 
                row and column index represent the conceptual space / location 
                for each node. The 3rd dimension is the actual cartesian 
                x,y location for the node, which is used for calculating 
                distances between nodes. 
                e.g. 2 rows by 3 columns with hexagonal node coordinates
                    [[[0.5, 1.732], [1.5, 1.732], [2.5, 1.732]], 
                     [[1.0, 0.866], [2.0, 0.866], [3.0, 0.866]]]
        'cluster_labels' (numpy array) - a numpy array of integers of the 
            assigned clusters to the SOM map.
        'grid' (string) - 'hex' or 'square'. The grid type that was used when 
            creating the SOM map/lattice.
        'figure_path' (string) - a path on disk to save the plotted figure to.
        'dframe' (pandas dataframe) (optional) - dataframe of the data 
            samples. This is different from 'data' in that it can include a
            proper dataframe index, as well as other non data columns that 
            could be useful when plotting. When provided, used to plot the 
            dataframe index (data sample index) onto the plotted SOM in the 
            location of that data samples BMU.
        'class_name' (string) (optional) - column name found in 'dframe' that 
            indicates an integer column of known classifications for data 
            samples. If present will plot the data samples id's in a color 
            based on the classification integer.
        
    Returns:
        Saves the plot to 'figure_path' and returns the Pyplot. 

    """
    default_colors = ['blue', 'green', 'red', 'cyan', 'magenta', 'yellow', 'black', 'white']
    
    x_loc = map_coords[:,:,0]
    y_loc = map_coords[:,:,1]
    #cm_subsection = np.linspace(0, 1, 193)
    
    if grid == 'hex':
        # Compute radius
        hex_radius = 2 / math.sqrt(3) * 0.5
        fig, axs = plt.subplots(ncols=1)
        axs.set_aspect('equal')
        color_idx = 0
        patch_collection = []
        for xc, yc, in zip(x_loc.flatten(), y_loc.flatten()):
            group = cluster_labels[color_idx]
            #print(group)
            #print(color_idx)
            hex = patches.RegularPolygon((xc,yc), numVertices=6, radius=hex_radius, alpha=0.5, edgecolor='k', facecolor=default_colors[group])
            axs.add_patch(hex)
            color_idx+=1
            
        xmin = np.min(x_loc)
        xmax = np.max(x_loc)
        ymin = np.min(y_loc)
        ymax = np.max(y_loc)
        axs.set_xlim(xmin - 1.0, xmax + 1.0)
        axs.set_ylim(ymin - 1.0, ymax + 1.0)
        
        legend_patch = []
        print(np.unique(cluster_labels))
        for item in np.unique(cluster_labels):
            item_patch = patches.Patch(color=default_colors[item], label='Cluster %s' % item)
            legend_patch.append(item_patch)
            
        #axs.legend(handles=legend_patch, loc='upper center', bbox_to_anchor=(0.5, 1.2), ncol=int(math.ceil(len(legend_patch) / 2.0)))
        axs.legend(handles=legend_patch, loc='lower center', bbox_to_anchor=(0.5, -0.3), ncol=int(math.ceil(len(legend_patch) / 2.0)))

    if dframe is not None:
        num_segments = data.shape[0]
        print("num segs: ", num_segments)
        num_feats = data.shape[1]
        dframe_index_list = dframe.index.values.tolist()
        print("len of dframe: ", len(dframe_index_list))
        
        if class_name is not None:
            class_values = dframe[class_name].tolist()
            unique_classes = np.unique(class_values)
            cmap_name = 'tab10'
            cmap = plt.get_cmap(cmap_name)
            classes_norm = colors.Normalize(vmin=np.min(unique_classes), vmax=np.max(unique_classes))
            
            text_colors = ["blue", "yellow", "orange", "red", "green", "purple"]
        
        for i in range(num_segments):
            t = data[i, :].reshape(np.array([num_feats, 1]))
            #id = dframe.loc[i, idfield]
            id = dframe_index_list[i]
            
            # find its Best Matching Unit (BMU)
            bmu, bmu_idx = find_bmu(t, som_weights)
            x_cord = map_coords[bmu_idx[0], bmu_idx[1], 0]
            y_cord = map_coords[bmu_idx[0], bmu_idx[1], 1]
            #print x_cord, y_cord
            x_cord = x_cord + np.random.normal(0.0, 0.1)
            y_cord = y_cord + np.random.normal(0.0, 0.1)
            if class_name is not None:
                class_val = class_values[i]
                #axs.text(x_cord, y_cord, s=id, color=cmap(classes_norm(class_val)))
                axs.text(x_cord, y_cord, s=id, color=text_colors[class_val - 1])
            else:
                axs.text(x_cord, y_cord, s=id, color='black')
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.title('Clusters on the SOM Map', fontsize=16)
    plt.savefig(figure_path,bbox_inches='tight')
    return plt
    
def create_som_figure(data, segment_df, som_weights, map_coords, cluster_labels, grid):
    """
    """
    
        
    color_map = ['blue', 'red', 'black', 'cyan', 'green', 'magenta', 'white']
    #fig = plt.figure()
    x_loc = map_coords[:,:,0]
    y_loc = map_coords[:,:,1]
    #cm_subsection = np.linspace(0, 1, 193)
    
    if grid == 'hex':
        # Compute radius
        hex_radius = 2 / math.sqrt(3) * 0.5
        fig, axs = plt.subplots(ncols=1)
        axs.set_aspect('equal')
        color_idx = 0
        patch_collection = []
        for xc, yc, in zip(x_loc.flatten(), y_loc.flatten()):
            group = cluster_labels[color_idx]
            hex = patches.RegularPolygon((xc,yc), numVertices=6, radius=hex_radius, alpha=0.5, edgecolor='k', facecolor=color_map[group])
            #print(hex.get_path())
            #print(hex.get_patch_transform())
            #print(hex.xy)
            #sys.exit()
            axs.add_patch(hex)
            color_idx+=1
        #print(axs.patches)
        
        # NOTE: might be able to draw boundary line around clusters by 
        # iterating over each hexagon, getting its neighbours and 
        # depending on if a neighbor is a different cluster, draw a line along 
        # that path segment, by getting the patch path. Maybe keep a list 
        # of path segments drawn so they aren't drawn twice
        #sys.exit()
        xmin = np.min(x_loc)
        xmax = np.max(x_loc)
        ymin = np.min(y_loc)
        ymax = np.max(y_loc)
        axs.set_xlim(xmin - 1.0, xmax + 1.0)
        axs.set_ylim(ymin - 1.0, ymax + 1.0)
        
        
        #axs.scatter(x_loc.flatten(), y_loc.flatten())
        num_segments = data.shape[0]
        num_feats = data.shape[1]
        
        regime_colors = ["blue", "yellow", "orange", "red", "green", "purple"]
        
        dframe_index_list = segment_df.index.values.tolist()
        
        for i in range(num_segments):
            t = data[i, :].reshape(np.array([num_feats, 1]))
            #regime_type = segment_df.iloc[i, 5]
            #id = segment_df.iloc[i, 0]

            regime_type = segment_df.iloc[i, 4]
            id = dframe_index_list[i]

            # find its Best Matching Unit (BMU)
            bmu, bmu_idx = find_bmu(t, som_weights, num_feats)
            x_cord = map_coords[bmu_idx[0], bmu_idx[1], 0]
            y_cord = map_coords[bmu_idx[0], bmu_idx[1], 1]
            #print x_cord, y_cord
            x_cord = x_cord + np.random.normal(0.0, 0.1)
            y_cord = y_cord + np.random.normal(0.0, 0.1)
            axs.text(x_cord, y_cord, s=id, color=regime_colors[regime_type - 1])
        
        plt.show()
    
    else:
        for i in range(num_segments):
            t = data[i, :].reshape(np.array([num_feats, 1]))
            regime_type = segment_df.iloc[i, 5]

            # find its Best Matching Unit (BMU)
            bmu, bmu_idx = find_bmu(t, som_weights)
            #print bmu, bmu_idx
            x_loc.append(bmu_idx[0])
            y_loc.append(bmu_idx[1])
            color_loc.append(color_map[regime_type - 1])
            
        plt.scatter(x_loc, y_loc)
        plt.grid(True)
        plt.show()
        
def save_som_model(som_weights, som_path, grid_type, cluster=None):
    """Save SOM model in a text file with SOM creation information.
    
    Saves the SOM as a .txt. The SOM is flattened, meaning that the 
    SOM weights are saved as a 1D array. In order to reconstruct the 
    SOM weights, the original numpy shape and grid type are saved in 
    the footer of the txt file to be read in by somutils.load_som_model.
    
    Parameters:
        'som_weights' (3D numpy array) - a numpy array where the first two 
            dimensions are the row, column of the map and the 3rd 
            dimension is a vector representing the unit weights
            for that row, col cell. The unit vector weights should have 
            the same length as the 'target' vector. e.g.
            [ [ [0.5,1.0,0.3] , [1.0,0.2,0.6] ],
              [ [0.1,0.9,0.9] , [0.7,0.6,0.5] ] ]
        'som_path' (string) - a path on disk to save the model to as a txt
            file. Path should end with a '.txt' extension.
        'grid_type' (string) - 'hex' or 'square'. The grid type that was used when 
            creating the SOM map/lattice.
        'cluster' (int) (optional) - number of clusters used when originally
            clustering the SOM.
    Returns:
        Nothing
    
    """
    som_shape = som_weights.shape
    
    som_flattened = som_weights.flatten()
    
    if cluster == None:
        footer_info = "{'shape':%s,'grid_type'%s}" % (str(som_shape), grid_type)
    else:
        footer_info = "{'shape':%s,'grid_type':%s,'cluster':%s}" % (str(som_shape), grid_type, cluster)
    
    np.savetxt(som_path, som_flattened, footer=footer_info)
    
def load_som_model(som_model_path):
    """Load a saved SOM model.
    
    Loads the model and reconstructs it based on the information saved in 
    the footer of the text file. Returns the reconstructed model as well 
    as the details saved in the footer of the text file.
    
    Parameters:
        'som_model_path' (string) - the path on disk to the saved text file 
            of the SOM model.
    
    Returns:
        A tuple (som_model, som_details)
        'som_model' (3D numpy array) - the reconstructed SOM.
        'som_details' (dictionary) - the information saved in the footer 
            of text file with keys: 'shape', 'grid_type', and 'cluster'
    """
    
    som_model_flat = np.loadtxt(som_model_path)
    # Read in footer details that are tagged on all saved som models
    footer_details = ""
    with open(som_model_path, 'r') as fh:
        for line in fh:
            pass
        
        footer_details = line
        
    # Get the details as a dictionary. The last line should look like:
    # # {'shape':(x,y,z),'cluster':c}\n
    # So first index of 2, removes # and whitespace, and -1 removes the 
    # newline character
    som_details = ast.literal_eval(footer_details[2:-1])
    
    som_model_full = som_model_flat.reshape(som_details['shape'])
    
    return som_model_full, som_details
