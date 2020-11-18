# Run self-organizing map to cluster event metrics
# This script is based off of the R script, sedRegData_V2.R, from Kristen Underwood
# Edited according to Brittany L's Run_SOM.R script, which is an adaptation (more generalized?) version of KU's

# Load packages ----
  library("tidyverse") # general workhorse
  library("kohonen")   # to run the SOM
  library("vegan")     # to run adonis() for calc of nonparametric F test
  # devtools::install_github("laresbernardo/lares")
  library("lares")     # examine var correlations
  library("ggforce")   # for facet plots on multiple pages

  # Load the range normalization function written by Kristin Underwood
  source("Scripts/L2norm.R")

# Create a new folder to save the plots and CSVs produced by the SOM code
newFolder <- paste("Data/somResults/wade/", Sys.Date(), sep = "")
dir.create(newFolder, recursive = TRUE)
  
# Read in data ----
  # Calculated event metrics for each site as calculated in compile_calculate_allVars.R
  wade <- read_csv("Data/eventMetrics_wade.csv") %>% 
    # Code the multipeak col as numerical
    mutate(multipeak = ifelse(multipeak == "NO", 0, 1))

# List the variables that will not be used in the analysis (e.g., response/dependent & ID/INFO)
drop.info <- names(wade %>% 
                     select(# Remove the non-numerical ID/INFO
                            site, season, event_start,
                            # Remove response variables
                            NO3_kg_km2, SRP_kg_km2, event_NO3_SRP, turb_kg_km2))

# Select vars to keep Step 1 ----
# Look at correlations between variables
wade %>% 
  # Remove the vars listed above
  select(-one_of(drop.info)) %>% 
  # Remove all rain_Xd vars and keep API instead
  select(-c(rain_1d:rain_30d)) %>% 
  # Removal all GW event data (e.g., max and change)
  select(-c(starts_with("gw_event_"))) %>% 
  # Remove all pre-event GW levels for individual wells and keep the aggregate ones
  select(-c(starts_with(c("gw_1d_well", "gw_4d_well")))) %>% 
  # Remove all pre-event soil var means and keep the aggregate "dry" and "wet" ones
  select(-c(contains("WWp"))) %>% 
  # Remove all 30cm & 45cm aggreate soil vars except VWC
  select(-c(DO_pre_dry_30cm, Redox_pre_dry_30cm, SoilTemp_pre_dry_30cm,
            DO_pre_wet_30cm, Redox_pre_wet_30cm, SoilTemp_pre_wet_30cm,
            DO_pre_dry_45cm, Redox_pre_dry_45cm, SoilTemp_pre_dry_45cm,
            DO_pre_wet_45cm, Redox_pre_wet_45cm, SoilTemp_pre_wet_45cm)) %>%
  # For now, let's drop the aggreate soil vars for the dry site and keep the wet ones
  select(-c(contains("pre_dry"))) %>% 
  # Keep only gw well 5 for now
  # select(-c(ends_with("well1"))) %>% 
  # Keep only complete observations/rows (no NAs in any of the columns)
  na.omit() %>% 
  # Visualize these
  lares::corr_cross(max_pvalue = 0.10, top = 20)

# Look at plots of correlated variables
  # GW level ~ VWC
  # wade %>%
  #   pivot_longer(cols = c(VWC_pre_pit6_15cm, VWC_pre_pit6_30cm, VWC_pre_pit6_45cm), names_to = "var", values_to = "VWC") %>%
  #   ggplot(aes(x = gw_4d_well1, y = VWC, color = var)) +
  #   geom_point(shape = 1) +
  #   geom_abline(slope = 1) +
  #   theme_classic()

  # Other correlations
  # scatter plot comparison
  wade %>% 
    ggplot(aes(x = q_event_delta, y = q_event_max)) +
    geom_point(shape = 1) +
    geom_abline(slope = 1) +
    geom_smooth(method = "lm") +
    theme_classic()
  # boxplot comparison
  wade %>% 
    pivot_longer(cols = c(q_event_delta, q_event_max), names_to = "var", values_to = "value") %>% 
    ggplot(aes(x = var, y = value)) +
    geom_boxplot()
  
# Decisions
  # If the 1-d and 4-d values for a variable are highly correlated, use the 4-d value
  # Trying to find a VWC variable that correlates well with GW level so that I can remove the GW level (no GW data in 2017)
    # VWC_pre_wet_30cm is highly correlated (83%) with gw_4d_allWells, so drop gw_4d_allWells
    # Keep VWC_pre_wet_15cm & _30cm (first one to match all other soil vars at 15 cm; second one to rep GW levels), but drop _45cm
  # Q
    # q_1d and q_4d are highly correlated (88.6%), so sticking with rule above will keep the q_4d
    # Drop q_event_dQRate_cmsPerHr b/c it's confusing and hopefully q_event_delta or rain intensity will capture this
    # q_event_delta & q_event_max are highly correlated (90.6%); let's keep both for now
  # Rain
    # Drop all the rain_Xd vars, b/c API_4d should cover this, though would be interesting to test how many days pre-event (e.g., 4 days for API) matters
  # Soil temps
    # soil temp @ 15cm @ pits 1 & 6 highly correlated, so let's keep the temp from pit 6 b/c we're using the VWC from that pit
  
# Look at correlations again without dropped variables
  # List the additional dropped variables
  drop.vars <- names(wade %>% 
                       select(# Remove all rain_Xd vars and keep API instead
                              rain_1d:rain_30d,
                              # Removal all GW event data (e.g., max and change)
                              starts_with("gw_event_"),
                              # Remove all pre-event GW levels for individual wells and keep the aggregate ones
                              starts_with(c("gw_1d_well", "gw_4d_well")),
                              # Remove all pre-event soil var means and keep the aggregate "dry" and "wet" ones
                              contains("WWp"),
                              # Remove all 30cm & 45cm aggreate soil vars except VWC
                              DO_pre_dry_30cm, Redox_pre_dry_30cm, SoilTemp_pre_dry_30cm,
                              DO_pre_wet_30cm, Redox_pre_wet_30cm, SoilTemp_pre_wet_30cm,
                              DO_pre_dry_45cm, Redox_pre_dry_45cm, SoilTemp_pre_dry_45cm,
                              DO_pre_wet_45cm, Redox_pre_wet_45cm, SoilTemp_pre_wet_45cm,
                              # For now, let's drop the aggreate soil vars for the dry site and keep the wet ones
                              contains("pre_dry"),
                              # Drop these highly correlated variables
                              q_1d,
                              gw_1d_allWells,
                              gw_4d_allWells,
                              VWC_pre_wet_45cm,
                              # Drop this complicated variable
                              q_event_dQRate_cmsPerHr
                              ))

  # Create df w/ dropped variables
  wade_drop <- wade %>% 
    # Drop variables
    select(-one_of(drop.info, drop.vars))

  # Look at correlations again without dropped variables
  wade_drop %>% 
    # Keep only complete observations/rows (no NAs in any of the columns)
    na.omit() %>% 
    lares::corr_cross(max_pvalue = 0.10, top = 20)
  
# SOM ----  
  # PREPARE DATA ----
    # To begin, let's only use observations where there are no NAs
    myData <- wade_drop %>% 
      # Keep only complete observations/rows (no NAs in any of the columns)
      na.omit()

    # We're left with only 44 obs/rows of 76; but if we determine certain variables aren't useful for clustering
    # we can remove them and reiterate this process and may end up with more obs/rows
  
    # Not exactly sure why this exists; relic of KU's script
    myDataSet <- c("wade")  #identify data set version
    
    # Vector of observation number from original dataset
    observ <- myData %>% 
      mutate(obs = row_number()) %>% 
      dplyr::pull(obs)
  
  # SET UP THE GRID/LATTICE FOR THE SOM ----
  # Run this code once to determine which dimensions to input into the params_SOM . . .csv file that is saved as the 'params' object below
    # According to the heuristic rule from Vesanto 2000, number of grid elements/grid size/nodes = 5 * sqrt(n)
    VesantoNodes = round(5 * sqrt(nrow(myData)), 0)
    
    # To determine the the shape of the grid, we'll determine the ratio of columns to rows using the ratio of 
    # the first two eigen values of the input data set as recommended by Park et al. 2006
    # PCA
    # scale = TRUE = correlation matrix; scale = FALSE = covariance matrix
    PCA <- prcomp(myData, scale = TRUE, center = TRUE)
    eigenValues <- PCA$sdev^2
    first <- nth(eigenValues, 1)
    second <- nth(eigenValues, 2)
    gridRatio <- round((first/second), 1)
  
    # Input this information into the params_SOM . . .csv file
    
    
# SOM Code from Kristen Underwood's sedRegData_V2.R script
# Also adapted by referencing Brittney L's Run_SOM.R script
# ------------- Define Color Palettes of SOM plots  -------------------------------------------

# Colour palette definition - this added from blog comments (found in 2014-01 CSO_SOM.R script)
# Used later in clustering plot
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

# Palette defined by kohonen package, Shane Lynn 14-01-2014, code in zip file downloaded from link in above blog
# Used later in Component Plane (HeatMap) plots
coolBlueHotRed <- function(n, alpha = 1) {
      rainbow(n, end=4/6, alpha=alpha)[n:1]
}

#barPalette=c("gray", "red", "green3", "blue","cyan", "magenta", "yellow", "black")
barPalette=c("red", "orange1", "green3", "blue","cyan", "magenta", "yellow", "black")
sedRegPallette=c("cyan", "yellow", "orange", "red", "green3", "orchid","gray", "blue")    
    
# ------------- Set Grid parameters for various Runs  -------------------------------------------

# Read in table of parameters (grid rows, cols, grid type, learning rate etc)
### IMPORTANT: cannot have number nodes greater than num observations
params <- read_csv("Data/somParams/params_SOM_wade_2020-11-18.csv")
params <- as.matrix(params[1:nrow(params),])

# initialize matrix to store results of each SOM run
temp1 <- matrix(0, nrow(params), 5) #initialize empty matrix to append to params
Results <- cbind(params, temp1) # concatenate empty columns to copy of params matrix

# initialize matrix to store pattern to node assignments by run
clustAssignments <- matrix(0, 1, nrow(myData))

# initialize matrix to store pattern to node assignments by run
#clustMeans_Res <- matrix(0, 1, ncol(myData_num))

# LOOP THROUGH CHOICES OF PARAMS BY RUN #
  #i <- 1  #use this to break for loop  #end for loop near line 410
  for (i in 1:nrow(params)) {
        #### Define SOM input parameters
        myRun <- params[i,1]
        myDataSet <- params[i,2]
        myTopo <- params[i,4]
        rows <- as.numeric(params[i,5])
        cols <- as.numeric(params[i,6])
        normMeth <- params[i,7]
        wghtMeth <- params[i,8]
        niter <- as.numeric(params[i,9])  #number of iterations, 100 is default
        crsAlpha <- as.numeric(params[i,10]) #learning rate: coarse tuning phase, 0.05 is default
        finAlpha <- as.numeric(params[i,11]) #learning rate: fine tuning phase, 0.01 is default
        nclusters <- as.numeric(params[i,12])
        #for now, rest of som parameters are default values
        
       
        # -----------------Normalize the data ---------------------------------
        ### output from either is a matrix, which is format required by som()
        
        if (normMeth == "scale") {
              # Method 1 - z transformation (subtract mean, divide by stdev)
              myData.sc <- scale(myData)  #scale is a base R function
              
        } else {
              # Method 2 - L2 normalization by variable (scale between 0 and 1)
              # normMeth <- c("L2")
              # I load this up top (note from DK)
              # source("L2norm.R")  #function has been created by KU
              myData.sc <- L2norm(myData)
        }
        
        # ----------------- Weight the inputs (Optional) ---------------------------------
        
        if (wghtMeth == "PCA") {
          ### Weight the data based on the PC loadings.
          # copy the data into a new object for weighting
          myData_wghtd <- myData.sc 
          
          # Multiply the individual variables by the weights:
          
          # this "for" loop goes through each column in the data 
          # which is going to go into the SOM, matches the column name to the
          # names of the PCA loadings for PC1, and multiplies those loadings 
          # by the scaled values in the column to get weighted values:
          
          #ADD a zero weight for variable L ??
          for (w in 1:ncol(myData_wghtd)) {
            myData_wghtd[,w]<-myData_wghtd[,w] * weightsPCA[which(names(weightsPCA) == colnames(myData_wghtd)[w])]
          }
          myData.sc <- myData_wghtd
        }
        
        # ---------------------- Run the SOM ----------------------------------
        
        #set.seed(12)  #to replicate results 
        myGrid <- somgrid(cols, rows, topo = myTopo)  #function somgrid is sourced from the "class" package
        som_model <- som(X = myData.sc, grid = myGrid, rlen = niter,
                         alpha=c(0.05,0.01), 
                         keep.data = TRUE)  #run the som
        
        #, mode = c("online")  #add this as arg to som()
        #, neighbourhood.fct = c("bubble")  #add this as arg to somgrid? no need, it is default
        
        # Calculate errors
        QE <- mean(som_model$distances)  # Quantization Error
        # kohonen_2017 now generates this automatically in summary() and print()
        
        # OLD kohonen_2015 way of calculating topographic error
        #TEdist <- topo.error(som_model, type = "nodedist") #another option
        #TEbmu <- topo.error(som_model, type = "bmu") # Topographic Error
        
        # NEW kohonen_2017 way  #### 4/10: this section still needs work
        obj.dists <- object.distances(som_model, type = "data")
        # Or create An object of class "dist", which can be directly fed into 
        # (e.g.) a hierarchical clustering.
        code.dists <- object.distances(som_model, type = "codes")
        
        un.dists <- unit.distances(myGrid)
        
        check1 <- 'check1'
        print(check1)
        
        # ------------------- Assign Nodes to a Cluster -------------------------------------
        
        #according to dissimilarity between trained weight vectors
        #given nClusters specified in params;
        #can review performance of SOM under various cluster # by doing mult runs
        #OLD CODE
        #som_cluster <- cutree(hclust(dist(som_model$codes)), nclusters)
        #NEW CODE
        som_cluster <- cutree(hclust(code.dists), nclusters)
       
        check2 <- 'check2'
        print(check2)    
        # -------------- Visualize results in various plots (exported to pdf) --------------------------
        myFileName2 <- paste(myRun,"SOMplots",myDataSet,wghtMeth,normMeth, paste0(rows,"x", cols, myTopo), paste0(nclusters,"cl"), sep = "_")
        pdf(file=paste(newFolder, myFileName2, sep = "/"), height=8, width=10)  #dev.off() is at 400
        par(mfrow=c(1,2), mar= c(5.5, 4, 4, 2) + 0.1)
        opar<-par()
        
        # Node counts
        plot(som_model, type = "count", main = "Node Counts", shape = "straight")
        mtext(paste("nodes =", (rows*cols)), side = 1, line = 1)
        mtext(paste("QE = ",round(QE, digits = 3)), side=1, line = 2)
        #mtext(paste("TEbmu =",round(TEbmu, digits = 3)), side=1, line = 3)
        
        # Quality Map
        plot(som_model, type = "quality", main = "Mapping Quality", shape = "straight")
        mtext(paste("Data Set =", myDataSet), side=1, line = 1)
        mtext(paste("Normalization Method =", normMeth), side=1, line = 2)
        mtext(paste("Weighted Inputs =", wghtMeth), side=1, line = 3)
        mtext(paste(myFileName2), side=3, outer=TRUE)
        # A good mapping should show small distances everywhere in the map". (Wehrens & Buydens, 2007)
        # mean distance of objects, mapped to a particular node, to the codebook vector of that node
        
        check3 <- 'check3'
        print(check3)
        
        # Training Map
        plot(som_model, type="changes", main = "Training Progress")
        
        # U-Matrix
        plot(som_model, type = "dist.neighbours", main = "U Matrix", shape = "straight", palette.name=grey.colors)
        
        # Codes/ Weight Vectors #default type = "codes"
        plot(som_model, main = "Weight Vectors", shape = "straight", codeRendering = "segments")
        
        # Hierarchical Clustering plot
        #plot(hclust(dist(som_model$codes)))   #OLD CODE
        plot(hclust(code.dists))
        
        # K-Means Clustering plot  ##NEW CODE: had to convert som_model$codes to data frame by adding as.data.frame(som_model$codes)
        wss <- (nrow(as.data.frame(som_model$codes))-1)*sum(apply(as.data.frame(som_model$codes),2,var)) 
        for (k in 2:8) {  #this can be no more than min(grid elements) minus 1 
              wss[k] <- sum(kmeans(as.data.frame(som_model$codes), centers=k)$withinss)
        }
        plot(wss, main= "K-Means Clustering")
        
        # plot the clustering results and add cluster boundaries: 
        plot(som_model, type="mapping", bgcol = barPalette[som_cluster], shape = "straight", main = "Clusters")
        add.cluster.boundaries(som_model, som_cluster) #this is a function within kohonen
        
        # U-Matrix with cluster boundaries
        plot(som_model, type = "dist.neighbours", main = "U Matrix", shape = "straight", palette.name=grey.colors)
        add.cluster.boundaries(som_model, som_cluster)
        
        check4 <- 'check4'
        print(check4)
        
        
        # Component Planes (Heat Maps) - Standardized Variable
        # Plot the component planes for each variable using a loop:
        ### NEW CODE: had to convert som_model$codes to a data frame in each call below
        par(mfrow = c(3, 3))
        par(cex = 0.6)
        par(mar = c(3, 3, 0, 0), oma = c(0, 0, 0, 0))
        #par(mgp = c(2, 0.5, 0))
        
        #par(mfrow=c(4,5), mai=c(0.2,0.4,0.2,0))  #mar = c(2,1,1,1), oma=c(0,0,0,0)
        for (c in 1:ncol(as.data.frame(som_model$codes))) {
              plot(som_model, type = "property", cex.lab = 1.4, border = "gray50", shape = "straight", property = as.data.frame(som_model$codes)[,c], main=colnames(as.data.frame(som_model$codes))[c], palette.name = coolBlueHotRed)
              add.cluster.boundaries(som_model, som_cluster)
        }
        
        # # # Component Planes (Heat Maps) - Unstandardized variable
        # ## NOTE: KU will need to add modification to this code from Shayne Lynn blog to address a 
        # # lattice that has empty nodes.
        # # intention is to map the average unscaled value (for a given variable) for 
        # # all input patterns mapped to each node
        # for (i in 1:ncol(som_model$codes)) {
        #       node_mns <- aggregate(as.numeric(myData_num[,i]), by=list(som_model$unit.classif), 
        #                                 FUN=mean, simplify=TRUE)[,2]
        #       #var_unscaled <- #need to fill in zero values for unoccupied nodes
        #       plot(som_model, type = "property", property=var_unscaled, main=colnames(myData_num)[i], palette.name=coolBlueHotRed)
        # }
        # 
        # # OR could try "reverse scaling" the codebook vectors stored in each node,
        # # by multiplying by the stdev of the original variables and
        # # adding the mean of the original variables
        # # perhaps there is a reverse function for scale() or I will need to calculate and store 
        # # stedev and mean in the early lines of the code for use here
        
        # Plot input patterns assigned to each node onto the clustered map
        par(mfrow=c(1,2), mar=c(6,3,3,1), oma=c(0,0,3,0))
        plot(som_model, type = "mapping" , shape = "straight", labels = som_model$unit.classif, main="Node ID Map (syst chk)") # maps node identification
        #plot(som_model, type = "mapping", labels = som_cluster, col=som_cluster+1, main="Mapping Plot")
        #add.cluster.boundaries(som_model, som_cluster)
        
        check5 <- 'check5'
        print(check5)      
        
        # Brittany didn't use the following chunck; commenting out for now
        ### Now match node numbers back to original [nrow] input patterns for plotting
        # newlabels <- cbind(som_model$unit.classif, observ, mySRClass)
        # plot(som_model, type = "mapping", shape = "straight", labels = newlabels[ ,2], cex=0.75, main="Input Patterns \nMapped to each Node")
        # #plot(som_model, type = "mapping", shape = "straight", labels = newlabels[ ,2], col = sedRegPallette[as.integer(mySRClass)], cex=0.75, main="Input Patterns \nMapped to each Node")
        # add.cluster.boundaries(som_model, som_cluster)
        
        ### Now match the cluster assignments back to the original [nrow] input patterns for plotting:
        # such as the line graphs with color coding for clusters that Peter Isles made
        # som_model$unit.classif # this is the node assignments for each data point
        # som_cluster # this is the cluster assignment for each node
        cluster_assign<-som_cluster[som_model$unit.classif] 
        # each data point, could be appended to the original dataset 
        cluster_assign
        
        # Now plot scaled variables by cluster as they differ from the mean
        par(mfrow=c(3,2), mar=c(7,3,3,1), oma=c(0,0,3,0))
        
        #p <- 1 #use this line to break loop for debugging
        for (p in 1:length(unique(cluster_assign))) {
              clusterData<- as.matrix(myData.sc[which(cluster_assign==p),], byrow = TRUE)
              if (nrow(clusterData) == 17) {
                    clusterData <- t(as.matrix(clusterData))
                    clusterMeans <- clusterData  # in case cluster has only one pattern
                    numPatterns <- 1
              } else {
                    clusterMeans<-apply(clusterData, 2, mean)
                    numPatterns <- nrow(clusterData)
              }
              
              dataMeans<-apply(myData.sc, 2, mean)  #compute overall mean for comparison
              barplot(clusterMeans-dataMeans, names.arg=names(clusterData), ylim = c(-.8,.8), las=3, main=paste("Cluster ", p, sep=""), col=barPalette[p])
              # barplot(clusterMeans-dataMeans, names.arg=names(clusterData), las=3, main=paste("Cluster ", p, sep=""), col=barPalette[p])
              mtext(paste("n =", numPatterns), side = 3, line = -0.5)
        }  #end for p loop  #ylim = c(-1.2, 1.2),
        
        #       # plot bar plots of variable values by cluster
        #       # bottom margin = 6.4 to leave room for x label with vertical hatch mark labels
        #       par(mfrow=c(3,2), mar=c(6.5,3,3,1), oma=c(0,0,3,0))
        #       for (pp in 1:length(unique(cluster_assign))) {
        #             clusterData<- as.matrix(myData.sc[which(cluster_assign==pp),], byrow = TRUE)
        #             if (nrow(clusterData) >1) {
        #                   numPatterns <- nrow(clusterData)
        #             } else {
        #                   clusterData <- t(as.matrix(clusterData)) # in case cluster has only one pattern
        #                   numPatterns <- 1
        #             }
        #             boxplot(clusterData ~ colnames(clusterData), xlab = "", ylab = "", 
        #             names=paste(colnames(clusterData)), las=2, cex=2, pch="*", col=barPalette[pp], 
        #             main=paste("Cluster ", pp, sep=""))
        #             mtext(paste("n =", numPatterns), side=3, line=0.1, at=0.0)
        # 
        #       }  #end for pp loop
        #       
        #repeat cluster plot for reference and using same color coding as bar charts
        par(mfrow=c(1,2), mar=c(6,3,3,1), oma=c(0,0,3,0))
        plot(som_model, type="mapping", shape = "straight", bgcol = barPalette[som_cluster], main = "Clusters")
        add.cluster.boundaries(som_model, som_cluster) 
        #mtext(paste("group mean - data mean for vars", normMeth), side=3, outer=TRUE)
        
        # Brittany did not use the next 2 chunks; commenting out for now
        #repeat plot of input patterns mapped to each node for ref next to cluster plot
        # plot(som_model, type = "mapping", shape = "straight", labels = newlabels[ ,2], cex=0.75, main="Input Patterns \nMapped to each Node")
        # add.cluster.boundaries(som_model, som_cluster)
        
        #repeat large plot of input patterns mapped to each node for ref next to cluster plot
        # par(mfrow=c(1,1), mar=c(6,3,3,1), oma=c(0,0,3,0))
        # plot(som_model, type = "mapping", shape = "straight", labels = newlabels[ ,2], col = sedRegPallette[as.integer(mySRClass)], cex=1.1, main="Input Patterns \nMapped to each Node \nColored by Mike Class")
        # add.cluster.boundaries(som_model, som_cluster)
        
        
        
        ###############
        #calculate nonparametric F statistic
        
        #first create dataframe of cluster assignments to call from the adonis2() function
        obsNum <- as.character(paste0("obs",observ))
        clustAssignDF <- as.data.frame(cbind(obsNum, cluster_assign))
        colnames(clustAssignDF) <- c('obsNum','Cluster') 
        #then run NP F statistic using adonis function of vegan package
        npFtest <- adonis2(obj.dists ~ Cluster, data=clustAssignDF, by = "terms", permutations=99)
        #extract F ratio and pVal for storage in results table
        npF <- npFtest$F[1]
        pVal_npF <- npFtest$`Pr(>F)`[1]
        
        check6 <- 'check6'
        print(check6)
        
        # pp <- 1 #use this line to break loop for debugging
        # #for (pp in 1:length(unique(cluster_assign))) {
        #   finalWghtsData<- as.matrix(myData.sc[which(cluster_assign==pp),], byrow = TRUE)
        #   if (nrow(clusterData) == 18) {
        #     clusterData <- t(as.matrix(clusterData))
        #     clusterMeans <- clusterData  # in case cluster has only one pattern
        #     numPatterns <- 1
        #   } else {
        #     clusterMeans<-apply(clusterData, 2, mean)
        #     numPatterns <- nrow(clusterData)
        #   }
        #   
        #   dataMeans<-apply(myData.sc, 2, mean)  #compute overall mean for comparison
        #   barplot(clusterMeans-dataMeans, ylim = c(-1, 2.5), names.arg=names(clusterData), las=3, main=paste("Cluster ", pp, sep=""), col=barPalette[pp])
        #   mtext(paste("n =", numPatterns), side = 3, line = -0.5)
        # #}  #end for pp loop
        
        
        dev.off()
        
        # ------------------- Track and Store results of each Run -------------------
        Results[i,14]<- mean(som_model$distances)
        Results[i,15] <- round(QE, digits = 6)
        Results[i,16] <- round(npF, digits = 6)
        Results[i,17] <- round(pVal_npF, digits = 6)
        
        clustAssignments <- rbind(clustAssignments, cluster_assign)
          
        
  } # END FOR LOOP OF SOM RUNS


# FORMAT & EXPORT RESULTS TABLES ----
colnames(Results) <- c('Run','DataSet','Nodes','Topol','rows','cols','normMeth', 
                       'wghtMeth','niter', 'alphaCrs', 'alphaFin', 
                       'Clusters', 'ColRowRat', 'Dist_mn','QE','npF','pVal_npF','blank') 

ResultsDF <- as_tibble(Results) %>% type_convert()
write_csv(ResultsDF, paste0(newFolder,"/","Results_", myDataSet,"_",nclusters,"_cl",".csv"))

clustAssignm <- clustAssignments[-1, ]
colnames(clustAssignm) <- as.character(paste0("obs",observ))
# runID <- as.matrix(params[,1])
# colnames(runID)<- c("Run")
clustAssignm <- cbind (Results, clustAssignm)

clustAssignmDF <- as_tibble(clustAssignm)
write_csv(clustAssignmDF, paste0(newFolder,"/","ClustAssign_",myDataSet,"_",nclusters,"_cl",".csv"))


# CHOOSE BEST LATTICE CONFIGURATION ----
  # Examine the non-parametric F-stat (npF) and quantization error (QE) to choose the best lattice configuration
  # You want to maximize npF (maximize b/w cluster variance while minimizing w/i cluster variance) &
  # Minimize QE (avg distance b/w each data vector & best-matching unit [BMU])

  # Set coefficient for second axis on plot below
  # Note: this might not be perfect and may need to adjust
  coeff <- min(ResultsDF$npF)/max(ResultsDF$QE)
  
  # Plot npF and QE
  ResultsDF %>% 
    # Parse columns into correct type (done above)
    # type_convert() %>% 
    # Create lattice config ID (rows x cols)
    mutate(latID = paste(rows, cols, sep = "x")) %>% 
    # Plot
    ggplot(aes(x = reorder(latID, -npF))) +
    geom_bar(aes(y = npF, fill = "npF"), stat= "identity") +
    geom_line(aes(y = QE * coeff, group = 1, color = "QE")) +
    geom_point(aes(y = QE * coeff, group = 1, color = "QE"), size = 2) +
    scale_y_continuous(
      name = "npF",
      sec.axis = sec_axis(~. / coeff, name = "QE")
    ) +
    scale_fill_manual(values = "gray70") +
    scale_color_manual(values = "blue") +
    xlab("Lattice config. (rows x cols)") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.spacing = unit(-0.5, "lines"),
          panel.grid = element_blank(),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))
  
  # Save plot
  ggsave(paste0(newFolder,"/","npFvsQE.pdf"), height = 4, width = 6, units = "in", dpi = 75)
  
# Choose lattice dimensions you want to examine based on npf/QE plot above
  n_rows = 6
  n_cols = 9
  
# CREATE DF WITH EVENT IDs & CLUSTER #'s ----  
datWithCluster <- clustAssignmDF %>% 
  # Parse columns into correct type
  type_convert() %>% 
  filter(rows == n_rows & cols == n_cols) %>% 
  # Pivot to longer format
  pivot_longer(cols = obs1:ncol(.), names_to = "obs", values_to = "cluster") %>% 
  select(cluster) %>% 
  # Bind cluster ID to myData
  bind_cols(myData) %>% 
  # Join to original data to get event IDs
  left_join(wade) %>% 
  # Drop all the independent variables you didn't use in the SOM (copy from myData above); 
  # but KEEP the INFO vars
  select(-one_of(drop.vars)) %>% 
  # Arrange columns
  select(site:ncol(.), everything())
  
  # Write to CSV
  write_csv(datWithCluster, paste0(newFolder, "/", "SOMresults", "_", myDataSet, "_", Sys.Date(), "_", nclusters, "cl", "_", n_rows, "x", n_cols, ".csv"))

# BOXPLOTS OF INDEPENDENT VARIABLES BY CLUSTER ----
  # # of plots (use this to decide how many PDF pages you want to plot all the plots on)
  df <- datWithCluster %>% 
    pivot_longer(cols = DOY:ncol(.), names_to = "var", values_to = "value")
  length(unique(df$var))

  # Plot & save
  pdf(paste0(newFolder, "/", "boxplots_varsByCluster", "_", n_rows, "x", n_cols, ".pdf"),
      width = 7.5, height = 10)
  for(i in 1:2){ # this is the # of PDF pages your plots will be spread across
    print(datWithCluster %>% 
      pivot_longer(cols = DOY:ncol(.), names_to = "var", values_to = "value") %>% 
      ggplot(aes(x = cluster, y = value, group = cluster)) +
      facet_wrap_paginate(~var, scales = "free_y", ncol = 4, nrow = 3, page = i) +
      geom_boxplot(fill = "white") +
      geom_point(position=position_jitter(width=0.2), shape=1, size=1, color="gray50", alpha=0.6) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            strip.text = element_text(size = 7),
            panel.spacing.x = unit(0.3, "inches"),
            panel.spacing.y = unit(0.45, "inches")))
  }
  dev.off()