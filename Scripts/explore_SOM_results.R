# Explore output of SOM

# Load packages
  library("tidyverse")
  library("lubridate")
  library("cowplot")
  library("grid")
  library("gridExtra")  

# Additional functions
  # Functions required to reorder a column/levels with faceting, such that the values are ordered within each facet
  # Source: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R
    # reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    #   new_x <- paste(x, within, sep = sep)
    #   stats::reorder(new_x, by, FUN = fun)
    # }
    # 
    # scale_x_reordered <- function(..., sep = "___") {
    #   reg <- paste0(sep, ".+$")
    #   ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
    # }
    # 
    # # We need scale_x_reordered, but here is the y-axis equivalent
    # scale_y_reordered <- function(..., sep = "___") {
    #   reg <- paste0(sep, ".+$")
    #   ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
    # }
  
  
# Read in data
  # Most recent SOM results
  hford <- read_csv("Data/somResults/hford/2020-11-18/SOMresults_hford_2020-11-18_4cl_5x8.csv") %>% 
    mutate(event_start = ymd_hms(event_start, tz = "Etc/GMT+4"))
  wade <- read_csv("Data/somResults/wade/2020-11-18/SOMresults_wade_2020-11-18_4cl_6x9.csv") %>% 
    mutate(event_start = ymd_hms(event_start, tz = "Etc/GMT+4"))
  
  # Calculated hysteresis metrics
  hyst <- read_csv("Data/hysteresis_indices.csv") %>%
    mutate(event_start = ymd_hms(event_start, tz = "Etc/GMT+4"),
           event_end = ymd_hms(event_end, tz = "Etc/GMT+4"))  
    
  # # SOM cluster output
  #   # Hungerford
  #   som_hf_4C_14V <- read_csv("Data/cluster_results_hf_4clusters_14vars.csv") %>% 
  #     rename(event_start = "X1", weight = "0") %>% 
  #     mutate(event_start = ymd_hms(event_start, tz = "Etc/GMT+4")) %>% 
  #     mutate(site = "Hungerford") %>% 
  #     # There were event_start + weight + cluster replicates; not sure if these
  #     # are meaningful, but removing them for now
  #     distinct()
  #   # Wade
  #   som_wd_4C_15V <- read_csv("Data/cluster_results_wd_4clusters_15vars.csv") %>% 
  #     rename(event_start = "X1", weight = "0") %>% 
  #     mutate(event_start = ymd_hms(event_start, tz = "Etc/GMT+4")) %>% 
  #     mutate(site = "Wade") %>% 
  #     # There were event_start + weight + cluster replicates; not sure if these
  #     # are meaningful, but removing them for now
  #     distinct()    
    

# Plotting specifics
  theme1 <- theme_classic() +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 12),
                  axis.title.x = element_text(margin=margin(5,0,0,0)),
                  axis.title.y = element_text(margin=margin(0,5,0,0)),
                  legend.title = element_text(size = 9),
                  legend.text = element_text(size = 9))
  
  # A new theme for the hysteresis plots
  theme3 <- theme(axis.line = element_blank(),
                  axis.text = element_text(size = 8),
                  axis.title = element_blank(),
                  plot.margin = unit(c(1.25,1.25,1.25,1.25), "lines"),
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.text = element_text(size = 6),
                  legend.key.size = unit(0.1, "in"),
                  legend.margin = margin(0, 0, 0, 0),
                  legend.title = element_text(size = 6)) 
  
  # Labels for the panels by cluster
  # labels <- c("0" = "Cluster 0", "1" = "Cluster 1", "2" = "Cluster 2", "3" = "Cluster 3", "4" = "Cluster 4")  
  
  # Text for Clockwise, Counterclockwise, Diluting, Flushing on the plots
  text_cw <- textGrob("Clockwise", gp = gpar(fontsize = 10))
  text_ccw <- textGrob("Counterclockwise", gp = gpar(fontsize = 10))
  text_dil <- textGrob("Diluting", gp = gpar(fontsize = 10), rot = 90)
  text_flu <- textGrob("Flushing", gp = gpar(fontsize = 10), rot = 270)  
  
 
# Summarize the clusters
  # HUNGERFORD
    # Look at seasonal differences by cluster
    hford %>% 
      group_by(cluster, season) %>% 
      tally() %>% 
      # Order the seasons
      mutate(season = factor(season, levels = c("spring", "summer", "fall"))) %>%
      ggplot(aes(x = cluster, y = n, fill = season)) +
        geom_bar(position = "stack", stat = "identity") +
        scale_fill_manual(name = "Season",
                          breaks = c("spring", "summer", "fall"),
                          labels = c("Spring", "Summer", "Fall"),
                          values = c("#0072B2", "#009E73", "#E69F00")) +
        ylab("No. of events") +
        xlab("Cluster") +
        theme1
    ggsave("Plots/SOM_hf_clustersBySeason.png", width = 4, height = 4, units = "in", dpi = 150)

    # Look at z-scores by cluster
    # Plotting median z-scores
    hford %>% 
      # Rename clusters
      mutate(cluster = paste("Cluster", cluster, sep = " ")) %>% 
      # Calculate z-scores for each independent variable
      mutate_at(vars(c(DOY:ncol(.))),
              .funs = list(~ (. - mean(., na.rm = T)) / sd(., na.rm = T))) %>%
      pivot_longer(cols = DOY:ncol(.), names_to = "var", values_to = "value") %>% 
      group_by(cluster, var) %>% 
      summarize(median = median(value, na.rm = T)) %>% 
      ggplot(aes(x = var, y = median)) +
        # Must use scales = "free" when using the reordering functions
        facet_wrap(~cluster) +
        geom_bar(stat = "identity") +
        # scale_x_reordered() +
        ylab("Median z-scores") +
        coord_flip() +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.title.y = element_blank())
    ggsave("Plots/SOM_hf_zScoresByCluster.png", width = 6, height = 6, units = "in", dpi = 150)
    
    # Plotting mean z-scores + SE
    hford %>% 
      # Rename clusters
      mutate(cluster = paste("Cluster", cluster, sep = " ")) %>% 
      # Calculate z-scores for each independent variable
      mutate_at(vars(c(DOY:ncol(.))),
              .funs = list(~ (. - mean(., na.rm = T)) / sd(., na.rm = T))) %>%
      pivot_longer(cols = DOY:ncol(.), names_to = "var", values_to = "value") %>% 
      group_by(cluster, var) %>% 
      summarize(mean = mean(value),
                SE = sd(value)/sqrt(length(!is.na(value)))) %>% 
      ggplot(aes(x = var, y = mean)) +
        # Must use scales = "free" when using the reordering functions
        facet_wrap(~cluster) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width = 0.2, position = position_dodge(0.9)) +
        # scale_x_reordered() +
        ylab("Median z-scores") +
        coord_flip() +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.title.y = element_blank())
    
    # Plot timeline of event yields & ratios colored by cluster number
    hford %>% 
      mutate(log_event_NO3_SRP = log(event_NO3_SRP)) %>% 
      pivot_longer(cols = c(NO3_kg_km2, SRP_kg_km2, log_event_NO3_SRP, turb_kg_km2), names_to = "var", values_to = "value") %>% 
      mutate(var = factor(var, levels = c("NO3_kg_km2", "SRP_kg_km2", "log_event_NO3_SRP", "turb_kg_km2"))) %>% 
      # Add leading zero to single digit months and days
      mutate(year = year(event_start),
             month = str_pad(month(event_start), 2, pad = 0),
             day = str_pad(day(event_start), 2, pad = 0),
             month_day = paste(month, "/", day, sep = "")) %>%
      ggplot(aes(x = month_day, y = value, fill = as.factor(cluster))) +
        facet_wrap(var ~ year, scales = "free", ncol = 3) +
        geom_bar(stat = "identity") +
        scale_fill_manual(name = "Cluster",
                          values = c("#009E73", "#D55E00", "#CC79A7", "#0072B2")) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(angle = 90),
              axis.title.y = element_blank())
    ggsave("Plots/SOM_hf_eventsByCluster.png", width = 10, height = 6, units = "in", dpi = 150)
    
    # Plot NO3:SRP yield ratios against water yield
    hford %>% 
      ggplot(aes(x = q_mm, y = log(event_NO3_SRP), color = as.factor(cluster))) +
        geom_point(size = 2, stroke = 0.75, alpha = 0.8) +
        #geom_smooth(method=lm, se=TRUE) +
        scale_color_manual(name = "Cluster",
                          values = c("#009E73", "#D55E00", "#CC79A7", "#0072B2")) +
        # geom_hline(yintercept = 16, linetype = "dashed", size = 0.75, color = "gray40") +
        # ylab(expression(paste(Ratio~of~"NO"["3"]^" -"~" : "~SRP~event~yield))) + 
        ylab(expression(atop(log~Molar~ratio~of~NO[3]^-{}~":"~SRP~yield, "for"~each~event))) +
        xlab("Event water yield (mm)") +
        # scale_y_continuous(limits = c(0, 1300), breaks=seq(0, 1200, 300)) +
        # facet_wrap(~site, labeller = labeller(var = labels, site = labels2)) +
        theme1
     ggsave("Plots/SOM_hf_ratiosBYWaterYield.png", width = 4, height = 4, units = "in", dpi = 150)
     
    # Look at how clusters map onto HI and FI
    hford %>% 
      left_join(hyst, by = c("site", "event_start")) %>%
      pivot_longer(cols = c(FI_NO3, FI_SRP, HI_NO3_mean, HI_SRP_mean), names_to = "var", values_to = "value") %>% 
      ggplot(aes(x = cluster, y = value, group = cluster)) +
        facet_wrap(~var, scales = "free") +
        geom_boxplot() +
        geom_hline(yintercept = 0)
    
    # Nitrate
    pl_hifi_no3 <- 
      hford %>% 
      left_join(hyst, by = c("site", "event_start")) %>%
      ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(aes(x = FI_NO3, y = HI_NO3_mean, color = as.factor(cluster)), size = 2, alpha = 0.8) +
      scale_color_manual(name = "Cluster",
                         values = c("#009E73", "#D55E00", "#CC79A7", "#0072B2")) +
      ylim(-1, 1) + xlim(-1, 1) +
      ylab("Storm hysteresis index") + xlab("Storm flushing index") +
      theme3 +
      ggtitle("Nitrate") + 
      theme(plot.title = element_text(hjust = 0, size = 12),
            legend.position = c(0.8, 0.85),
            legend.box = "horizontal") +
      annotation_custom(text_dil, xmin=-1.45, xmax=-1.45, ymin=0, ymax=0) +
      annotation_custom(text_flu, xmin=1.2, xmax=1.2, ymin=0, ymax=0) +
      annotation_custom(text_cw, xmin=0, xmax=0, ymin=1.2, ymax=1.2) +
      annotation_custom(text_ccw, xmin=0, xmax=0, ymin=-1.4, ymax=-1.4) +
      coord_cartesian(clip = "off")
    
    # SRP
    pl_hifi_srp <- 
      hford %>% 
      left_join(hyst, by = c("site", "event_start")) %>%
      ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(aes(x = FI_SRP, y = HI_SRP_mean, color = as.factor(cluster)), size = 2, alpha = 0.8) +
      scale_color_manual(name = "Cluster",
                         values = c("#009E73", "#D55E00", "#CC79A7", "#0072B2")) +
      ylim(-1, 1) + xlim(-1, 1) +
      ylab("Storm hysteresis index") + xlab("Storm flushing index") +
      theme3 +
      ggtitle("SRP") + 
      theme(plot.title = element_text(hjust = 0, size = 12),
            legend.position = "none") +
      annotation_custom(text_dil, xmin=-1.45, xmax=-1.45, ymin=0, ymax=0) +
      annotation_custom(text_flu, xmin=1.2, xmax=1.2, ymin=0, ymax=0) +
      annotation_custom(text_cw, xmin=0, xmax=0, ymin=1.2, ymax=1.2) +
      annotation_custom(text_ccw, xmin=0, xmax=0, ymin=-1.4, ymax=-1.4) +
      coord_cartesian(clip = "off")
  
    # Combine the 2 plots into one
    pl_HI_v_FI_alt <- plot_grid(pl_hifi_no3, pl_hifi_srp, ncol = 2, align = "hv",
                                labels = "auto", hjust = -0.4, vjust = 2.5, scale = 1)
    # Create common x and y axis titles
    y.grob <- textGrob("Storm hysteresis index",
                       gp = gpar(fontsize = 13), rot = 90)
    x.grob <- textGrob("Storm flushing index",
                       gp = gpar(fontsize = 13))
    # Add axis title to plot
    test_alt <- grid.arrange(arrangeGrob(pl_HI_v_FI_alt, left = y.grob, bottom = x.grob))
    ggsave("Plots/HIFI_hf.png", test_alt, width = 6, height = 3.5, units = "in", dpi = 300)   
     
  
  
  
     
  # WADE
    # Look at seasonal differences by cluster
    wade %>% 
      group_by(cluster, season) %>% 
      tally() %>% 
      # Order the seasons
      mutate(season = factor(season, levels = c("spring", "summer", "fall"))) %>%
      ggplot(aes(x = cluster, y = n, fill = season)) +
        geom_bar(position = "stack", stat = "identity") +
        scale_fill_manual(name = "Season",
                          breaks = c("spring", "summer", "fall"),
                          labels = c("Spring", "Summer", "Fall"),
                          values = c("#0072B2", "#009E73", "#E69F00")) +
        ylab("No. of events") +
        xlab("Cluster") +
        theme1
    ggsave("Plots/SOM_wd_clustersBySeason.png", width = 4, height = 4, units = "in", dpi = 150)

    # Look at z-scores by cluster
    # Plotting median z-scores
    wade %>% 
      # Rename clusters
      mutate(cluster = paste("Cluster", cluster, sep = " ")) %>% 
      # Calculate z-scores for each independent variable
      mutate_at(vars(c(DOY:ncol(.))),
              .funs = list(~ (. - mean(., na.rm = T)) / sd(., na.rm = T))) %>%
      pivot_longer(cols = DOY:ncol(.), names_to = "var", values_to = "value") %>% 
      group_by(cluster, var) %>% 
      summarize(median = median(value)) %>% 
      ggplot(aes(x =  var, y = median)) +
        # Must use scales = "free" when using the reordering functions
        facet_wrap(~cluster) +
        geom_bar(stat = "identity") +
        ylab("Median z-scores") +
        coord_flip() +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.title.y = element_blank())
    ggsave("Plots/SOM_wd_zScoresByCluster.png", width = 6, height = 6, units = "in", dpi = 150)
    
    # Plotting mean z-scores + SE
    wade %>% 
      # Rename clusters
      mutate(cluster = paste("Cluster", cluster, sep = " ")) %>% 
      # Calculate z-scores for each independent variable
      mutate_at(vars(c(DOY:ncol(.))),
              .funs = list(~ (. - mean(., na.rm = T)) / sd(., na.rm = T))) %>%
      pivot_longer(cols = DOY:ncol(.), names_to = "var", values_to = "value") %>% 
      group_by(cluster, var) %>% 
      summarize(mean = mean(value),
                SE = sd(value)/sqrt(length(!is.na(value)))) %>% 
      ggplot(aes(x =  var, y = mean)) +
        # Must use scales = "free" when using the reordering functions
        facet_wrap(~cluster) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width = 0.2, position = position_dodge(0.9)) +
        ylab("Median z-scores") +
        coord_flip() +
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.title.y = element_blank())
    
    # Plot timeline of event yields & ratios colored by cluster number
    wade %>% 
      mutate(log_event_NO3_SRP = log(event_NO3_SRP)) %>% 
      pivot_longer(cols = c(NO3_kg_km2, SRP_kg_km2, log_event_NO3_SRP, turb_kg_km2), names_to = "var", values_to = "value") %>% 
      mutate(var = factor(var, levels = c("NO3_kg_km2", "SRP_kg_km2", "log_event_NO3_SRP", "turb_kg_km2"))) %>% 
      # Add leading zero to single digit months and days
      mutate(year = year(event_start),
             month = str_pad(month(event_start), 2, pad = 0),
             day = str_pad(day(event_start), 2, pad = 0),
             month_day = paste(month, "/", day, sep = "")) %>%
      ggplot(aes(x = month_day, y = value, fill = as.factor(cluster))) +
        facet_wrap(var ~ year, scales = "free", ncol = 3) +
        geom_bar(stat = "identity") +
        scale_fill_manual(name = "Cluster",
                          values = c("#009E73", "#D55E00", "#CC79A7", "#0072B2")) +      
        theme_bw() +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(angle = 90),
              axis.title.y = element_blank())
    ggsave("Plots/SOM_wd_eventsByCluster.png", width = 10, height = 6, units = "in", dpi = 150)
    
    
    # Plot NO3:SRP yield ratios against water yield
    wade %>% 
      ggplot(aes(x = q_mm, y = event_NO3_SRP, color = as.factor(cluster))) +
        geom_point(size = 2, stroke = 0.75, alpha = 0.8) +
        #geom_smooth(method=lm, se=TRUE) +
        scale_color_manual(name = "Cluster",
                          values = c("#009E73", "#D55E00", "#CC79A7", "#0072B2")) +
        # geom_hline(yintercept = 16, linetype = "dashed", size = 0.75, color = "gray40") +
        # ylab(expression(paste(Ratio~of~"NO"["3"]^" -"~" : "~SRP~event~yield))) + 
        ylab(expression(atop(Molar~ratio~of~NO[3]^-{}~":"~SRP~yield, "for"~each~event))) +
        xlab("Event water yield (mm)") +
        # scale_y_continuous(limits = c(0, 1300), breaks=seq(0, 1200, 300)) +
        # facet_wrap(~site, labeller = labeller(var = labels, site = labels2)) +
        theme1
     ggsave("Plots/SOM_wd_ratiosBYWaterYield.png", width = 4, height = 4, units = "in", dpi = 150)
     
     
    # Nitrate
    pl_hifi_no3_2 <- 
      wade %>% 
      left_join(hyst, by = c("site", "event_start")) %>%
      ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(aes(x = FI_NO3, y = HI_NO3_mean, color = as.factor(cluster)), size = 2, alpha = 0.8) +
      scale_color_manual(name = "Cluster",
                         values = c("#009E73", "#D55E00", "#CC79A7", "#0072B2")) +
      ylim(-1, 1) + xlim(-1, 1) +
      ylab("Storm hysteresis index") + xlab("Storm flushing index") +
      theme3 +
      ggtitle("Nitrate") + 
      theme(plot.title = element_text(hjust = 0, size = 12),
            legend.position = c(0.8, 0.85),
            legend.box = "horizontal") +
      annotation_custom(text_dil, xmin=-1.45, xmax=-1.45, ymin=0, ymax=0) +
      annotation_custom(text_flu, xmin=1.2, xmax=1.2, ymin=0, ymax=0) +
      annotation_custom(text_cw, xmin=0, xmax=0, ymin=1.2, ymax=1.2) +
      annotation_custom(text_ccw, xmin=0, xmax=0, ymin=-1.4, ymax=-1.4) +
      coord_cartesian(clip = "off")
    
    # SRP
    pl_hifi_srp_2 <- 
      wade %>% 
      left_join(hyst, by = c("site", "event_start")) %>%
      ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(aes(x = FI_SRP, y = HI_SRP_mean, color = as.factor(cluster)), size = 2, alpha = 0.8) +
      scale_color_manual(name = "Cluster",
                         values = c("#009E73", "#D55E00", "#CC79A7", "#0072B2")) +
      ylim(-1, 1) + xlim(-1, 1) +
      ylab("Storm hysteresis index") + xlab("Storm flushing index") +
      theme3 +
      ggtitle("SRP") + 
      theme(plot.title = element_text(hjust = 0, size = 12),
            legend.position = "none") +
      annotation_custom(text_dil, xmin=-1.45, xmax=-1.45, ymin=0, ymax=0) +
      annotation_custom(text_flu, xmin=1.2, xmax=1.2, ymin=0, ymax=0) +
      annotation_custom(text_cw, xmin=0, xmax=0, ymin=1.2, ymax=1.2) +
      annotation_custom(text_ccw, xmin=0, xmax=0, ymin=-1.4, ymax=-1.4) +
      coord_cartesian(clip = "off")
  
    # Combine the 2 plots into one
    pl_HI_v_FI_alt_2 <- plot_grid(pl_hifi_no3_2, pl_hifi_srp_2, ncol = 2, align = "hv",
                                labels = "auto", hjust = -0.4, vjust = 2.5, scale = 1)
    # Create common x and y axis titles
    y.grob <- textGrob("Storm hysteresis index",
                       gp = gpar(fontsize = 13), rot = 90)
    x.grob <- textGrob("Storm flushing index",
                       gp = gpar(fontsize = 13))
    # Add axis title to plot
    test_alt_2 <- grid.arrange(arrangeGrob(pl_HI_v_FI_alt_2, left = y.grob, bottom = x.grob))
    ggsave("Plots/HIFI_wd.png", test_alt_2, width = 6, height = 3.5, units = "in", dpi = 300)      
     