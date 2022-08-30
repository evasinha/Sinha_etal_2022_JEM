# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

# Function details
# plot_line                             - Make line plot
# plot_point_line                       - Make point line plot
# plot_line_facet                       - Make line plot with facet wrap
# plot_line_facet_grid                  - Make line plot with facet grid
# plot_point_line_facet                 - Make point and line plot with facet wrap
# plot_point_facet                      - Make point plot with facet wrap
# plot_point_facet_cont_color           - Make point plot with facet wrap with continuous color gradient
# plot_point_facet_grid_cont_color      - Make point facet grid plot with continuous color gradient
# plot_point_line_facet_grid            - Make point and line plot with facet grid
# plot_point_bin_hexa_facet_grid        - Make scatter plot along with hexagonal heat map of point counts
# plot_percentage_change_several_values - Plot showing percentage change from Reference scenario for various values
# plot_column_facet_wrap                - Make column plot with facet wrap
# plot_column_facet_grid                - Make column plot with facet grid
# plot_column_facet_geo                 - Make column plot with facet geo
# plot_line_facet_geo                   - Make line plot with facet geo
# plot_timeseries_facet_grid_select_scenario - Make timeseries and column plot with facet grid from data

#_______________________________________________________________________________
# Make area plot
plot_area <- function(plot_data, x_col, y_col, group_col, fill_col, leg_nrow, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, group=group_col, fill=fill_col)) + 
    geom_area() +
    scale_x_continuous(breaks=seq(from=2015, to=2100, by=15), expand=c(0.01,0.01)) +
    labs(y     = y_lab,
         title = plot_title) + 
    guides(color=guide_legend(title=NULL, nrow=leg_nrow)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          legend.position  = 'bottom',
          legend.key.size  = unit(2,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_blank(),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make line plot
plot_line <- function(plot_data, x_col, y_col, group_col, color_col, leg_nrow, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, group=group_col, color=color_col)) + 
    geom_line(lwd=1.25) +
    scale_x_continuous(breaks=seq(from=2015, to=2100, by=15), expand=c(0.01,0.01)) +
    scale_color_manual(values=color_pal) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(color=guide_legend(title=NULL, nrow=leg_nrow)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          legend.position  = 'bottom',
          legend.key.size  = unit(2,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make point line plot
plot_point_line <- function(plot_data, x_col, y_col, group_col, color_col, 
                            leg_nrow, y_lab, plot_title,  y_min, y_max, y_inc) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, group=group_col, color=color_col)) + 
    geom_point(size=2) +
    geom_line(lwd=1.25) +
    scale_x_continuous(breaks=seq(from=2015, to=2100, by=15), expand=c(0.01,0.01)) +
    scale_y_continuous(limits=c(y_min, y_max),breaks=seq(from=y_min, to=y_max, by=y_inc),  expand=c(0.01,0.01)) +
    scale_color_manual(values=color_pal) +
    labs(y     = y_lab,
         title = plot_title) + 
    guides(color=guide_legend(title=NULL, nrow=leg_nrow)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          legend.position  = 'bottom',
          legend.key.size  = unit(2,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_blank(),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make point line plot
plot_point_linetype <- function(plot_data, x_col, y_col, group_col, color_col, linetype_col,
                            leg_nrow, y_lab, plot_title,  y_min, y_max, y_inc) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, group=group_col, color=color_col, linetype=linetype_col)) + 
    geom_point(size=2) +
    geom_line(lwd=1.25) +
    scale_x_continuous(breaks=seq(from=2015, to=2100, by=15), expand=c(0.01,0.01)) +
    scale_y_continuous(limits=c(y_min, y_max),breaks=seq(from=y_min, to=y_max, by=y_inc),  expand=c(0.01,0.01)) +
    scale_color_manual(values=color_pal) +
    labs(y     = y_lab,
         title = plot_title) + 
    guides(color=guide_legend(title=NULL, nrow=leg_nrow), linetype=guide_legend(title=NULL)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          legend.box       = 'vertical',
          legend.position  = 'bottom',
          legend.key.size  = unit(2,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_blank(),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make line plot with facet wrap
plot_line_facet <- function(plot_data, x_col, y_col, group_col, color_col, linetype_col,
                            facet_var, facet_ncol, facet_scale, leg_nrow, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, group=group_col, color=color_col, linetype=linetype_col)) + 
    geom_line(lwd=1.25) +
    facet_wrap(as.formula(paste('~', facet_var)), scales=facet_scale, ncol=facet_ncol) +
    scale_x_continuous(breaks=seq(from=2015, to=2100, by=15), expand=c(0.01,0.01)) +
    scale_y_continuous(breaks= scales::pretty_breaks()) +
    scale_color_manual(values=color_pal) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(color=guide_legend(title=NULL, nrow=leg_nrow), linetype=guide_legend(title=NULL)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          panel.spacing.x  = unit(1.6, 'lines'),
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text       = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.box       = 'vertical',
          legend.position  = 'bottom',
          legend.key.size  = unit(2,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'),
          plot.margin      = unit(c(5.5, 15.5, 5.5, 5.5), 'points'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make line plot with facet grid
plot_line_facet_grid <- function(plot_data, x_col, y_col, group_col, color_col, linetype_col,
                                 facet_row, facet_col, facet_scale, leg_nrow, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, group=group_col, color=color_col, linetype=linetype_col)) + 
    geom_line(lwd=1.25) +
    facet_grid(as.formula(paste(facet_row, '~', facet_col)), scales=facet_scale, switch='y') +
    scale_x_continuous(breaks=seq(from=2015, to=2100, by=15), expand=c(0.01,0.01)) +
    scale_color_manual(values=color_pal) +
    labs(y     = y_lab,
         title = plot_title) + 
    guides(color=guide_legend(title=NULL, nrow=leg_nrow), linetype=guide_legend(title=NULL)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          panel.spacing.x  = unit(1.6, 'lines'),
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA, color=NA),     # Remove background color of facet label but keep black border
          strip.placement  = 'outside',
          strip.text.x     = element_text(hjust=0),
          legend.position  = 'bottom',
          legend.key.size  = unit(2,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_blank(),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
  
}


#_______________________________________________________________________________
# Make point and line plot with facet wrap
plot_point_line_facet <- function(plot_data, x_col, y_col, group_col, color_col,
                                  facet_var, facet_ncol, leg_nrow, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, group=group_col, color=color_col)) + 
    geom_point(size=2) +
    geom_line(lwd=1.25) +
    facet_wrap(as.formula(paste('~', facet_var)), scales='free_y', ncol=facet_ncol) +
    scale_color_manual(values=color_pal) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(color=guide_legend(title=NULL, nrow=leg_nrow)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.key.size  = unit(2,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make point plot with facet wrap
plot_point_facet <- function(plot_data, x_col, y_col, color_col,
                             facet_var, facet_ncol, leg_nrow, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, color=color_col)) + 
    geom_point(size=2) +
    facet_wrap(as.formula(paste('~', facet_var)), scales='fixed', ncol=facet_ncol) +
    scale_color_manual(values=color_pal) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(color=guide_legend(title=NULL, override.aes=list(size=4), nrow=leg_nrow)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.key.size  = unit(2,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make point plot with facet wrap with continuous color gradient
plot_point_facet_cont_color <- function(plot_data, x_col, y_col, color_col, shape_col,
                                        facet_var, facet_ncol, facet_scale, leg_nrow, color_breaks, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, color=color_col, shape=shape_col)) + 
    geom_point(size=2) +
    scale_color_gradientn(colours=rev(matlab.like(20)), breaks=color_breaks) +  # rainbow(20)
    scale_x_continuous(expand=c(0.01,0.01)) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(shape=guide_legend(title=NULL, override.aes=list(size=4))) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.box       = 'vertical',
          legend.key.width  = unit(6, 'lines'),
          legend.key.height = unit(1, 'lines'),
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  if (!is.null(facet_var)) {
    p1 <- p1 + facet_wrap(as.formula(paste('~', facet_var)), scales=facet_scale, ncol=facet_ncol)
  }
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make point facet grid plot with continuous color gradient
plot_point_facet_grid <- function(plot_data, x_col, y_col, color_col, shape_col,
                                  facet_row, facet_col, facet_scale, leg_nrow, 
                                  x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, color=color_col, shape=shape_col)) + 
    geom_point(size=2) +
    facet_grid(as.formula(paste(facet_row, '~', facet_col)), scales=facet_scale) +
    scale_color_manual(values=color_pal) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(shape=guide_legend(title=NULL, override.aes=list(size=4))) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.box       = 'vertical',
          legend.key.width  = unit(6, 'lines'),
          legend.key.height = unit(1, 'lines'),
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))

  return(p1)
  
}

#_______________________________________________________________________________
# Make point facet grid plot with continuous color gradient
plot_point_facet_grid_cont_color <- function(plot_data, stat_ellipse_data, x_col, y_col, color_col, shape_col,
                                             facet_row, facet_col, facet_scale, leg_nrow, color_breaks, 
                                             x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, color=color_col, shape=shape_col)) + 
    geom_point(size=2) +
    facet_grid(as.formula(paste(facet_row, '~', facet_col)), scales=facet_scale) +
    scale_color_gradientn(colours=rev(matlab.like(20)), breaks=color_breaks) +  # rainbow(20)
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(shape=guide_legend(title=NULL, override.aes=list(size=4))) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.box       = 'vertical',
          legend.key.width  = unit(6, 'lines'),
          legend.key.height = unit(1, 'lines'),
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  if(!is.null(stat_ellipse_data)) {
    p1 <- p1 + stat_ellipse(data=stat_ellipse_data, level=0.68, type='norm', show.legend=FALSE)  # level=0.68 corresponds to approximately 1 sigma
  }
  return(p1)
  
}

#_______________________________________________________________________________
# Make point and line plot with facet grid
plot_point_line_facet_grid <- function(plot_data, x_col, y_col, group_col, color_col,
                                       facet_row, facet_col, leg_nrow, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, group=group_col, color=color_col)) + 
    geom_point(size=2) +
    geom_line(lwd=1.25) +
    facet_grid(as.formula(paste(facet_row, '~', facet_col))) +
    scale_color_manual(values=color_pal) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(color=guide_legend(title=NULL, nrow=leg_nrow)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.key.size  = unit(2,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make scatter plot along with hexagonal heat map of point counts
plot_point_bin_hexa_facet_grid <- function(plot_data, point_data, x_col, y_col,
                                           facet_row, facet_col, facet_scale, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col)) + 
    stat_bin_hex(bins=30, size=4) + 
    geom_point(data=point_data, aes(color=Crop, shape=Source), size=3) +
    facet_grid(as.formula(paste(facet_row, '~', facet_col)), scales=facet_scale) +
    scale_fill_gradientn(colours=brewer.pal(9,'YlOrRd')[c(2:9,rep(9,times=5))], name='Count') +
    scale_color_manual(values=color_pal) +
    scale_shape_manual(values=shape_pal) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(color='none') + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.box       = 'vertical',
          legend.key.width  = unit(4, 'lines'),
          legend.key.height = unit(1, 'lines'),
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
  
}

#________________________________________________________
# Plot showing percentage change from Reference scenario for various values
# Lollipop chart with arrows
# https://uc-r.github.io/lollipop
plot_percentage_change_several_values_facet_wrap <- function(plot.data.table, ylim_min, ylim_max, facet_ncol, leg_nrow){
  
  # Subselect regions and add flag for direction of change
  plot.data.table <- plot.data.table %>%
                     filter(scenario != 'Reference') %>%
                     mutate(change_dir = ifelse(per_change < 0, 'decrease','increase'))
  
  rowid <- which(plot.data.table$per_change > ylim_min & plot.data.table$per_change < ylim_max)
  plot.data.table$split_axis <- 'Yes'
  plot.data.table[rowid, 'split_axis'] <- 'No'
  
  label.data.table <- plot.data.table
  
  plot.data.table[which(plot.data.table$per_change > ylim_max), 'per_change'] <- ylim_max
  plot.data.table[which(plot.data.table$per_change < ylim_min), 'per_change'] <- ylim_min
  
  label.data.table[rowid, 'per_change'] <- NA
  label.data.table$label_y <- NA
  label.data.table[which(label.data.table$per_change > ylim_max), 'label_y'] <- ylim_max + floor(7 * ylim_max/100)
  label.data.table[which(label.data.table$per_change < ylim_min), 'label_y'] <- ylim_min - floor(7 * ylim_max/100)
  
  label.data.table[which(label.data.table$per_change > ylim_max), 'sym_y'] <- ylim_max
  label.data.table[which(label.data.table$per_change < ylim_min), 'sym_y'] <- ylim_min
  
  
  p1 <- ggplot(plot.data.table, aes(x=Desc, y=per_change, ymin=0, ymax=per_change, color=scenario)) +
    geom_linerange(size=1, position=position_dodge(0.5), show.legend=FALSE) +
    geom_point(data=filter(plot.data.table, split_axis == 'No'), size=2.5, position=position_dodge(0.5)) + 
    geom_hline(yintercept = 0, color='black') +
    geom_text(data=label.data.table, aes(x=Desc, y=label_y, label=round(per_change, 0), color=scenario), size=5.5, position=position_dodge(0.5)) +
    geom_text(data=label.data.table, aes(x=Desc, y=sym_y, label='\\\\', color=scenario), size=5, position=position_dodge(0.5)) +
    facet_wrap(region ~ ., ncol=facet_ncol) +
    coord_flip(ylim = c(ylim_min, ylim_max)) +
    # scale_x_discrete(limits = rev(unique(plot.data.table$Desc))) +
    scale_color_manual(values=color_pal) +
    scale_y_continuous(breaks=seq(ylim_min, ylim_max, by=10)) +
    labs(y     = NULL,
         title = NULL) +
    guides(color=guide_legend(title=NULL, override.aes=list(size=4), nrow=leg_nrow, reverse=TRUE)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          panel.spacing    = unit(1.5, 'lines'),
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text       = element_text(size=15,family='Helvetica',color='black', hjust=0),
          strip.background = element_rect(fill=NA, color=NA),     # Remove background and border color of facet label
          legend.position  = 'bottom',
          legend.key.size  = unit(1.5,'line'),
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.y     = element_blank(),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'),
          plot.margin      = unit(c(5.5, 10.5, 5.5, 5.5), 'points'))
  
  return(p1)
  
}

#________________________________________________________
# Plot showing percentage change from Reference scenario for various values
# Lollipop chart with arrows
# https://uc-r.github.io/lollipop
plot_percentage_change_several_values_facet_grid <- function(plot.data.table, hline_data, leg_nrow){
  
  # Subselect regions and add flag for direction of change
  plot.data.table <- plot.data.table %>%
                     filter(scenario != 'Reference') %>%
                     mutate(change_dir = ifelse(per_change < 0, 'decrease','increase'))
  
  p1 <- ggplot(plot.data.table, aes(x=Desc, y=per_change, ymin=y_min, ymax=per_change, color=scenario)) +
    geom_linerange(size=1, position=position_dodge(0.5), show.legend=FALSE) +
    geom_point(size=2, position=position_dodge(0.5)) + 
    coord_flip() +
    geom_hline(data=hline_data, aes(yintercept=yintercept), color='black') +
    facet_grid(region ~ break_y_axis, scales='free') +
    # scale_x_discrete(limits = rev(unique(plot.data.table$Desc))) +
    scale_color_manual(values=color_pal) +
    labs(y     = NULL,
         title = NULL) +
    guides(color=guide_legend(title=NULL, override.aes=list(size=4), nrow=leg_nrow, reverse=TRUE)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          panel.spacing    = unit(1.5, 'lines'),
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text       = element_text(size=15,family='Helvetica',color='black', hjust=0),
          strip.background = element_rect(fill=NA, color=NA),     # Remove background and border color of facet label
          legend.position  = 'bottom',
          legend.key.size  = unit(1.5,'line'),
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.y     = element_blank(),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'),
          plot.margin      = unit(c(5.5, 10.5, 5.5, 5.5), 'points'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make column plot with facet wrap
plot_column <- function(plot_data, x_col, y_col, fill_col,
                        x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, fill=fill_col)) +
    geom_col(width=0.75, position='dodge', show.legend=FALSE) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text       = element_text(size=15,family='Helvetica',color='black', hjust=0),
          strip.background = element_rect(fill=NA, color=NA),     # Remove background and border color of facet label
          legend.position  = 'bottom',
          legend.key.size  = unit(1.5,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=10,family='Helvetica',color='black', angle=90, hjust=1, vjust=0.5))
  
  return(p1)
}

#_______________________________________________________________________________
# Make column plot with facet wrap
plot_column_non_dodge <- function(plot_data, x_col, y_col, fill_col,
                        x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, fill=fill_col)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values=color_pal) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    theme_bw() +  # remove background
    guides(fill=guide_legend(title=NULL)) +
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text       = element_text(size=15,family='Helvetica',color='black', hjust=0),
          strip.background = element_rect(fill=NA, color=NA),     # Remove background and border color of facet label
          legend.position  = 'bottom',
          legend.key.size  = unit(1.5,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black'))
  
  return(p1)
}

#_______________________________________________________________________________
# Make column plot with facet wrap
plot_column_facet_wrap <- function(plot_data, x_col, y_col, fill_col,
                                   facet_var, facet_ncol, facet_scale, 
                                   leg_nrow, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, fill=fill_col)) +
    geom_col(width=0.75, position='dodge') +
    geom_hline(yintercept=0, color='black') +
    facet_wrap(as.formula(paste('~', facet_var)), scales=facet_scale, ncol=facet_ncol) +
    scale_fill_manual(values=color_pal) +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(fill=guide_legend(title=NULL, nrow=leg_nrow)) +
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text       = element_text(size=15,family='Helvetica',color='black', hjust=0),
          strip.background = element_rect(fill=NA, color=NA),     # Remove background and border color of facet label
          legend.position  = 'bottom',
          legend.key.size  = unit(1.5,'line'), 
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=10,family='Helvetica',color='black', angle=90, hjust=1, vjust=0.5))
  
  return(p1)
}

#_______________________________________________________________________________
# Make column plot with facet grid
plot_column_facet_grid <- function(plot_data, x_col, y_col, fill_col,
                                   facet_row, facet_col, facet_scale, leg_nrow, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, fill=fill_col)) +
    geom_col(width=0.75) +
    geom_hline(yintercept=0, color='black') +
    facet_grid(as.formula(paste(facet_row, '~', facet_col)), scales=facet_scale, switch='y') +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(fill=guide_legend(title=NULL, nrow=leg_nrow)) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines,
          text              = element_text(size=15,family='Helvetica',color='black'),
          strip.text.x       = element_text(size=15,family='Helvetica',color='black', hjust=0),
          strip.text.y       = element_text(size=15,family='Helvetica',color='black'),
          strip.background.x = element_rect(fill=NA, color=NA),     # Remove background and border color of facet label
          strip.background.y = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          strip.placement    = 'inside',
          legend.position    = 'bottom',
          legend.key.size    = unit(1.5,'line'), 
          legend.text        = element_text(size=15,family='Helvetica',color='black'),
          plot.title         = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y       = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y        = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x        = element_text(size=10,family='Helvetica',color='black', angle=90, hjust=1, vjust=0.5))
  
  return(p1)
}

#_______________________________________________________________________________
# Make column plot with facet geo
plot_column_facet_geo <- function(plot_data, rect_data, gcam_grid, x_col, y_col, color_col, fill_col,
                                  facet_var, leg_nrow, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, color=color_col)) +
 #   geom_rect(data=rect_data, aes_string(fill=fill_col), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.3, inherit.aes=FALSE) +
    geom_col(position='dodge', fill='transparent') +
    facet_geo(as.formula(paste('~', facet_var)), grid = gcam_grid, label='name', scales='free_y') +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(fill=guide_legend(title=NULL, nrow=leg_nrow), color=guide_legend(title=NULL)) +
    scale_color_manual(values=color_pal) +
    scale_x_continuous(breaks=c(2020,2060,2100), expand=c(0.01,0.01)) +
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=10,family='Helvetica',color='black'),
          strip.text       = element_text(size=10,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.box       = 'vertical',
          legend.text      = element_text(size=10,family='Helvetica',color='black'),
          plot.title       = element_text(size=10,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=10,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=10,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=10,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=10,family='Helvetica',color='black'))
  
  return(p1)
  
}

#_______________________________________________________________________________
# Make line plot with facet geo
plot_line_facet_geo <- function(plot_data, rect_data, gcam_grid, x_col, y_col, color_col, fill_col,
                                facet_var, leg_nrow, x_lab, y_lab, plot_title) {
  
  p1 <- ggplot(data=plot_data, aes_string(x=x_col, y=y_col, color=color_col)) +
  #  geom_rect(data=rect_data, aes_string(fill=fill_col), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.3, inherit.aes=FALSE) +
    geom_line() +
    facet_geo(as.formula(paste('~', facet_var)), grid = gcam_grid, label='name', scales='free_y') +
    labs(x     = x_lab,
         y     = y_lab,
         title = plot_title) + 
    guides(fill=guide_legend(title=NULL, nrow=leg_nrow), color=guide_legend(title=NULL)) +
    scale_color_manual(values=color_pal) +
    scale_x_continuous(breaks=c(2020,2060,2100), expand=c(0.01,0.01)) +
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=10,family='Helvetica',color='black'),
          strip.text       = element_text(size=10,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.box       = 'vertical',
          legend.text      = element_text(size=10,family='Helvetica',color='black'),
          plot.title       = element_text(size=10,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=10,family='Helvetica',color='black'),  
          axis.title.x     = element_text(size=10,family='Helvetica',color='black'),
          axis.text.y      = element_text(size=10,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=10,family='Helvetica',color='black'))
  
  return(p1)
  
}

#________________________________________________________
# Make timeseries and column plot with facet grid from data
plot_timeseries_facet_grid_select_scenario <- function(plot.data.table, select_scenario, y_lab_1, y_lab_2){
  
  # Estimate top 5 regions with largest percentage increase and decrease in 
  # agricultural production for the select_scenario
  # scenarios compared to the 'Reference' scenario by 2100 [MtN]
  region_per_change <- bind_rows((plot.data.table %>% 
                                    filter(scenario == select_scenario, year == 2100, region != 'Global') %>% 
                                    group_by(scenario) %>%
                                    # top_n(5, per_change_ref) %>% mutate(Desc='Regions with largest % increase in 2100 compared to Reference scenario')),
                                    top_n(5, change_ref) %>% mutate(Desc='Regions with largest increase in 2100 compared to Reference scenario')),
                                 (plot.data.table %>% 
                                    filter(scenario == select_scenario, year == 2100, region != 'Global') %>% 
                                    group_by(scenario) %>%
                                    # top_n(-5, per_change_ref) %>% mutate(Desc='Regions with largest % decrease in 2100 compared to Reference scenario')))
                                    top_n(-5, change_ref) %>% mutate(Desc='Regions with largest decrease in 2100 compared to Reference scenario')))
  
  plot.data <- filter(plot.data.table, 
                      scenario %in% c('Reference [SSP2-4p5]', select_scenario), 
                      region != 'Global')
  # Make timeseries plot with facet grid from data
  p1 <- plot_line_facet_geo(plot.data, 
                            region_per_change,
                            gcam_grid, 
                            x_col      = 'year', 
                            y_col      = 'value',
                            color_col  = 'scenario',
                            fill_col   = 'Desc',
                            facet_var  = 'region', 
                            leg_nrow   = 1, 
                            x_lab      = NULL, 
                            y_lab_1, 
                            plot_title = NULL)
  
  print(p1)
  
  # Make column plot with facets from data
  plot.data <- filter(plot.data.table, 
                      scenario %in% select_scenario,
                      year %in% c(2020, 2040, 2060, 2080, 2100))
  
  p2 <- plot_column_facet_geo(plot.data, 
                              region_per_change,
                              gcam_grid, 
                              x_col      = 'year', 
                              y_col      = 'change_ref',
                              color_col  = 'scenario', 
                              fill_col   = 'Desc',
                              facet_var  = 'region', 
                              leg_nrow   = 1, 
                              x_lab      = NULL, 
                              y_lab_2, 
                              plot_title = NULL) 
  
  print(p2)
  
}
