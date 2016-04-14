  library(raster)
  library(ggplot2)
  library(gridExtra)

  #' Visualise Spatial Uncertainty using Adjacent Maps
  #'
  #' Allows for visualising spatial uncertainty in both continuous and categorical
  #' data using Adjacent Maps. For continuous data it provides maps of mean and
  #' standard deviation of realisations. For categorical data, the function
  #' provides maps of the most likely class and the associated probability.
  #'
  #' @param simulations Object of class simulations
  #' @param title,xlab,ylab,legend Character vector giving plot title, x axis
  #'   label, y axis label and legend respectively. To be provided in format
  #'   c("plot1", "plot2).
  #' @param grid Logical. Add grid to background of graph
  #' @param axis Logical. Add axis ticks and labels to graph
  #' @param mean_col,std_col \code{\link[grDevices]{colorRampPalette}}. Colours to
  #'   be used for plotting the mean and standard deviation of continuous data
  #'   respectively.
  #'
  #' @return
  #' @export
  #'
  #' @examples


  plotAdjacent <- function(simulations, title = NULL, xlab = NULL, ylab = NULL, legend = NULL,
                           grid = T, axis = T, mean_col = NULL, std_col = NULL){

#     require(raster)
#     require(ggplot2)
#     require(gridExtra)

    # Check if simulations is of correct class
    if (is(simulations, "Simulations") != T) {
      stop("Expected object of class Simulations")
    }
    #Check if titles provided for both graphs
    if(length(title)!=2 & is.null(title)==F) {
      warning("Title should be provided for both graphs as character vector")
    }
    #Check if labels provided for both graphs
    if(length(xlab)!=2 & is.null(xlab)==F) {
      warning("X axis labels should be provided for both graphs as character vector")
    }
    if(length(ylab)!=2 & is.null(ylab)==F) {
      warning("Y axis labels should be provided for both graphs as character vector")
    }
    #Check if legends provided for both graphs
    if(length(legend)!=2 & is.null(legend)==F) {
      warning("Legend should be provided for both graphs as character vector")
    }

    # Set background grid and axis ticks/labels
    gridlines = element_blank()
    if (grid) {
      gridlines = element_line()
    }
    axis.tick = element_blank()
    axis.text = element_blank()
    if (axis){
      axis.tick = element_line()
      axis.text = element_text()
    }
    #Define theme
    theme <- theme(plot.title = element_text(size = 18, face = "bold"),
                   panel.grid = gridlines,
                   panel.border = element_rect(colour = "black", fill = "NA"),
                   axis.ticks = axis.tick,
                   axis.text.y = axis.text,
                   axis.text.x = axis.text,
                   legend.title = element_text(size = 12, face = "bold"),
                   legend.key = element_rect(fill='white'),
                   legend.text = element_text(size = 8))

    # Plot for continuous data
    if (hasValues(simulations@Mean)) {
      # Convert mean and standard deviation to data frames
      mean.df<- as.data.frame(simulations@Mean, xy = T)
      std.df<- as.data.frame(simulations@Standard.Deviation, xy = T)
      # Set default colour palette for mean and standard deviation
      if (is.null(mean_col)){
        mean_col <-colorRampPalette(c("#3f524c", "#5a7b5e", "#96ac87", "#cfc59f", "#fdedd8"))(20)
      }
      if (is.null(std_col)){
        std_col <- colorRampPalette(c("black", "white"))(25)
      }
      # Set default legend labels for continuous data
      if (is.null(legend)){
        legend <- c("Predicted value", "Standard Deviation")
      }
      # Create plot
      dem.mean <- ggplot(na.omit(mean.df), aes(x=x, y=y)) +
        geom_tile(aes(fill = layer)) +
        scale_fill_gradientn(colours=mean_col, name = legend[1]) +
        coord_equal(xlim=c(min(mean.df$x),max(mean.df$x)),ylim = c(min(mean.df$y),max(mean.df$y))) +
        theme +
        labs(title=title[1], x=xlab[1], y=ylab[1])
      dem.std <- ggplot(na.omit(std.df), aes(x=x, y=y)) +
        geom_tile(aes(fill = layer)) +
        scale_fill_gradientn(colours = std_col, name = legend[2]) +
        coord_equal(xlim=c(min(std.df$x),max(std.df$x)),ylim = c(min(std.df$y),max(std.df$y))) +
        theme +
        labs(title=title[2], x=xlab[2], y=ylab[2])
      grid.arrange(dem.mean, dem.std, ncol = 2)
    }

    # Plot for categorical data
    if (hasValues(simulations@Most.Likely.Class)) {
      # Convert most likelty class and class probabilities to data frames
      mlc.df<- as.data.frame(simulations@Most.Likely.Class, xy = T)
      clpb.df<- as.data.frame(simulations@Class.Probabilities, xy = T)
      # Set default colour palette for class probability and most likely class
      if (is.null(std_col)){
        std_col <- colorRampPalette(c("white", "black"))(25)
      }
      # Default legend labels for categorical data
      if (is.null(legend)){
        legend <- c("Most Likely Class", "Class Probability")
      }
      # Create plot
      mlc <- ggplot(na.omit(mlc.df), aes(x=x, y=y)) +
        geom_tile(aes(fill = factor(layer))) +
        scale_fill_discrete(name = legend[1]) +
        coord_equal(xlim=c(min(mlc.df$x),max(mlc.df$x)),ylim = c(min(mlc.df$y),max(mlc.df$y))) +
        theme +
        labs(title=title[1], x=xlab[1], y=ylab[1])
      clpb <- ggplot(na.omit(clpb.df), aes(x=x, y=y)) +
        geom_tile(aes(fill = mlc.prob)) +
        scale_fill_gradientn(colours = std_col, name = legend[2]) +
        coord_equal(xlim=c(min(clpb.df$x),max(clpb.df$x)),ylim = c(min(clpb.df$y),max(clpb.df$y))) +
        theme +
        labs(title=title[2], x=xlab[2], y=ylab[2])
      grid.arrange(mlc, clpb, ncol = 2)
    }
  }

