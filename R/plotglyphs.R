  library(raster)
  library(ggplot2)

#'   Visualise Spatial Uncertainty using Glyphs.
#'
#'   Allows for visualising spatial uncertainty for continuous data using glyphs.
#'
#'   @param simulations Object of class simulations containing continuous data
#'   @param density Numeric. Gives spacing of density of glyphs.
#'   @param title,xlab,ylab Character vector giving plot title, x axis label and y
#'     axis label respectively.
#'   @param grid Logical. Add grid to background of graph
#'   @param axis Logical. Add axis ticks and labels to graph
#'   @param mean_col \code{\link[grDevices]{colorRampPalette}}. Colours to
#'     be used for plotting the mean values of the realisations.
#'   @param glyph_col Character giving the colour of the glyphs representing the
#'     standard deviation of the realisations.
#'   @param legend Character giving the legend titles. To be provided in format
#'     c("legend1", "legend2").
#'
#'   @return
#'   @export
#'
#'   @examples
#'

  plotGlyphs <- function(simulations, density, title = NULL, xlab = NULL, ylab = NULL,
                         grid = T, axis = T, mean_col = NULL, glyph_col = NULL, legend = NULL) {

#       require(raster)
#       require(ggplot2)

    # Check if simulations is of correct class
    if (is(simulations, "Simulations") != T) {
      stop("simulations object expected to be class Simulations")
    }
    # Check if simulations is of continuous data
    if (hasValues(simulations@Mean) != T) {
      stop("simulations object expected to contain continuous data")
    }
    # Check if density is of correct class
    if (is(density, "numeric") != T) {
      stop("density object expected to be class Numeric")
    }
    #Check if legends provided for graph and glyphs
    if(length(legend)!=2 & is.null(legend)==F) {
      warning("Legend should be provided for both data and uncertainty glyphs")
    }
    # Convert mean to data frame
    mean.df<- as.data.frame(simulations@Mean, xy = T)
    # Aggregate standard deviation and create data frame
    lowres <- aggregate(simulations@Standard.Deviation, fact=density, expand = F)
    spatial.points <- as.data.frame(rasterToPoints(lowres, spatial = T))
    # Set background grid and axis ticks/labels
    gridlines = element_line()
    if (!grid) {
      gridlines = element_blank()
    }
    axis.tick = element_line()
    axis.text = element_text()
    if (!axis){
      axis.tick = element_blank()
      axis.text = element_blank()
    }
    # Create theme
    theme <- theme(plot.title = element_text(size = 18, face = "bold"),
                   panel.grid = gridlines,
                   panel.border = element_rect(colour = "black", fill = "NA"),
                   axis.ticks = axis.tick,
                   axis.text.y = axis.text,
                   axis.text.x = axis.text,
                   legend.title = element_text(size = 12, face = "bold"),
                   legend.key = element_rect(fill='white'),
                   legend.text = element_text(size = 8))

    # Set colour palette for mean and colour for glyph
    if (is.null(mean_col)){
      mean_col <- colorRampPalette(c("#3f524c", "#5a7b5e", "#96ac87", "#cfc59f", "#fdedd8"))(20)
    }
    if (is.null(glyph_col)){
      glyph_col <- "black"
    }
    # Set default legend labels
    if (is.null(legend)){
      legend <- c("Predicted value", "Standard Deviation")
    }
    # Create plot
    dem.mean <- ggplot(na.omit(mean.df), aes(x=x, y=y)) +
      geom_tile(aes(fill = layer)) +
      coord_equal(xlim=c(min(mean.df$x),max(mean.df$x)),ylim = c(min(mean.df$y),max(mean.df$y))) +
      scale_fill_gradientn(colours=mean_col, name = legend[1])
    dem.with.error <- dem.mean +
      geom_point(data = spatial.points, aes(x=x, y=y, size=layer), colour = glyph_col, shape = 1) +
      scale_size_area()+
      theme +
      labs(title=title, x = xlab, y = ylab, size = legend[2])
    return(dem.with.error)
  }
