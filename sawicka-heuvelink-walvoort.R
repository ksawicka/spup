# -------------------------------------------------------------------
# Example 1 - uncertainty propagation analysis with auto- and
# cross- correlated variables

# load packages
library(spup)
library(raster)
library(purrr)

# set seed
set.seed(12345)

# load and view the data
data(OC, OC_sd, TN, TN_sd)
class(OC); class(TN)

par(mfrow = c(1, 2), mar = c(1, 1, 2, 2), mgp = c(1.7, 0.5, 0),
    oma = c(0,0,0,0), las = 1, cex.main = 1, tcl = -0.2,
    cex.axis = 0.8, cex.lab = 0.8)
plot(OC, main = "Mean of Organic Carbon", xaxt = 'n', yaxt = 'n') 
plot(TN, main = "Mean of Total Nitrogen", xaxt = 'n', yaxt = 'n')

# define spatial correlogram models
OC_crm <- makeCRM(acf0 = 0.6, range = 1000, model = "Sph")
TN_crm <- makeCRM(acf0 = 0.4, range = 1000, model = "Sph")

par(mfrow = c(1, 2), mar = c(3, 2.5, 2, 1), mgp = c(1.7, 0.5, 0),
    oma = c(0,0,0,0), las = 1, cex.main = 1, tcl = -0.2,
    cex.axis = 0.8, cex.lab = 0.8)
plot(OC_crm, main = "OC correlogram")
plot(TN_crm, main = "TN correlogram")	

# define uncertainty model for the OC and the TN
OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd),
                  crm = OC_crm, id = "OC")
TN_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(TN, TN_sd),
                  crm = TN_crm, id = "TN")

# define multivariate uncertainty model
mySpatialMUM <- defineMUM(UMlist = list(OC_UM, TN_UM), 
                          cormatrix = matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))

# create possible realizations from the joint distribution of OC and TN
MC <- 100
OCTN_sample <- genSample(UMobject = mySpatialMUM, n = MC,
                         samplemethod = "ugs", nmax = 20, asList = FALSE)

# compute and plot OC and TN sample statistics
# e.g. mean and standard deviation
OC_sample <- OCTN_sample[[1:MC]]
TN_sample <- OCTN_sample[[(MC+1):(2*MC)]]
OC_sample_mean <- mean(OC_sample)
TN_sample_mean <- mean(TN_sample)
OC_sample_sd <- calc(OC_sample, fun = sd)  
TN_sample_sd <- calc(TN_sample, fun = sd)

par(mfrow = c(1, 2), mar = c(1, 1, 2, 2), mgp = c(1.7, 0.5, 0),
    oma = c(0,0,0,1), las = 1, cex.main = 1,
    tcl = -0.2, cex.axis = 0.8, cex.lab = 0.8)
plot(OC_sample_mean, main = "Mean of OC realizations",
     xaxt = 'n', yaxt = 'n')
plot(TN_sample_mean, main = "Mean of TN realizations",
     xaxt = 'n', yaxt = 'n')

# C/N model
C_N_model_raster <- function (OC, TN) OC/TN

# coerce a raster stack to a list 
l <- list()
l[[1]] <- map(1:100, function(x){OCTN_sample[[x]]})
l[[2]] <- map(101:200, function(x){OCTN_sample[[x]]})
OCTN_sample <- l

# run uncertainty propagation
CN_sample <- propagate(realizations = OCTN_sample,
                       model = C_N_model_raster, n = MC)

# coerce C/Ns list to a raster stack
CN_sample <- stack(CN_sample)
names(CN_sample) <- paste("CN.", c(1:nlayers(CN_sample)), sep = "")

# compute and plot the slope sample statistics
CN_mean <- mean(CN_sample)
CN_sd <- calc(CN_sample, fun = sd)

par(mfrow = c(1, 2), mar = c(1, 1, 2, 2), mgp = c(1.7, 0.5, 0),
    oma = c(0,0,0,1), las = 1, cex.main = 1, tcl = -0.2,
    cex.axis = 0.8, cex.lab = 0.8)
plot(CN_mean, main = "Mean of C/N", xaxt = 'n', yaxt = 'n')
plot((CN_sd/CN_mean)*100, main = "Relative error of C/N [%]",
     xaxt = 'n', yaxt = 'n')

# calculate quantiles
CN_sample_df <- as(CN_sample, "SpatialGridDataFrame")
CN_q <- quantile_MC_sgdf(CN_sample_df, probs = c(0.01, 0.1, 0.5, 0.9),
                         na.rm = TRUE)

CN_q$good4crops99perc <- ifelse(CN_q$prob1perc > 20, 1, 0)
CN_q$good4crops90perc <- ifelse(CN_q$prob10perc > 20, 1, 0)
CN_q$good4crops50perc <- ifelse(CN_q$prob50perc > 20, 1, 0)
CN_q$good4crops10perc <- ifelse(CN_q$prob90perc > 20, 1, 0)
CN_q$good4crops <- CN_q$good4crops99perc + CN_q$good4crops90perc +
  CN_q$good4crops50perc + CN_q$good4crops10perc

CN_q$good4crops[CN_q$good4crops == 4] <- "No improvement needed"
CN_q$good4crops[CN_q$good4crops == 3] <- "Possibly improvement needed"
CN_q$good4crops[CN_q$good4crops == 2] <- "Likely improvement needed"
CN_q$good4crops[CN_q$good4crops == 1] <- "Definitely improvement needed"
CN_q$good4crops[CN_q$good4crops == 0] <- "Definitely improvement needed"
CN_q$good4crops <- factor(CN_q$good4crops, levels = c("Definitely improvement needed",
  "Likely improvement needed", "Possibly improvement needed", "No improvement needed"))

spplot(CN_q, "good4crops", col.regions = c("red3", "sandybrown", "greenyellow",
        "forestgreen"), main = "Areas with sufficient C/N for cropping")

# ---------------------------------------------------------------------------------
# Example 2 - uncertainty propagation analysis with categorical data

# load packages
library(sp)
library(spup)
library(purrr)
library(png)

set.seed(12345)

# load data 
data(woon) 
# Netherlands contour and Rotterdam location 
NL <- readPNG("./vignettes/RotterdamNL.png") 
# collate info about figure size and type 
NL_map <- list("grid.raster", NL, x = 89987, y = 436047, width = 120,
				height = 152, default.units = 'native') 
 # collate info about a scale bar location in the figure, type and colour 
scale <- list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(90000,435600),
			scale = 100, fill=c("transparent","black")) 
# collate info about minimum value on a scale bar 
text1 <- list("sp.text", c(89990,435600), "0", cex = 0.8) 
# collate info about maximum value on a scale bar 
text2 <- list("sp.text", c(90130,435600), "500 m", cex = 0.8) 
# collate info about North arrow 
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(),
				offset = c(89990,435630), scale = 50) 
# plot 'woon' object with a location miniature, 
# North arrow and scale bar defined above 				
spplot(woon, "check", do.log = T, col.regions = "transparent",
       colorkey = FALSE, key.space = list(x = 0.1, y = 0.93, corner = c(0,1)),
       sp.layout = list(scale, text1, text2 ,arrow, NL_map),	
       main = "Neighbourhood in Rotterdam, NL")

# plot probabilities for each polygon 
spplot(woon[c(4,5,6)])

# define uncertainty model for the building function
woonUM <- defineUM(TRUE, categories = c("residential","office","other"), cat_prob = 
                     woon[, c(4:6)])

# create possible realizations of the building function
woon_sample <- genSample(woonUM, 100, asList = FALSE)

# view several realizations
woon_s <- woon_sample@data
woon_s <- lapply(woon_sample@data, as.factor)
woon_sample@data <- as.data.frame(woon_s)
rm(woon_s)
spplot(woon_sample[c(3,4,1,2)], col.regions = c("#5ab4ac", "#f5f5f5", "#d8b365"),
       main = list(label = "Examples of building function realizations", cex = 1))


# define tax model
tax <- function (building_Function) 
{
  building_Function$tax2pay <- NA
  building_Function$tax2pay[building_Function$Function == "residential"] <- 1000
  building_Function$tax2pay[building_Function$Function == "office"] <- 10000
  building_Function$tax2pay[building_Function$Function == "other"] <- 10
  total_tax <- sum(building_Function$tax2pay)
  total_tax
}

# As in previous C/N ratio example, in order to run the propagation function
# the sample of an uncertain input variable must be saved in a list. 
# Either coerce the existing woon_sample object or get it automatically by
# setting up asList = TRUE in genSample().

# coerce  SpatialPolygonDataFrame to a list of individual SpatialPolygonDataFrames
woon_sample <- map(1:ncol(woon_sample), function(x){woon_sample[x]})
for (i in 1:100) names(woon_sample[[i]]) <- "Function"

# run uncertainty propagation
tax_sample <- propagate(woon_sample, model = tax, n = 100)

tax_sample <- unlist(tax_sample)
ci95perc <- c(mean(tax_sample) - 1.96*(sd(tax_sample)/10),
              mean(tax_sample) + 1.96*(sd(tax_sample)/10))
ci95perc

# ----------------------------------------------------------------------------------
# Example 3 - uncertainty propagation analysis with a model written in .c

library(spup)
library(dplyr) # a grammar of data manipulation
library(readr) # fast I/O
library(whisker) # rendering methods
library(purrr) # functional programming

set.seed(12345)

read_lines("input.txt")
read_lines("input.txt.template")

my_template <- template("input.txt.template")
my_template %>% 
  read_lines

my_template %>% 
  render(b0 = 3, b1 = 4)


# spup includes an example of a code for the external model written in the C language:
# that calculates a value of a simple linear regression. Save this code in the file with .c extension and compile to 
# a MS-Windows executable use a free C compiler GCC running command
# gcc dummy_model.c -o dummy_model. This will create a file dummy_model.exe.

# #include <stdio.h>
# 
# int main() {
#   
#   FILE *fp;
#   double x[9] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0};
#   double y;
#   double b0;
#   double b1;
#   int i;
#   
#   fp = fopen("input.txt", "r");
#   if (fp == NULL) {
#     printf("Can't read input.txt\n");
#     return 1;
#   }
#   fscanf(fp, "%lf %lf\n", &b0, &b1);
#   fclose(fp);
#   
#   fp = fopen("output.txt", "w");
#   if (fp == NULL) {
#     printf("Can't create output.txt\n");
#     return 1;
#   }
#   else {
#     for (i=0; i<9; i++) {
#       y = b0 + b1 * x[i];
#       fprintf(fp, "%10.2f\n", y);
#     }
#     fclose(fp);
#   }
#   
#   return 0;
# }

dummy_model <- executable("dummy_model.exe")

# render the template
render(my_template, b0 = 3.1, b1 = 4.2)

# run external model
dummy_model()

# read output (output file of dummy_model is "output.txt")
scan(file = "output.txt", quiet = TRUE)

# number of Monte Carlo runs
n_realizations <- 100

n_realizations %>%
  purrr::rerun({
    # render template
    render(my_template, b0 = rnorm(n = 1), b1 = runif(n = 1))
    
    # run model
    dummy_model()
    
    # read output
    scan("examples/output.txt", quiet = TRUE)
  }) %>%
  set_names(paste0("r", 1:n_realizations)) %>% 
  as_data_frame %>%
  apply(MARGIN = 1, FUN = quantile)    










