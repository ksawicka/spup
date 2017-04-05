tax <- function(building_Function) { 
  building_Function$tax2pay <- NA
  building_Function$tax2pay[building_Function$Function == 1] <- 1000
  building_Function$tax2pay[building_Function$Function == 2] <- 10000
  building_Function$tax2pay[building_Function$Function == 3] <- 10
  total_tax <- sum(building_Function$tax2pay)
  total_tax
}
