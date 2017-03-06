
# === Clear the console screen ===
cat("\014")

# === Clearing the Environment ===
# remove all variables / items stored in the environment previously
rm(list = ls(all = TRUE))

# reset graphics configurations
graphics.off()


# === Packages ===
# Step 1: List all the "packages" that you need 
# for your script inside c("package1", "package2", ...)
# Store the packages list in a vecrot libraries
# This is a character vector

# This is a list of packages that I needed for running one of my
# scripts in Statistical Programming Languages
libraries = c("stats", "graphics", "timeSeries", "reshape2", "ggplot2", 
              "psych", "xtable", "plyr","dplyr","data.table", "RODBC", "xlsx", "rJava", "xlsxjars")

# This command applies the following function to the list:
# install the packages mentioned in the vector "libraries" 
# except if they have already been installed
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})

# This command applies the following function to the list:
# Load all packages in this list
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# drop the character vector which contains the names of the packages 
# which you needed for your RScript to run

#Check margins and ADJUST MARGINS - if an error comes from code line
# Error in plot.new() : figure margins too large
par("mar")
par(mar=c(1,1,1,1))
par("mar")


