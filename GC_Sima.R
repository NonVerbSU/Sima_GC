## -----------------------------------------------------------------------------
## Clear RStudio
rm(list = ls())  # Clear the global environment --> over there
graphics.off()  # Clear all plots
cat("\014")  # Clear the console - the window below this one
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(lmtest)
library(writexl)
library(ggplot2)

##------------------------------------------------------------------------------
# Set the path to your folder - this is in order to find your files 
## Set it to where your files are - start with 'Lie' Excel files 
setwd("E:/Research Assistant - SU/Deception/Subset_data_C-Uppsats/Lie")
folder_path <- "E:/Research Assistant - SU/Deception/Subset_data_C-Uppsats/Lie"

# Create a list of all of the Excel files within your 'Lie' folder
xlsx_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read the files into a named list, let's call it  'data_list* for now
data_list <- list()

for (file in xlsx_files) {
  # Extract the file name for each file - i.e. the participant code
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the Excel file using the 'readxl' package
  data <- readxl::read_excel(file)
  
  # Store the data frame in the list with its original file name
  data_list[[file_name]] <- data
}

## Okay now time to check the data - has the import routine worked?
# Check the structure of the first file in the list
str(data_list[[1]])

##------------------------------------------------------------------------------
##Okay, now it's time to subset your data so that you're only working with AU12_r

# Subset each data frame in the list and convert to data frame
data_list_subset <- lapply(data_list, function(data_frame) {
  # Subset the data frame to include only "timestamp" and "AU12_r"
  subset_data <- data_frame[c("timestamp", "AU12_r")]
  
  # Convert the subsetted data frame to a regular data frame
  subset_data <- as.data.frame(subset_data)
  
  return(subset_data)
})

# Now check the structure of the first new data frame in the list
str(data_list_subset[[1]])

# Okay, looks about right, so now print the column names of the first data frame
print(names(data_list_subset[[1]]))

# Let's check the 'head' of the first column in the first subsetted data frame
head(data_list_subset[[1]]$timestamp)

#Great, so it looks like the data frames have the structure (str) we want

##------------------------------------------------------------------------------
## Now we want to merge the data frames into dyads, with the AU12_r column
# present for each of the participants in the dyad...

# Firstly, we want to specify the dyads by name
lie_dyadic_pairs <- list(c("B254A_sub", "D542B_sub"), c("B623A_sub", "C641B_sub"), 
                         c("C642A_sub", "L324B_sub"), c("M524B_sub", "R516A_sub"), 
                         c("M500A_sub", "K216B_sub"), c("L253A_sub", "A165B_sub"),
                         c("A236A_sub", "A524B_sub"), c("D525A_sub", "K623B_sub"), 
                         c("F420A_sub", "I312B_sub"), c("C264A_sub", "C240B_sub"))

# Apologies, this code is a little long-winded, I finally managed to create a new
# function for extracting the AU_12_r columns from each data frame

extract_AU12_r_dyad <- function(df1, df2) {
  min_length <- min(nrow(df1), nrow(df2))
  df1 <- df1[1:min_length, ]
  df2 <- df2[1:min_length, ]
  
  AU12_r_dyad <- data.frame(
    AU12_r_df1 = df1$AU12_r,
    AU12_r_df2 = df2$AU12_r
  )
  
  return(AU12_r_dyad)
}

# Okay, now the long-winded bit. Run it for each pair to create new subset dyad
# data frames
AU12_r_dyad_1 <- extract_AU12_r_dyad(data_list_subset[["B254A_sub"]], data_list_subset[["D542B_sub"]])
AU12_r_dyad_2 <- extract_AU12_r_dyad(data_list_subset[["B623A_sub"]], data_list_subset[["C641B_sub"]])
AU12_r_dyad_3 <- extract_AU12_r_dyad(data_list_subset[["C642A_sub"]], data_list_subset[["L324B_sub"]])
AU12_r_dyad_4 <- extract_AU12_r_dyad(data_list_subset[["M524B_sub"]], data_list_subset[["R516A_sub"]])
AU12_r_dyad_5 <- extract_AU12_r_dyad(data_list_subset[["M500A_sub"]], data_list_subset[["K216B_sub"]])
AU12_r_dyad_6 <- extract_AU12_r_dyad(data_list_subset[["L253A_sub"]], data_list_subset[["A165B_sub"]])
AU12_r_dyad_7 <- extract_AU12_r_dyad(data_list_subset[["A236A_sub"]], data_list_subset[["A524B_sub"]])
AU12_r_dyad_8 <- extract_AU12_r_dyad(data_list_subset[["D525A_sub"]], data_list_subset[["K623B_sub"]])
AU12_r_dyad_9 <- extract_AU12_r_dyad(data_list_subset[["F420A_sub"]], data_list_subset[["I312B_sub"]])
AU12_r_dyad_10 <- extract_AU12_r_dyad(data_list_subset[["C264A_sub"]], data_list_subset[["C240B_sub"]])
##-----------------------------------------------------------------------------
# Okay, now for your first GC. 
# A reminder, this is for the 'Lie' data. 
# This formulation calculate whether person B (the interviewer) 
# granger causes person A (the interviewee/statement reader)

# Dyad 1
dyad_1 <- AU12_r_dyad_1
granger_test_1 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_1)
granger_test_1_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_1)
granger_test_1_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_1)

# Dyad 2
dyad_2 <- AU12_r_dyad_2
granger_test_2 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_2)
granger_test_2_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_2)
granger_test_2_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_2)

# Dyad 3
dyad_3 <- AU12_r_dyad_3
granger_test_3 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_3)
granger_test_3_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_3)
granger_test_3_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_3)

# Dyad 4
dyad_4 <- AU12_r_dyad_4
granger_test_4 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_4)
granger_test_4_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_4)
granger_test_4_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_4)

# Dyad 5
dyad_5 <- AU12_r_dyad_5
granger_test_5 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_5)
granger_test_5_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_5)
granger_test_5_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_5)

# Dyad 6
dyad_6 <- AU12_r_dyad_6
granger_test_6 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_6)
granger_test_6_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_6)
granger_test_6_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_6)

# Dyad 7
dyad_7 <- AU12_r_dyad_7
granger_test_7 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_7)
granger_test_7_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_7)
granger_test_7_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_7)

# Dyad 8
dyad_8 <- AU12_r_dyad_8
granger_test_8 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_8)
granger_test_8_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_8)
granger_test_8_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_8)

# Dyad 9
dyad_9 <- AU12_r_dyad_9
granger_test_9 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_9)
granger_test_9_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_9)
granger_test_9_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_9)

# Dyad 10
dyad_10 <- AU12_r_dyad_10
granger_test_10 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_10)
granger_test_10_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_10)
granger_test_10_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_10)

##------------------------------------------------------------------------------
# Now we're going to flip the code to just focus on whether Person A GC B
# i.e. Are liars more concerned with driving the nonverbal narrative to be
# more convincing?

# Dyad 1 - Person B Granger causes Person A
dyad_1_B <- AU12_r_dyad_1
granger_test_1_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_1_B)
granger_test_1_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_1_B)
granger_test_1_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_1_B)

# Dyad 2 - Person B Granger causes Person A
dyad_2_B <- AU12_r_dyad_2
granger_test_2_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_2_B)
granger_test_2_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_2_B)
granger_test_2_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_2_B)

# Dyad 3 - Person B Granger causes Person A
dyad_3_B <- AU12_r_dyad_3
granger_test_3_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_3_B)
granger_test_3_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_3_B)
granger_test_3_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_3_B)

# Dyad 4 - Person B Granger causes Person A
dyad_4_B <- AU12_r_dyad_4
granger_test_4_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_4_B)
granger_test_4_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_4_B)
granger_test_4_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_4_B)

# Dyad 5 - Person B Granger causes Person A
dyad_5_B <- AU12_r_dyad_5
granger_test_5_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_5_B)
granger_test_5_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_5_B)
granger_test_5_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_5_B)

# Dyad 6 - Person B Granger causes Person A
dyad_6_B <- AU12_r_dyad_6
granger_test_6_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_6_B)
granger_test_6_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_6_B)
granger_test_6_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_6_B)

# Dyad 7 - Person B Granger causes Person A
dyad_7_B <- AU12_r_dyad_7
granger_test_7_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_7_B)
granger_test_7_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_7_B)
granger_test_7_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_7_B)

# Dyad 8 - Person B Granger causes Person A
dyad_8_B <- AU12_r_dyad_8
granger_test_8_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_8_B)
granger_test_8_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_8_B)
granger_test_8_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_8_B)

# Dyad 9 - Person B Granger causes Person A
dyad_9_B <- AU12_r_dyad_9
granger_test_9_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_9_B)
granger_test_9_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_9_B)
granger_test_9_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_9_B)

# Dyad 10 - Person B Granger causes Person A
dyad_10_B <- AU12_r_dyad_10
granger_test_10_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_10_B)
granger_test_10_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_10_B)
granger_test_10_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_10_B)


## Okay, now we have GC results for all Lie dyads for both directions of GC.
# It's time to get these together and extract them so that you have the data available
# outside of an R environment...

# List to store results
granger_results_list <- list(
  B_GC_A = list(
    dyad_1 = list(result = granger_test_1, lag_6 = granger_test_1_6, lag_12 = granger_test_1_12),
    dyad_2 = list(result = granger_test_2, lag_6 = granger_test_2_6, lag_12 = granger_test_2_12),
    dyad_3 = list(result = granger_test_3, lag_6 = granger_test_3_6, lag_12 = granger_test_3_12),
    dyad_4 = list(result = granger_test_4, lag_6 = granger_test_4_6, lag_12 = granger_test_4_12),
    dyad_5 = list(result = granger_test_5, lag_6 = granger_test_5_6, lag_12 = granger_test_5_12),
    dyad_6 = list(result = granger_test_6, lag_6 = granger_test_6_6, lag_12 = granger_test_6_12),
    dyad_7 = list(result = granger_test_7, lag_6 = granger_test_7_6, lag_12 = granger_test_7_12),
    dyad_8 = list(result = granger_test_8, lag_6 = granger_test_8_6, lag_12 = granger_test_8_12),
    dyad_9 = list(result = granger_test_9, lag_6 = granger_test_9_6, lag_12 = granger_test_9_12),
    dyad_10 = list(result = granger_test_10, lag_6 = granger_test_10_6, lag_12 = granger_test_10_12)
  ),
  A_GC_B = list(
    dyad_1 = list(result = granger_test_1_B, lag_6 = granger_test_1_B_6, lag_12 = granger_test_1_B_12),
    dyad_2 = list(result = granger_test_2_B, lag_6 = granger_test_2_B_6, lag_12 = granger_test_2_B_12),
    dyad_3 = list(result = granger_test_3_B, lag_6 = granger_test_3_B_6, lag_12 = granger_test_3_B_12),
    dyad_4 = list(result = granger_test_4_B, lag_6 = granger_test_4_B_6, lag_12 = granger_test_4_B_12),
    dyad_5 = list(result = granger_test_5_B, lag_6 = granger_test_5_B_6, lag_12 = granger_test_5_B_12),
    dyad_6 = list(result = granger_test_6_B, lag_6 = granger_test_6_B_6, lag_12 = granger_test_6_B_12),
    dyad_7 = list(result = granger_test_7_B, lag_6 = granger_test_7_B_6, lag_12 = granger_test_7_B_12),
    dyad_8 = list(result = granger_test_8_B, lag_6 = granger_test_8_B_6, lag_12 = granger_test_8_B_12),
    dyad_9 = list(result = granger_test_9_B, lag_6 = granger_test_9_B_6, lag_12 = granger_test_9_B_12),
    dyad_10 = list(result = granger_test_10_B, lag_6 = granger_test_10_B_6, lag_12 = granger_test_10_B_12)
  )
)

# Flatten the nested list
flattened_list <- unlist(granger_results_list, recursive = FALSE)

# Convert the flattened list to a data frame
results_df <- do.call(rbind, lapply(flattened_list, as.data.frame))

# Write the nested list to an Excel file
write_xlsx(results_df, path = "granger_results_Lie_updated.xlsx")

##-----------------------------------------------------------------------------
## Maybe, just maybe, you want to avoid the charge of multiple comparisons and 
# perform Bonferroni corrections on your p-values. If so, try something like this...

# Columns containing p-values
p_value_columns <- c("result.Pr..F.", "lag_6.Pr..F.", "lag_12.Pr..F.")

# Number of comparisons
num_comparisons <- 3  # For each group (no lag, 6-frame lag, 12-frame lag)

# Apply Bonferroni correction and create new columns
for (col in p_value_columns) {
  bonferroni_col <- paste0(col, "_Bonferroni")
  results_df[[bonferroni_col]] <- pmin(results_df[[col]] * num_comparisons, 1.0)
}

# Write the data frame to an Excel file
write_xlsx(results_df, path = "granger_results_lie_updated.xlsx")

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## Okay, now let's do it all over again, this time with the 'Truth' data.

# Set the path to your folder - now for the 'Truth' subset Excel files 
setwd("E:/Research Assistant - SU/Deception/Subset_data_C-Uppsats/True")
folder_path <- "E:/Research Assistant - SU/Deception/Subset_data_C-Uppsats/True"

# Create a list of all of the Excel files within your 'Lie' folder
xlsx_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read the files into a named list, let's call it  'data_list* for now
data_list <- list()

for (file in xlsx_files) {
  # Extract the file name for each file - i.e. the participant code
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the Excel file using the 'readxl' package
  data <- readxl::read_excel(file)
  
  # Store the data frame in the list with its original file name
  data_list[[file_name]] <- data
}

## Okay now time to check the data - has the import routine worked?
# Check the structure of the first file in the list
str(data_list[[1]])

##------------------------------------------------------------------------------
##Okay, now it's time to subset your data so that you're only working with AU12_r

# Subset each data frame in the list and convert to data frame
data_list_subset <- lapply(data_list, function(data_frame) {
  # Subset the data frame to include only "timestamp" and "AU12_r"
  subset_data <- data_frame[c("timestamp", "AU12_r")]
  
  # Convert the subsetted data frame to a regular data frame
  subset_data <- as.data.frame(subset_data)
  
  return(subset_data)
})

# Now check the structure of the first new data frame in the list
str(data_list_subset[[1]])

# Okay, looks about right, so now print the column names of the first data frame
print(names(data_list_subset[[1]]))

# Let's check the 'head' of the first column in the first subsetted data frame
head(data_list_subset[[1]]$timestamp)

#Great, so it looks like the data frames have the structure (str) we want

##------------------------------------------------------------------------------
## Now we want to merge the data frames into dyads, with the AU12_r column
# present for each of the participants in the dyad...

# Firstly, we want to specify the dyads by name
truth_dyadic_pairs <- list(c("E426A_sub", "M236B_sub"), c("V245A_sub", "P524B_sub"), 
                         c("N561A_sub", "I256B_sub"), c("A526A_sub", "M526B_sub"), 
                         c("N123A_sub", "A453B_sub"), c("A473A_sub", "N652B_sub"),
                         c("S246A_sub", "A561B_sub"), c("S530A_sub", "L424B_sub"), 
                         c("S653A_sub", "T616B_sub"), c("E520A_sub", "L342B_sub"),
                         c("T536A_sub", "B216B_sub"))

# Apologies, this code is a little long-winded, I finally managed to create a new
# function for extracting the AU_12_r columns from each data frame

extract_AU12_r_dyad <- function(df1, df2) {
  min_length <- min(nrow(df1), nrow(df2))
  df1 <- df1[1:min_length, ]
  df2 <- df2[1:min_length, ]
  
  AU12_r_dyad <- data.frame(
    AU12_r_df1 = df1$AU12_r,
    AU12_r_df2 = df2$AU12_r
  )
  
  return(AU12_r_dyad)
}

# Okay, now the long-winded bit again. Run it for each pair to create new subset dyad
# data frames, this time for truth
# Extract and combine data frames for each dyad by name
AU12_r_dyad_1 <- extract_AU12_r_dyad(data_list_subset[["E426A_sub"]], data_list_subset[["M236B_sub"]])
AU12_r_dyad_2 <- extract_AU12_r_dyad(data_list_subset[["V245A_sub"]], data_list_subset[["P524B_sub"]])
AU12_r_dyad_3 <- extract_AU12_r_dyad(data_list_subset[["N561A_sub"]], data_list_subset[["I264B_sub"]])
AU12_r_dyad_4 <- extract_AU12_r_dyad(data_list_subset[["A526A_sub"]], data_list_subset[["M526B_sub"]])
AU12_r_dyad_5 <- extract_AU12_r_dyad(data_list_subset[["N123A_sub"]], data_list_subset[["A453B_sub"]])
AU12_r_dyad_6 <- extract_AU12_r_dyad(data_list_subset[["A473A_sub"]], data_list_subset[["N652B_sub"]])
AU12_r_dyad_7 <- extract_AU12_r_dyad(data_list_subset[["S246A_sub"]], data_list_subset[["A561B_sub"]])
AU12_r_dyad_8 <- extract_AU12_r_dyad(data_list_subset[["S530A_sub"]], data_list_subset[["L424B_sub"]])
AU12_r_dyad_9 <- extract_AU12_r_dyad(data_list_subset[["S653A_sub"]], data_list_subset[["T616B_sub"]])
AU12_r_dyad_10 <- extract_AU12_r_dyad(data_list_subset[["E520A_sub"]], data_list_subset[["L342B_sub"]])
AU12_r_dyad_11 <- extract_AU12_r_dyad(data_list_subset[["T536A_sub"]], data_list_subset[["B216B_sub"]])

##-----------------------------------------------------------------------------
# Okay, now for your first 'Truth' GC. 
# This formulation calculate whether person B (the interviewer) 
# granger causes (GC) person A (the interviewee/statement reader)

# Dyad 1
dyad_1 <- AU12_r_dyad_1
granger_test_1 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_1)
granger_test_1_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_1)
granger_test_1_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_1)

# Dyad 2
dyad_2 <- AU12_r_dyad_2
granger_test_2 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_2)
granger_test_2_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_2)
granger_test_2_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_2)

# Dyad 3
dyad_3 <- AU12_r_dyad_3
granger_test_3 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_3)
granger_test_3_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_3)
granger_test_3_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_3)

# Dyad 4
dyad_4 <- AU12_r_dyad_4
granger_test_4 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_4)
granger_test_4_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_4)
granger_test_4_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_4)

# Dyad 5
dyad_5 <- AU12_r_dyad_5
granger_test_5 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_5)
granger_test_5_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_5)
granger_test_5_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_5)

# Dyad 6
dyad_6 <- AU12_r_dyad_6
granger_test_6 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_6)
granger_test_6_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_6)
granger_test_6_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_6)

# Dyad 7
dyad_7 <- AU12_r_dyad_7
granger_test_7 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_7)
granger_test_7_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_7)
granger_test_7_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_7)

# Dyad 8
dyad_8 <- AU12_r_dyad_8
granger_test_8 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_8)
granger_test_8_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_8)
granger_test_8_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_8)

# Dyad 9
dyad_9 <- AU12_r_dyad_9
granger_test_9 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_9)
granger_test_9_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_9)
granger_test_9_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_9)

# Dyad 10
dyad_10 <- AU12_r_dyad_10
granger_test_10 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_10)
granger_test_10_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_10)
granger_test_10_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_10)

# Dyad 11
dyad_11 <- AU12_r_dyad_11
granger_test_11 <- lmtest::grangertest(AU12_r_df1 ~ AU12_r_df2, data = dyad_11)
granger_test_11_6 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 6), data = dyad_11)
granger_test_11_12 <- lmtest::grangertest(AU12_r_df1 ~ lag(AU12_r_df2, 12), data = dyad_11)

##------------------------------------------------------------------------------
# Now we're going to flip the code to just focus on whether Person A GC B
# i.e. Are liars more concerned with driving the nonverbal narrative to be
# more convincing?

# Dyad 1 - Person B Granger causes Person A
dyad_1_B <- AU12_r_dyad_1
granger_test_1_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_1_B)
granger_test_1_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_1_B)
granger_test_1_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_1_B)

# Dyad 2 - Person B Granger causes Person A
dyad_2_B <- AU12_r_dyad_2
granger_test_2_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_2_B)
granger_test_2_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_2_B)
granger_test_2_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_2_B)

# Dyad 3 - Person B Granger causes Person A
dyad_3_B <- AU12_r_dyad_3
granger_test_3_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_3_B)
granger_test_3_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_3_B)
granger_test_3_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_3_B)

# Dyad 4 - Person B Granger causes Person A
dyad_4_B <- AU12_r_dyad_4
granger_test_4_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_4_B)
granger_test_4_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_4_B)
granger_test_4_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_4_B)

# Dyad 5 - Person B Granger causes Person A
dyad_5_B <- AU12_r_dyad_5
granger_test_5_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_5_B)
granger_test_5_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_5_B)
granger_test_5_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_5_B)

# Dyad 6 - Person B Granger causes Person A
dyad_6_B <- AU12_r_dyad_6
granger_test_6_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_6_B)
granger_test_6_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_6_B)
granger_test_6_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_6_B)

# Dyad 7 - Person B Granger causes Person A
dyad_7_B <- AU12_r_dyad_7
granger_test_7_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_7_B)
granger_test_7_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_7_B)
granger_test_7_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_7_B)

# Dyad 8 - Person B Granger causes Person A
dyad_8_B <- AU12_r_dyad_8
granger_test_8_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_8_B)
granger_test_8_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_8_B)
granger_test_8_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_8_B)

# Dyad 9 - Person B Granger causes Person A
dyad_9_B <- AU12_r_dyad_9
granger_test_9_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_9_B)
granger_test_9_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_9_B)
granger_test_9_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_9_B)

# Dyad 10 - Person B Granger causes Person A
dyad_10_B <- AU12_r_dyad_10
granger_test_10_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_10_B)
granger_test_10_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_10_B)
granger_test_10_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_10_B)

# Dyad 11 - Person B Granger causes Person A
dyad_11_B <- AU12_r_dyad_11
granger_test_11_B <- lmtest::grangertest(AU12_r_df2 ~ AU12_r_df1, data = dyad_11_B)
granger_test_11_B_6 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 6), data = dyad_11_B)
granger_test_11_B_12 <- lmtest::grangertest(AU12_r_df2 ~ lag(AU12_r_df1, 12), data = dyad_11_B)

##------------------------------------------------------------------------------
## Okay, now we have GC results for all Lie dyads for both directions of GC.
# It's time to get these together and extract them so that you have the data available
# outside of an R environment...

# List to store results
granger_results_list <- list(
  B_GC_A = list(
    dyad_1 = list(result = granger_test_1, lag_6 = granger_test_1_6, lag_12 = granger_test_1_12),
    dyad_2 = list(result = granger_test_2, lag_6 = granger_test_2_6, lag_12 = granger_test_2_12),
    dyad_3 = list(result = granger_test_3, lag_6 = granger_test_3_6, lag_12 = granger_test_3_12),
    dyad_4 = list(result = granger_test_4, lag_6 = granger_test_4_6, lag_12 = granger_test_4_12),
    dyad_5 = list(result = granger_test_5, lag_6 = granger_test_5_6, lag_12 = granger_test_5_12),
    dyad_6 = list(result = granger_test_6, lag_6 = granger_test_6_6, lag_12 = granger_test_6_12),
    dyad_7 = list(result = granger_test_7, lag_6 = granger_test_7_6, lag_12 = granger_test_7_12),
    dyad_8 = list(result = granger_test_8, lag_6 = granger_test_8_6, lag_12 = granger_test_8_12),
    dyad_9 = list(result = granger_test_9, lag_6 = granger_test_9_6, lag_12 = granger_test_9_12),
    dyad_10 = list(result = granger_test_10, lag_6 = granger_test_10_6, lag_12 = granger_test_10_12),
    dyad_11 = list(result = granger_test_11, lag_6 = granger_test_11_6, lag_12 = granger_test_11_12)
  ),
  A_GC_B = list(
    dyad_1 = list(result = granger_test_1_B, lag_6 = granger_test_1_B_6, lag_12 = granger_test_1_B_12),
    dyad_2 = list(result = granger_test_2_B, lag_6 = granger_test_2_B_6, lag_12 = granger_test_2_B_12),
    dyad_3 = list(result = granger_test_3_B, lag_6 = granger_test_3_B_6, lag_12 = granger_test_3_B_12),
    dyad_4 = list(result = granger_test_4_B, lag_6 = granger_test_4_B_6, lag_12 = granger_test_4_B_12),
    dyad_5 = list(result = granger_test_5_B, lag_6 = granger_test_5_B_6, lag_12 = granger_test_5_B_12),
    dyad_6 = list(result = granger_test_6_B, lag_6 = granger_test_6_B_6, lag_12 = granger_test_6_B_12),
    dyad_7 = list(result = granger_test_7_B, lag_6 = granger_test_7_B_6, lag_12 = granger_test_7_B_12),
    dyad_8 = list(result = granger_test_8_B, lag_6 = granger_test_8_B_6, lag_12 = granger_test_8_B_12),
    dyad_9 = list(result = granger_test_9_B, lag_6 = granger_test_9_B_6, lag_12 = granger_test_9_B_12),
    dyad_10 = list(result = granger_test_10_B, lag_6 = granger_test_10_B_6, lag_12 = granger_test_10_B_12),
    dyad_11 = list(result = granger_test_11_B, lag_6 = granger_test_11_B_6, lag_12 = granger_test_11_B_12)
  )
)


# Flatten the nested list
flattened_list <- unlist(granger_results_list, recursive = FALSE)

# Convert the flattened list to a data frame
results_df_t <- do.call(rbind, lapply(flattened_list, as.data.frame))

# Write the nested list to an Excel file
write_xlsx(results_df_t, path = "granger_results_truth_updated.xlsx")

##-----------------------------------------------------------------------------
## Maybe, just maybe, you want to avoid the charge of multiple comparisons and 
# perform Bonferroni corrections on your p-values. If so, try something like this...

# Columns containing p-values
p_value_columns <- c("result.Pr..F.", "lag_6.Pr..F.", "lag_12.Pr..F.")

# Number of comparisons
num_comparisons <- 3  # For each group (no lag, 6-frame lag, 12-frame lag)

# Apply Bonferroni correction and create new columns
for (col in p_value_columns) {
  bonferroni_col <- paste0(col, "_Bonferroni")
  results_df_t[[bonferroni_col]] <- pmin(results_df_t[[col]] * num_comparisons, 1.0)
}

# Write the data frame to an Excel file
write_xlsx(results_df_t, path = "granger_results_truth_updated.xlsx")

##------------------------------------------------------------------------------
# Plots, let's try a few things...
