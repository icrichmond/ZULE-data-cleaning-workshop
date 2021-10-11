# This is all the code chunks used throughout the workshop, feel free to copy and paste them from here if it makes it easier to follow along 
# Author: Isabella Richmond 

#### Loading packages -----------------------------------------
p <- c("tidyverse", "data.table", "anytime")
lapply(p, library, character.only = T)


#### 1. Import data and investigate ---------------------------
# NOTE: this code is functioning on the assumption that you have a project set-up as per the slides provided

# initial import using tidyverse 
tdf <- read_csv("input/example-data.csv")
# initial import using data.table 
dtdf <- fread("input/example-data.csv")

# when we investigate we see that both imports have some problems
# read_csv doesn't recognize that the file is delimited using ";" instead of ",". Let's use read_delim to fix that and add it to the data.table argument 
tdf <- read_delim("input/example-data.csv", delim = ";")
dtdf <- fread("input/example-data.csv", sep = ";")

# that's slightly better, but we still have the problem that the first 7 lines in tidyverse and first 8 lines in data.table need to be removed for this to be functional 
tdf <- read_delim("input/example-data.csv", delim = ";", skip = 8)
dtdf <- fread("input/example-data.csv", sep = ";", skip = 8)

# adding the skip argument fixes our problems! Now we can move on to other formatting issues... 




#### 2. Special Characters & UTF-8 encoding ---------------------
# What would life in Québec be without special characters! When we investigate our tidyverse dataframe, we see an issue with "Montréal" 
# This issue often arises with accents and other special characters, when the dataframe is not properly read as UTF-8
# Luckily, we have an easy fix for this! 
# tidyverse can be re-encoded after import 
tdf$City <- enc2utf8(as(tdf$City, "character"))
# data.table incorporates it into the import line 
dtdf <- fread("input/example-data.csv", sep = ";", skip = 8, encoding = "UTF-8") 




#### 3. NAs & associated values ---------------------------------
# there are often NAs in our datasets, which we may or may not want to keep 
# often, there are also other values that count, biologically, as NAs but are not read into R automatically as NAs 
# for example, blanks, "NA" (instead of a true NA), zeroes (for some variables), etc.
# NOTE: data.table automatically imports most blanks, "NA", etc. as NA values. You cannot specify 0 as NA when importing using fread
# NAs can be addressed when importing data, or after 

# Strategy A: When importing (for tidyverse only)
tdf <- read_delim("input/example-data.csv", delim = ";", skip = 8, na = c(""," ", "NA", 0, "0"))

# Strategy B: After importing (for tidyverse and data.table)
tdf$DBH <- na_if(tdf$DBH, 0) # tidyverse
dtdf$DBH[dtdf$DBH == 0] <- NA # data.table

# Drop the NAs (we don't want them, but you might in your analysis)
tdf <- drop_na(tdf) # tidyverse
dtdf <- na.omit(dtdf) # data.table




#### 4. Duplicates ----------------------------------------------
# sometimes, there are duplicate observations/rows in datasets. These can really mess up your analysis if you don't catch them
# testing your dataset for duplicates and then removing what you find is an important step in data cleaning! 
# investigate which lines are duplicates
tdup <- tdf[duplicated(tdf), c("TreeID", "City", "DBH", "Species,")]
dtdup <- dtdf[duplicated(dtdf), c("TreeID", "City", "DBH", "Species,")]
# tidyverse remove duplicates
tdf <- distinct(tdf)
# data.table remove duplicates 
dtdf <- unique(dtdf)




#### 5. Column Names --------------------------------------------
# bad column names can make your life a living hell. Before moving into analysis make sure your column names follow best practices 
# you'll thank me later! You don't want numbers, special characters, or spaces in your column names. Let's change our really bad column names 
# tidyverse 
tdf <- rename(tdf, 
              Date = "1st Date Measured",
              Time = "Time  Measured", 
              Species = "Species,")
# data.table 
dtdf <- setnames(dtdf,                              
                 c("1st Date Measured", "Time  Measured", "Species,"),
                 c("Date", "Time", "Species"))




#### 6. Value Formatting ---------------------------------------
## Spelling mistakes 
# in our dataset, Toronto is spelled wrong. If you have a huge dataset with many spelling errors, OpenRefine is a great tool to use
# for us, our dataset is small and we can visually inspect it for spelling errors so we will replace the error in R 
# tidyverse 
tdf$City <- recode(tdf$City, Toronno = "Toronto")
# data.table
dtdf$City[dtdf$City %in% c("Toronno", "TO", "The birthplace of Drake")] <-"Toronto"

## Formatting column types 
# city and species are currently formatted as character types, but for our study we want them to act as factors - we need to reclassify them 
# its always important to check the classes of your data, R makes assumptions when importing your data and can be wrong! 
# tidyverse
tdf$City <- as_factor(tdf$City)
tdf$Species <- as_factor(tdf$Species)
# data.table
factor_cols <- c("City", "Species")
dtdf[ ,(factor_cols) := lapply(.SD, as.factor),
      .SDcols = factor_cols]

## Text parsing
# we have a comma at the end of our species names, that we don't really want 
# we can use text parsing to remove those unwanted commas from the Species column
# tidyverse 
tdf$Species <- str_remove(tdf$Species, "[,]") # this will remove a comma anywhere in the column
# data.table
dtdf$Species <- gsub(",","",dtdf$Species)




#### 7. Date/Time Formatting -----------------------------------
# date and time formatting in R can be really tricky and frustrating sometimes but is often really necessary for field data (and other data)
# anytime is a cool package that makes the process a little bit easier 
# first we want a date-time column, not separate entities 
# tidyverse 
tdf <- unite(tdf, "DateTime", Date:Time, sep = " ")
# data.table 
dtdf[,DateTime:=paste0(Date," ",Time)]
dtdf[, c("Date","Time"):=NULL]

# now use anytime to format the columns
# NOTE: Be careful of time zones, anytime will automatically set to where you are 
# if you are using data from other time zones and need to indicate that, use the parsedate package
tdf$DateTime <- anytime(tdf$DateTime)
dtdf$DateTime <- anytime(dtdf$DateTime)




#### 8. Reshaping ----------------------------------------------
# often the way datasets are initially set up are not ideal for things like plotting and modelling
# so we need to reshape the dataframe - make it longer or wider - so we can do what we need to do
# let's make the dataframe wider - we only want one entry per city 
# tidyverse
tdf_wide <- pivot_wider(tdf, names_from = Species, values_from = DBH)

# Hmmmmm that is not the most useful format - let's make it longer again (or melt it)
tdf_long <- pivot_longer(tdf_wide, cols = 4:14, names_to = "Species", values_to = "DBH", values_drop_na = T)

# dcast and melt for data.table
dtdf_wide <- dcast(dtdf, formula = City + TreeID + DateTime ~ Species, value.var = "DBH")
dtdf_long <- melt(dtdf_wide, id.vars = c("TreeID","City", "DateTime"), variable.name = "Species", value.name = "DBH", na.rm = T) 




#### 9. Saving/Exporting Data ----------------------------------
# don't forget to save your beautiful, cleaned data!
# maybe you have a "cleaned" folder in your input directory, save it there
# if this is a final product, save it to output 
# if this is an intermediate item, the best way to save it is as an .rds file 
# if this is a final product or something you will be sharing, save it as a .csv
# tidyverse 
saveRDS(tdf, "output/TidyData.rds")
write_csv(tdf, "output/TidyData.csv")
# data.table
saveRDS(dtdf, "output/DataTableData.rds")
fwrite(dtdf, "output/DataTableData.csv")




#### BONUS: Piping ---------------------------------------------
# you may be wondering why tidyverse has so many seemingly redundant functions with baseR
# the tidyverse allows you to do something called piping (%>%), which can make your code much more efficient 
# for example, if we took all the tidyverse code from above and piped it together, it would look like this: 
tdf <- read_delim("input/example-data.csv", delim = ";", skip = 8)
tdf$City <- enc2utf8(as(tdf$City, "character"))

tdf <- tdf %>%
  na_if(0) %>%
  drop_na() %>%
  distinct() %>%
  rename(Date = "1st Date Measured",
         Time = "Time  Measured", 
         Species = "Species,") %>%
  mutate(City = recode(City, Toronno = "Toronto")) %>% 
  mutate(City = as_factor(City)) %>%
  mutate(Species = as_factor(Species)) %>%
  mutate(Species = str_remove(Species, "[,]")) %>%
  unite("DateTime", Date:Time, sep = " ") %>%
  mutate(DateTime = anytime(DateTime)) %>% 
  pivot_wider(names_from = Species, values_from = DBH) %>%
  pivot_longer(cols = 4:14, names_to = "Species", values_to = "DBH", values_drop_na = T) %>%
  write_csv("output/TidyData.csv")
