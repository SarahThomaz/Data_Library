
########################################################
# Setup
########################################################
# For First-time Users
# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("scales")

library(scales)
library(tidycensus)
library(tidyverse)
library(readxl)

install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device = "win")

# Upload all relevant documents
BGOverlap <- read_excel("~/ArcGIS/Projects/Dashboard/BGOverlapPercentages_TableToExcel.xlsx")
BGOverlap$PERCENTAGE <- BGOverlap$PERCENTAGE/100 # Convert percentage to decimal format
BGOverlap$PERCENTAGE <- format(round(BGOverlap$PERCENTAGE, 2), nsmall = 2) # Set values to 2 decimal places
BGOverlap$PERCENTAGE <- as.double(BGOverlap$PERCENTAGE) # Convert Percentage to double

ProblemChild <- read_excel("~/ArcGIS/Projects/Dashboard/DistrictToProblemChild.xls")
ProblemChild$PERCENTAGE <- ProblemChild$PERCENTAGE/100 # Convert percentage to decimal format
ProblemChild$PERCENTAGE <- format(round(ProblemChild$PERCENTAGE, 2), nsmall = 2) # Set values to 2 decimal places
ProblemChild$PERCENTAGE <- as.double(ProblemChild$PERCENTAGE) # Convert Percentage to double

BG_Dist <- read_excel("~/ArcGIS/Projects/Dashboard/BG_To_Dist.xlsx")
########################################################

########################################################
# City Data Configuration
########################################################

#~~~~~~~~~~~~~~~~~~~~~~~~
# City-Level Data
#~~~~~~~~~~~~~~~~~~~~~~~~

City_Download <- get_acs(year = 2013,
                   geography = "block group", # Get data at Block-Group level (as opposed to tract or place)
                   output = "wide",# report data without duplicating id observations
                   state = "08",   # Set state as colorado
                   county = "069", # data from Larimer County
                   variables = c(Total_Pop = "B01003_001",        # Population
                                 Total_Families = "B11001_002",   # Number of Family Households
                                 Pop_25_and_Over = "B15003_001",  # Population 25 and Over
                                 Pop_under_18 = "B09018_001",     # Population under 18 (children & minorities)
                                 Pop_16_and_Over = "B23025_001",  # Population 16 and over
                                 Children_own = "B09002_001",
                                 Pop_Male = "B01001_002",
                                 Pop_Female = "B01001_026",
                                 
                                 Median_Age = "B01002_001",       # Median Age
                                 Hispanic_Latino ="B03002_012",   # Hispanic or Latino Origin
                                 White_Latino ="B03002_013",      # Hispanic/Latino - White
                                 White_Alone ="B02001_002",       # White Alone 
                                 Black_Alone ="B02001_003",       # Black Alone
                                 AI_or_AN_Alone ="B02001_004",    # American Indian or Alaskan Native
                                 Asian_Alone ="B02001_005",       # Asian Alone
                                 NH_or_PI_Alone ="B02001_006",    # Native Hawaiian or Pacific Islander
                                 Other_race_alone = "B02001_007", # 
                                 TwoPlus = "B02001_008",
                                 
                                 ForeignBorn = "B99051_005",         # Number of individuals born outside the US
                                 Veterans = "B21001_002",            # Number of veterans
                                 
                                 No_Health_Insuranceu19 = "B27010_017",  # Number of individuals under 19 with no insurance
                                 No_Health_Insurance1935 = "B27010_033", # Number of individuals ages 19-35 with no insurance
                                 No_Health_Insurance3665 = "B27010_050", # Number of individuals ages 35-65 with no insurance
                                 No_Health_Insurance65p = "B27010_066",  # Number of individuals over 65 with no insurance
                                 Single_Dad = "B09002_009",              # Number of single father households 
                                 Single_Mom = "B09002_015",              # Number of single mother households
                                 SNAP_Disability = "B22010_006",     # The two disability vars must -  
                                 noSnap_Disability = "B22010_003",   # be added together
                                 SNAP_noDisability = "B22010_007",  # used to calculate the total number of households
                                 
                                 ### Economic Data ###
                                 Poverty = "B17017_002",                 # Number of individuals with income under the poverty line 
                                 Median_Income = "B19013_001",           # Median Household income
                                 Median_Earnings = "B20002_001", # Median Earnins
                                 public_assistance = "B19057_002",       # Number of households receiving public assistance
                                 SNAP_Users = "B19058_002",              # Number of households receiving SNAP benefits
                                 notSNAP_User = "B19058_003",            # Number of households not receiving SNAP benefits
                                 Employed = "B23025_004", #Number of employed individuals
                                 Unemployed = "B23025_005",              # Number of unemployed individuals
                                 LaborForce = "B23025_002",              # Number of individuals 16+ in the labor force
                                 NotInLaborForce = "B23025_007",         # Number of individuals 16+ not in the labor force
                                 Armed_Forces = "B23025_006", # Number of individuals employed by the armed forces
                                 
                                 ### Housing Data ###
                                 Total_Households = "B11001_001",      # Number of Households
                                 Total_Housing_Units = "B25001_001", # Number of Housing Units
                                 mobile_home = "B25024_010",           # Number of Mobile Homes
                                 total_vacant = "B25002_003",          # Number of vacant units
                                 total_occupied = "B25002_002",
                                 owner_occ = "B25003_002",             # Number of Owner-Occupied units
                                 renter_occ = "B25003_003",            # Number of Renter-Occupied units
                                 avg_hh_size = "B25010_001", # Average HH size
                                 #geographic_mobility_past12months = "",
                                 med_home_value = "B25077_001",       # Median (self-reported) house value of owner-occupied housing
                                 med_owner_costs = "B25088_002",  # Median Homeowner Selected Costs
                                 med_owner_costs_pctOfIncome = "B25092_002", # Median Homeowner Selected Costs as Percentage of Income
                                 #med_contract_rent = "", # Median Contract Rent
                                 med_gross_rent = "B25064_001",              # Median Gross Rent
                                 med_rent_asPctofInc = "B25071_001", # Median gross rent as a percent of income
                                 #substandard_housing = "", # Housing considered substandard *Tract only
                                 no_vehicles_r = "B25045_012",         # Number of renter households with no vehicles available
                                 no_vehicles_o = "B25045_003",
                                 
                                 ### Education Data ###
                                 Undergrad_enrollment = "B14007_017",# College student (undergrad)
                                 Grad_enrollment = "B14007_018",     # College student (Grad)
                                 
                                 High_School_Only = "B15003_017", # Number of individuals over 25 with 
                                 ged_Only = "B15003_018", # Number of individuals over 25 with 
                                 Bachelors_Degree = "B15003_022", # Number of individuals over 25 with 
                                 Masters_Degree = "B15003_023", # Number of individuals over 25 with 
                                 Professional_Degree = "B15003_024", # Number of individuals over 25 with 
                                 Doctorate_Degree = "B15003_025", # Number of individuals over 25 with 
                                 
                                 # English speaking ability is currently under education, but it can also be placed in Demographic info.
                                 # Here all the different fields for poor English are downloaded by language
                                 # All these variables will be added by language to give, for example, number of Spanish speakers who speak English less than well
                                 # Those variables are then summed to give the number of individuals who speak English less than well. 
                                 #Pop_over_5 = "B16005_001", #Population 5 years and over (for percentage calculations)
                                 # Spanish
                                 Spanish17noa = "B16004_008",   # Under 17 -speak English "not at all"
                                 Spanish1864noa = "B16004_030", # 18 - 64 - speak English "not at all"
                                 Spanish65noa = "B16004_052",   # 65+ - speak English  "not at all"
                                 Spanish17nw = "B16004_007",    # Under 17 -speak English "not well"
                                 Spanish1864nw = "B16004_029",  # 18 - 64 - speak English "not well"
                                 Spanish65nw = "B16004_051",    # 65+ - speak English "not well"
                                 # IndoEuropean Language
                                 Indoeuro17noa = "B16004_013",     # Under 17 -speak English "not at all"
                                 Indoeuro1864noa = "B16004_035",   # 18 - 64 - speak English "not at all"
                                 Indoeuro65noa = "B16004_057",# 65+ - speak English  "not at all"
                                 Indoeuro17nw = "B16004_012",      # Under 17 -speak English "not well"
                                 Indoeuro1864nw = "B16004_034",    # 18 - 64 - speak English "not well"
                                 Indoeuro65nw = "B16004_056",      # 65+ - speak English "not well"
                                 # Asian or Pacific Island Language
                                 API17noa = "B16004_018",       # Under 17 -speak English "not at all"
                                 API1864noa = "B16004_040",     # 18 - 64 - speak English "not at all"
                                 API65noa = "B16004_062",       # 65+ - speak English  "not at all"
                                 API17nw = "B16004_017",        # Under 17 -speak English "not well"
                                 API1864nw = "B16004_039",      # 18 - 64 - speak English "not well"
                                 API65nw = "B16004_061",        # 65+ - speak English "not well"
                                 # Some Other Language
                                 Other17noa = "B16004_023",     # Under 17 -speak English "not at all"
                                 Other1864noa = "B16004_045",   # 18 - 64 - speak English "not at all"
                                 Other65noa = "B16004_067",     #65+ - speak English  "not at all"
                                 Other17nw = "B16004_022",      # Under 17 -speak English "not well"
                                 Other1864nw = "B16004_044",    # 18 - 64 - speak English "not well"
                                 Other65nw = "B16004_066"      # 65+ - speak English "not well"
                   )) 
City_Download$year <-2013

for (i in 2014:2021){ 
  temp <- get_acs(year = i,
                  geography = "block group", # Get data at Block-Group level (as opposed to tract or place)
                  output = "wide",# report data without duplicating id observations
                  state = "08",   # Set state as colorado
                  county = "069", # data from Larimer County
                  variables = c(Total_Pop = "B01003_001",        # Population
                                Total_Families = "B11001_002",   # Number of Family Households
                                Pop_25_and_Over = "B15003_001",  # Population 25 and Over
                                Pop_under_18 = "B09018_001",     # Population under 18 (children & minorities)
                                Pop_16_and_Over = "B23025_001",  # Population 16 and over
                                Children_own = "B09002_001",
                                Pop_Male = "B01001_002",
                                Pop_Female = "B01001_026",
                                
                                Median_Age = "B01002_001",       # Median Age
                                Hispanic_Latino ="B03002_012",   # Hispanic or Latino Origin
                                White_Latino ="B03002_013",      # Hispanic/Latino - White
                                White_Alone ="B02001_002",       # White Alone 
                                Black_Alone ="B02001_003",       # Black Alone
                                AI_or_AN_Alone ="B02001_004",    # American Indian or Alaskan Native
                                Asian_Alone ="B02001_005",       # Asian Alone
                                NH_or_PI_Alone ="B02001_006",    # Native Hawaiian or Pacific Islander
                                Other_race_alone = "B02001_007", # 
                                TwoPlus = "B02001_008",
                                
                                ForeignBorn = "B99051_005",         # Number of individuals born outside the US
                                Veterans = "B21001_002",            # Number of veterans
                                
                                No_Health_Insuranceu19 = "B27010_017",  # Number of individuals under 19 with no insurance
                                No_Health_Insurance1935 = "B27010_033", # Number of individuals ages 19-35 with no insurance
                                No_Health_Insurance3665 = "B27010_050", # Number of individuals ages 35-65 with no insurance
                                No_Health_Insurance65p = "B27010_066",  # Number of individuals over 65 with no insurance
                                Single_Dad = "B09002_009",              # Number of single father households 
                                Single_Mom = "B09002_015",              # Number of single mother households
                                SNAP_Disability = "B22010_006",     # The two disability vars must -  
                                noSnap_Disability = "B22010_003",   # be added together
                                SNAP_noDisability = "B22010_007",  # used to calculate the total number of households
                                
                                ######################
                                ### Economic Data ####
                                ######################
                                Poverty = "B17017_002",                 # Number of individuals with income under the poverty line 
                                Median_Income = "B19013_001",           # Median Household income
                                Median_Earnings = "B20002_001", # Median Earnins
                                public_assistance = "B19057_002",       # Number of households receiving public assistance
                                SNAP_Users = "B19058_002",              # Number of households receiving SNAP benefits
                                notSNAP_User = "B19058_003",            # Number of households not receiving SNAP benefits
                                Employed = "B23025_004", #Number of employed individuals
                                Unemployed = "B23025_005",              # Number of unemployed individuals
                                LaborForce = "B23025_002",              # Number of individuals 16+ in the labor force
                                NotInLaborForce = "B23025_007",         # Number of individuals 16+ not in the labor force
                                Armed_Forces = "B23025_006", # Number of individuals employed by the armed forces
                                
                                
                                
                                ######################
                                #### Housing Data ####
                                ######################
                                Total_Households = "B11001_001",      # Number of Households
                                Total_Housing_Units = "B25001_001", # Number of Housing Units
                                mobile_home = "B25024_010",           # Number of Mobile Homes
                                total_vacant = "B25002_003",          # Number of vacant units
                                total_occupied = "B25002_002",
                                owner_occ = "B25003_002",             # Number of Owner-Occupied units
                                renter_occ = "B25003_003",            # Number of Renter-Occupied units
                                avg_hh_size = "B25010_001", # Average HH size
                                #geographic_mobility_past12months = "",
                                med_home_value = "B25077_001",       # Median (self-reported) house value of owner-occupied housing
                                med_owner_costs = "B25088_002",  # Median Homeowner Selected Costs
                                med_owner_costs_pctOfIncome = "B25092_002", # Median Homeowner Selected Costs as Percentage of Income
                                #med_contract_rent = "", # Median Contract Rent
                                med_gross_rent = "B25064_001",              # Median Gross Rent
                                med_rent_asPctofInc = "B25071_001", # Median gross rent as a percent of income
                                #substandard_housing = "", # Housing considered substandard *Tract only
                                no_vehicles_r = "B25045_012",         # Number of renter households with no vehicles available
                                no_vehicles_o = "B25045_003",
                                
                                
                                ######################
                                ### Education Data ###
                                ######################
                                Undergrad_enrollment = "B14007_017",# College student (undergrad)
                                Grad_enrollment = "B14007_018",     # College student (Grad)
                                
                                High_School_Only = "B15003_017", # Number of individuals over 25 with 
                                ged_Only = "B15003_018", # Number of individuals over 25 with 
                                Bachelors_Degree = "B15003_022", # Number of individuals over 25 with 
                                Masters_Degree = "B15003_023", # Number of individuals over 25 with 
                                Professional_Degree = "B15003_024", # Number of individuals over 25 with 
                                Doctorate_Degree = "B15003_025", # Number of individuals over 25 with 
                                
                                # English speaking ability is currently under education, but it can also be placed in Demographic info.
                                # Here all the different fields for poor English are downloaded by language
                                # All these variables will be added by language to give, for example, number of Spanish speakers who speak English less than well
                                # Those variables are then summed to give the number of individuals who speak English less than well. 
                                #Pop_over_5 = "B16005_001", #Population 5 years and over (for percentage calculations)
                                # Spanish
                                Spanish17noa = "B16004_008",   # Under 17 -speak English "not at all"
                                Spanish1864noa = "B16004_030", # 18 - 64 - speak English "not at all"
                                Spanish65noa = "B16004_052",   # 65+ - speak English  "not at all"
                                Spanish17nw = "B16004_007",    # Under 17 -speak English "not well"
                                Spanish1864nw = "B16004_029",  # 18 - 64 - speak English "not well"
                                Spanish65nw = "B16004_051",    # 65+ - speak English "not well"
                                # IndoEuropean Language
                                Indoeuro17noa = "B16004_013",     # Under 17 -speak English "not at all"
                                Indoeuro1864noa = "B16004_035",   # 18 - 64 - speak English "not at all"
                                Indoeuro65noa = "B16004_057",# 65+ - speak English  "not at all"
                                Indoeuro17nw = "B16004_012",      # Under 17 -speak English "not well"
                                Indoeuro1864nw = "B16004_034",    # 18 - 64 - speak English "not well"
                                Indoeuro65nw = "B16004_056",      # 65+ - speak English "not well"
                                # Asian or Pacific Island Language
                                API17noa = "B16004_018",       # Under 17 -speak English "not at all"
                                API1864noa = "B16004_040",     # 18 - 64 - speak English "not at all"
                                API65noa = "B16004_062",       # 65+ - speak English  "not at all"
                                API17nw = "B16004_017",        # Under 17 -speak English "not well"
                                API1864nw = "B16004_039",      # 18 - 64 - speak English "not well"
                                API65nw = "B16004_061",        # 65+ - speak English "not well"
                                # Some Other Language
                                Other17noa = "B16004_023",     # Under 17 -speak English "not at all"
                                Other1864noa = "B16004_045",   # 18 - 64 - speak English "not at all"
                                Other65noa = "B16004_067",     #65+ - speak English  "not at all"
                                Other17nw = "B16004_022",      # Under 17 -speak English "not well"
                                Other1864nw = "B16004_044",    # 18 - 64 - speak English "not well"
                                Other65nw = "B16004_066"))      # 65+ - speak English "not well"
  temp$year <- i 
  City_Download<-rbind(City_Download,temp)    
  
                      
} 



# Drop all the Margin of Error fields, remove the E from Estimate Fields, remove the NAME column
City_Download <- City_Download %>% select(-ends_with("M")) 
names(City_Download) <- sub('E$', '', names(City_Download))
City_Download <- subset(City_Download, select = -c(NAM))




# Calculate the fields as a percentage of the block group that the city overlaps
temporary <- sapply(City_Download[,-c(1,48,49,50,51,52,53,89)], '*', BGOverlap$PERCENTAGE) # Apply the percentages to the population values in the dataset
FC_Stats <- cbind(temporary, City_Download[,c(1,48,49,50,51,52,53,89)])
FC_Stats <- merge(FC_Stats, BG_Dist, "GEOID")
FC_Stats <- FC_Stats[FC_Stats$DIST != 0,]


# Check the types (string vs numeric) of each field
# nums <- unlist(lapply(BGOverlap, is.numeric), use.names = FALSE)  
# view(nums)
# rm(nums)
 
# Drop unnecessary stuff
rm(BGOverlap, temp, temporary, BG_Dist, i)

#End Dataset Configuration
########################################################

########################################################
# County & State Data Configuration
########################################################

#~~~~~~~~~~~~~~~~~~~~~~~~
# County-Level Data
#~~~~~~~~~~~~~~~~~~~~~~~~

County_Download <- get_acs(year = 2013,
                           geography = "county", # Get data at Block-Group level (as opposed to tract or place)
                           output = "wide",# report data without duplicating id observations
                           state = "08",   # Set state as colorado
                           county = "069", # data from Larimer County
                           variables = c(Total_Pop = "B01003_001",        # Population
                                         Total_Families = "B11001_002",   # Number of Family Households
                                         Pop_25_and_Over = "B15003_001",  # Population 25 and Over
                                         Pop_under_18 = "B09018_001",     # Population under 18 (children & minorities)
                                         Pop_16_and_Over = "B23025_001",  # Population 16 and over
                                         Children_own = "B09002_001",
                                         Pop_Male = "B01001_002",
                                         Pop_Female = "B01001_026",
                                         
                                         Median_Age = "B01002_001",       # Median Age
                                         Hispanic_Latino ="B03002_012",   # Hispanic or Latino Origin
                                         White_Latino ="B03002_013",      # Hispanic/Latino - White
                                         White_Alone ="B02001_002",       # White Alone 
                                         Black_Alone ="B02001_003",       # Black Alone
                                         AI_or_AN_Alone ="B02001_004",    # American Indian or Alaskan Native
                                         Asian_Alone ="B02001_005",       # Asian Alone
                                         NH_or_PI_Alone ="B02001_006",    # Native Hawaiian or Pacific Islander
                                         Other_race_alone = "B02001_007", # 
                                         TwoPlus = "B02001_008",
                                         
                                         ForeignBorn = "B99051_005",         # Number of individuals born outside the US
                                         Veterans = "B21001_002",            # Number of veterans
                                         
                                         No_Health_Insuranceu19 = "B27010_017",  # Number of individuals under 19 with no insurance
                                         No_Health_Insurance1935 = "B27010_033", # Number of individuals ages 19-35 with no insurance
                                         No_Health_Insurance3665 = "B27010_050", # Number of individuals ages 35-65 with no insurance
                                         No_Health_Insurance65p = "B27010_066",  # Number of individuals over 65 with no insurance
                                         Single_Dad = "B09002_009",              # Number of single father households 
                                         Single_Mom = "B09002_015",              # Number of single mother households
                                         SNAP_Disability = "B22010_006",     # The two disability vars must -  
                                         noSnap_Disability = "B22010_003",   # be added together
                                         SNAP_noDisability = "B22010_007",  # used to calculate the total number of households
                                         
                                         ### Economic Data ###
                                         Poverty = "B17017_002",                 # Number of individuals with income under the poverty line 
                                         Median_Income = "B19013_001",           # Median Household income
                                         Median_Earnings = "B20002_001", # Median Earnins
                                         public_assistance = "B19057_002",       # Number of households receiving public assistance
                                         SNAP_Users = "B19058_002",              # Number of households receiving SNAP benefits
                                         notSNAP_User = "B19058_003",            # Number of households not receiving SNAP benefits
                                         Employed = "B23025_004", #Number of employed individuals
                                         Unemployed = "B23025_005",              # Number of unemployed individuals
                                         LaborForce = "B23025_002",              # Number of individuals 16+ in the labor force
                                         NotInLaborForce = "B23025_007",         # Number of individuals 16+ not in the labor force
                                         Armed_Forces = "B23025_006", # Number of individuals employed by the armed forces
                                         
                                         ### Housing Data ###
                                         Total_Households = "B11001_001",      # Number of Households
                                         Total_Housing_Units = "B25001_001", # Number of Housing Units
                                         mobile_home = "B25024_010",           # Number of Mobile Homes
                                         total_vacant = "B25002_003",          # Number of vacant units
                                         total_occupied = "B25002_002",
                                         owner_occ = "B25003_002",             # Number of Owner-Occupied units
                                         renter_occ = "B25003_003",            # Number of Renter-Occupied units
                                         avg_hh_size = "B25010_001", # Average HH size
                                         #geographic_mobility_past12months = "",
                                         med_home_value = "B25077_001",       # Median (self-reported) house value of owner-occupied housing
                                         med_owner_costs = "B25088_002",  # Median Homeowner Selected Costs
                                         med_owner_costs_pctOfIncome = "B25092_002", # Median Homeowner Selected Costs as Percentage of Income
                                         #med_contract_rent = "", # Median Contract Rent
                                         med_gross_rent = "B25064_001",              # Median Gross Rent
                                         med_rent_asPctofInc = "B25071_001", # Median gross rent as a percent of income
                                         #substandard_housing = "", # Housing considered substandard *Tract only
                                         no_vehicles_r = "B25045_012",         # Number of renter households with no vehicles available
                                         no_vehicles_o = "B25045_003",
                                         
                                         ### Education Data ###
                                         Undergrad_enrollment = "B14007_017",# College student (undergrad)
                                         Grad_enrollment = "B14007_018",     # College student (Grad)
                                         
                                         High_School_Only = "B15003_017", # Number of individuals over 25 with 
                                         ged_Only = "B15003_018", # Number of individuals over 25 with 
                                         Bachelors_Degree = "B15003_022", # Number of individuals over 25 with 
                                         Masters_Degree = "B15003_023", # Number of individuals over 25 with 
                                         Professional_Degree = "B15003_024", # Number of individuals over 25 with 
                                         Doctorate_Degree = "B15003_025", # Number of individuals over 25 with 
                                         
                                         # English speaking ability is currently under education, but it can also be placed in Demographic info.
                                         # Here all the different fields for poor English are downloaded by language
                                         # All these variables will be added by language to give, for example, number of Spanish speakers who speak English less than well
                                         # Those variables are then summed to give the number of individuals who speak English less than well. 
                                         #Pop_over_5 = "B16005_001", #Population 5 years and over (for percentage calculations)
                                         # Spanish
                                         Spanish17noa = "B16004_008",   # Under 17 -speak English "not at all"
                                         Spanish1864noa = "B16004_030", # 18 - 64 - speak English "not at all"
                                         Spanish65noa = "B16004_052",   # 65+ - speak English  "not at all"
                                         Spanish17nw = "B16004_007",    # Under 17 -speak English "not well"
                                         Spanish1864nw = "B16004_029",  # 18 - 64 - speak English "not well"
                                         Spanish65nw = "B16004_051",    # 65+ - speak English "not well"
                                         # IndoEuropean Language
                                         Indoeuro17noa = "B16004_013",     # Under 17 -speak English "not at all"
                                         Indoeuro1864noa = "B16004_035",   # 18 - 64 - speak English "not at all"
                                         Indoeuro65noa = "B16004_057",# 65+ - speak English  "not at all"
                                         Indoeuro17nw = "B16004_012",      # Under 17 -speak English "not well"
                                         Indoeuro1864nw = "B16004_034",    # 18 - 64 - speak English "not well"
                                         Indoeuro65nw = "B16004_056",      # 65+ - speak English "not well"
                                         # Asian or Pacific Island Language
                                         API17noa = "B16004_018",       # Under 17 -speak English "not at all"
                                         API1864noa = "B16004_040",     # 18 - 64 - speak English "not at all"
                                         API65noa = "B16004_062",       # 65+ - speak English  "not at all"
                                         API17nw = "B16004_017",        # Under 17 -speak English "not well"
                                         API1864nw = "B16004_039",      # 18 - 64 - speak English "not well"
                                         API65nw = "B16004_061",        # 65+ - speak English "not well"
                                         # Some Other Language
                                         Other17noa = "B16004_023",     # Under 17 -speak English "not at all"
                                         Other1864noa = "B16004_045",   # 18 - 64 - speak English "not at all"
                                         Other65noa = "B16004_067",     #65+ - speak English  "not at all"
                                         Other17nw = "B16004_022",      # Under 17 -speak English "not well"
                                         Other1864nw = "B16004_044",    # 18 - 64 - speak English "not well"
                                         Other65nw = "B16004_066"      # 65+ - speak English "not well"
                           )) 
County_Download$year <-2013

for (i in 2014:2021){ 
  temp <- get_acs(year = i,
                  geography = "county", # Get data at Block-Group level (as opposed to tract or place)
                  output = "wide",# report data without duplicating id observations
                  state = "08",   # Set state as colorado
                  county = "069", # data from Larimer County
                  variables = c(Total_Pop = "B01003_001",        # Population
                                Total_Families = "B11001_002",   # Number of Family Households
                                Pop_25_and_Over = "B15003_001",  # Population 25 and Over
                                Pop_under_18 = "B09018_001",     # Population under 18 (children & minorities)
                                Pop_16_and_Over = "B23025_001",  # Population 16 and over
                                Children_own = "B09002_001",
                                Pop_Male = "B01001_002",
                                Pop_Female = "B01001_026",
                                
                                Median_Age = "B01002_001",       # Median Age
                                Hispanic_Latino ="B03002_012",   # Hispanic or Latino Origin
                                White_Latino ="B03002_013",      # Hispanic/Latino - White
                                White_Alone ="B02001_002",       # White Alone 
                                Black_Alone ="B02001_003",       # Black Alone
                                AI_or_AN_Alone ="B02001_004",    # American Indian or Alaskan Native
                                Asian_Alone ="B02001_005",       # Asian Alone
                                NH_or_PI_Alone ="B02001_006",    # Native Hawaiian or Pacific Islander
                                Other_race_alone = "B02001_007", # 
                                TwoPlus = "B02001_008",
                                
                                ForeignBorn = "B99051_005",         # Number of individuals born outside the US
                                Veterans = "B21001_002",            # Number of veterans
                                
                                No_Health_Insuranceu19 = "B27010_017",  # Number of individuals under 19 with no insurance
                                No_Health_Insurance1935 = "B27010_033", # Number of individuals ages 19-35 with no insurance
                                No_Health_Insurance3665 = "B27010_050", # Number of individuals ages 35-65 with no insurance
                                No_Health_Insurance65p = "B27010_066",  # Number of individuals over 65 with no insurance
                                Single_Dad = "B09002_009",              # Number of single father households 
                                Single_Mom = "B09002_015",              # Number of single mother households
                                SNAP_Disability = "B22010_006",     # The two disability vars must -  
                                noSnap_Disability = "B22010_003",   # be added together
                                SNAP_noDisability = "B22010_007",  # used to calculate the total number of households
                                
                                ######################
                                ### Economic Data ####
                                ######################
                                Poverty = "B17017_002",                 # Number of individuals with income under the poverty line 
                                Median_Income = "B19013_001",           # Median Household income
                                Median_Earnings = "B20002_001", # Median Earnins
                                public_assistance = "B19057_002",       # Number of households receiving public assistance
                                SNAP_Users = "B19058_002",              # Number of households receiving SNAP benefits
                                notSNAP_User = "B19058_003",            # Number of households not receiving SNAP benefits
                                Employed = "B23025_004", #Number of employed individuals
                                Unemployed = "B23025_005",              # Number of unemployed individuals
                                LaborForce = "B23025_002",              # Number of individuals 16+ in the labor force
                                NotInLaborForce = "B23025_007",         # Number of individuals 16+ not in the labor force
                                Armed_Forces = "B23025_006", # Number of individuals employed by the armed forces
                                
                                
                                
                                ######################
                                #### Housing Data ####
                                ######################
                                Total_Households = "B11001_001",      # Number of Households
                                Total_Housing_Units = "B25001_001", # Number of Housing Units
                                mobile_home = "B25024_010",           # Number of Mobile Homes
                                total_vacant = "B25002_003",          # Number of vacant units
                                total_occupied = "B25002_002",
                                owner_occ = "B25003_002",             # Number of Owner-Occupied units
                                renter_occ = "B25003_003",            # Number of Renter-Occupied units
                                avg_hh_size = "B25010_001", # Average HH size
                                #geographic_mobility_past12months = "",
                                med_home_value = "B25077_001",       # Median (self-reported) house value of owner-occupied housing
                                med_owner_costs = "B25088_002",  # Median Homeowner Selected Costs
                                med_owner_costs_pctOfIncome = "B25092_002", # Median Homeowner Selected Costs as Percentage of Income
                                #med_contract_rent = "", # Median Contract Rent
                                med_gross_rent = "B25064_001",              # Median Gross Rent
                                med_rent_asPctofInc = "B25071_001", # Median gross rent as a percent of income
                                #substandard_housing = "", # Housing considered substandard *Tract only
                                no_vehicles_r = "B25045_012",         # Number of renter households with no vehicles available
                                no_vehicles_o = "B25045_003",
                                
                                
                                ######################
                                ### Education Data ###
                                ######################
                                Undergrad_enrollment = "B14007_017",# College student (undergrad)
                                Grad_enrollment = "B14007_018",     # College student (Grad)
                                
                                High_School_Only = "B15003_017", # Number of individuals over 25 with 
                                ged_Only = "B15003_018", # Number of individuals over 25 with 
                                Bachelors_Degree = "B15003_022", # Number of individuals over 25 with 
                                Masters_Degree = "B15003_023", # Number of individuals over 25 with 
                                Professional_Degree = "B15003_024", # Number of individuals over 25 with 
                                Doctorate_Degree = "B15003_025", # Number of individuals over 25 with 
                                
                                # English speaking ability is currently under education, but it can also be placed in Demographic info.
                                # Here all the different fields for poor English are downloaded by language
                                # All these variables will be added by language to give, for example, number of Spanish speakers who speak English less than well
                                # Those variables are then summed to give the number of individuals who speak English less than well. 
                                #Pop_over_5 = "B16005_001", #Population 5 years and over (for percentage calculations)
                                # Spanish
                                Spanish17noa = "B16004_008",   # Under 17 -speak English "not at all"
                                Spanish1864noa = "B16004_030", # 18 - 64 - speak English "not at all"
                                Spanish65noa = "B16004_052",   # 65+ - speak English  "not at all"
                                Spanish17nw = "B16004_007",    # Under 17 -speak English "not well"
                                Spanish1864nw = "B16004_029",  # 18 - 64 - speak English "not well"
                                Spanish65nw = "B16004_051",    # 65+ - speak English "not well"
                                # IndoEuropean Language
                                Indoeuro17noa = "B16004_013",     # Under 17 -speak English "not at all"
                                Indoeuro1864noa = "B16004_035",   # 18 - 64 - speak English "not at all"
                                Indoeuro65noa = "B16004_057",# 65+ - speak English  "not at all"
                                Indoeuro17nw = "B16004_012",      # Under 17 -speak English "not well"
                                Indoeuro1864nw = "B16004_034",    # 18 - 64 - speak English "not well"
                                Indoeuro65nw = "B16004_056",      # 65+ - speak English "not well"
                                # Asian or Pacific Island Language
                                API17noa = "B16004_018",       # Under 17 -speak English "not at all"
                                API1864noa = "B16004_040",     # 18 - 64 - speak English "not at all"
                                API65noa = "B16004_062",       # 65+ - speak English  "not at all"
                                API17nw = "B16004_017",        # Under 17 -speak English "not well"
                                API1864nw = "B16004_039",      # 18 - 64 - speak English "not well"
                                API65nw = "B16004_061",        # 65+ - speak English "not well"
                                # Some Other Language
                                Other17noa = "B16004_023",     # Under 17 -speak English "not at all"
                                Other1864noa = "B16004_045",   # 18 - 64 - speak English "not at all"
                                Other65noa = "B16004_067",     #65+ - speak English  "not at all"
                                Other17nw = "B16004_022",      # Under 17 -speak English "not well"
                                Other1864nw = "B16004_044",    # 18 - 64 - speak English "not well"
                                Other65nw = "B16004_066"))      # 65+ - speak English "not well"
  temp$year <- i 
  County_Download<-rbind(County_Download,temp)    
  
  
} 



# Drop all the Margin of Error fields, remove the E from Estimate Fields, remove the NAME column
County_Download <- County_Download %>% select(-ends_with("M")) 
names(County_Download) <- sub('E$', '', names(County_Download))
County_Download <- subset(County_Download, select = -c(NAM))



# Drop unnecessary stuff
rm(temp, i)




#~~~~~~~~~~~~~~
# State 
#~~~~~~~~~~~~~~

State_Download <- get_acs(year = 2013,
                          geography = "state", # Get data at Block-Group level (as opposed to tract or place)
                          output = "wide",# report data without duplicating id observations
                          state = "08",   # Set state as colorado
                          variables = c(Total_Pop = "B01003_001",        # Population
                                        Total_Families = "B11001_002",   # Number of Family Households
                                        Pop_25_and_Over = "B15003_001",  # Population 25 and Over
                                        Pop_under_18 = "B09018_001",     # Population under 18 (children & minorities)
                                        Pop_16_and_Over = "B23025_001",  # Population 16 and over
                                        Children_own = "B09002_001",
                                        Pop_Male = "B01001_002",
                                        Pop_Female = "B01001_026",
                                        
                                        Median_Age = "B01002_001",       # Median Age
                                        Hispanic_Latino ="B03002_012",   # Hispanic or Latino Origin
                                        White_Latino ="B03002_013",      # Hispanic/Latino - White
                                        White_Alone ="B02001_002",       # White Alone 
                                        Black_Alone ="B02001_003",       # Black Alone
                                        AI_or_AN_Alone ="B02001_004",    # American Indian or Alaskan Native
                                        Asian_Alone ="B02001_005",       # Asian Alone
                                        NH_or_PI_Alone ="B02001_006",    # Native Hawaiian or Pacific Islander
                                        Other_race_alone = "B02001_007", # 
                                        TwoPlus = "B02001_008",
                                        
                                        ForeignBorn = "B99051_005",         # Number of individuals born outside the US
                                        Veterans = "B21001_002",            # Number of veterans
                                        
                                        No_Health_Insuranceu19 = "B27010_017",  # Number of individuals under 19 with no insurance
                                        No_Health_Insurance1935 = "B27010_033", # Number of individuals ages 19-35 with no insurance
                                        No_Health_Insurance3665 = "B27010_050", # Number of individuals ages 35-65 with no insurance
                                        No_Health_Insurance65p = "B27010_066",  # Number of individuals over 65 with no insurance
                                        Single_Dad = "B09002_009",              # Number of single father households 
                                        Single_Mom = "B09002_015",              # Number of single mother households
                                        SNAP_Disability = "B22010_006",     # The two disability vars must -  
                                        noSnap_Disability = "B22010_003",   # be added together
                                        SNAP_noDisability = "B22010_007",  # used to calculate the total number of households
                                        
                                        ### Economic Data ###
                                        Poverty = "B17017_002",                 # Number of individuals with income under the poverty line 
                                        Median_Income = "B19013_001",           # Median Household income
                                        Median_Earnings = "B20002_001", # Median Earnins
                                        public_assistance = "B19057_002",       # Number of households receiving public assistance
                                        SNAP_Users = "B19058_002",              # Number of households receiving SNAP benefits
                                        notSNAP_User = "B19058_003",            # Number of households not receiving SNAP benefits
                                        Employed = "B23025_004", #Number of employed individuals
                                        Unemployed = "B23025_005",              # Number of unemployed individuals
                                        LaborForce = "B23025_002",              # Number of individuals 16+ in the labor force
                                        NotInLaborForce = "B23025_007",         # Number of individuals 16+ not in the labor force
                                        Armed_Forces = "B23025_006", # Number of individuals employed by the armed forces
                                        
                                        ### Housing Data ###
                                        Total_Households = "B11001_001",      # Number of Households
                                        Total_Housing_Units = "B25001_001", # Number of Housing Units
                                        mobile_home = "B25024_010",           # Number of Mobile Homes
                                        total_vacant = "B25002_003",          # Number of vacant units
                                        total_occupied = "B25002_002",
                                        owner_occ = "B25003_002",             # Number of Owner-Occupied units
                                        renter_occ = "B25003_003",            # Number of Renter-Occupied units
                                        avg_hh_size = "B25010_001", # Average HH size
                                        #geographic_mobility_past12months = "",
                                        med_home_value = "B25077_001",       # Median (self-reported) house value of owner-occupied housing
                                        med_owner_costs = "B25088_002",  # Median Homeowner Selected Costs
                                        med_owner_costs_pctOfIncome = "B25092_002", # Median Homeowner Selected Costs as Percentage of Income
                                        #med_contract_rent = "", # Median Contract Rent
                                        med_gross_rent = "B25064_001",              # Median Gross Rent
                                        med_rent_asPctofInc = "B25071_001", # Median gross rent as a percent of income
                                        #substandard_housing = "", # Housing considered substandard *Tract only
                                        no_vehicles_r = "B25045_012",         # Number of renter households with no vehicles available
                                        no_vehicles_o = "B25045_003",
                                        
                                        ### Education Data ###
                                        Undergrad_enrollment = "B14007_017",# College student (undergrad)
                                        Grad_enrollment = "B14007_018",     # College student (Grad)
                                        
                                        High_School_Only = "B15003_017", # Number of individuals over 25 with 
                                        ged_Only = "B15003_018", # Number of individuals over 25 with 
                                        Bachelors_Degree = "B15003_022", # Number of individuals over 25 with 
                                        Masters_Degree = "B15003_023", # Number of individuals over 25 with 
                                        Professional_Degree = "B15003_024", # Number of individuals over 25 with 
                                        Doctorate_Degree = "B15003_025", # Number of individuals over 25 with 
                                        
                                        # English speaking ability is currently under education, but it can also be placed in Demographic info.
                                        # Here all the different fields for poor English are downloaded by language
                                        # All these variables will be added by language to give, for example, number of Spanish speakers who speak English less than well
                                        # Those variables are then summed to give the number of individuals who speak English less than well. 
                                        #Pop_over_5 = "B16005_001", #Population 5 years and over (for percentage calculations)
                                        # Spanish
                                        Spanish17noa = "B16004_008",   # Under 17 -speak English "not at all"
                                        Spanish1864noa = "B16004_030", # 18 - 64 - speak English "not at all"
                                        Spanish65noa = "B16004_052",   # 65+ - speak English  "not at all"
                                        Spanish17nw = "B16004_007",    # Under 17 -speak English "not well"
                                        Spanish1864nw = "B16004_029",  # 18 - 64 - speak English "not well"
                                        Spanish65nw = "B16004_051",    # 65+ - speak English "not well"
                                        # IndoEuropean Language
                                        Indoeuro17noa = "B16004_013",     # Under 17 -speak English "not at all"
                                        Indoeuro1864noa = "B16004_035",   # 18 - 64 - speak English "not at all"
                                        Indoeuro65noa = "B16004_057",# 65+ - speak English  "not at all"
                                        Indoeuro17nw = "B16004_012",      # Under 17 -speak English "not well"
                                        Indoeuro1864nw = "B16004_034",    # 18 - 64 - speak English "not well"
                                        Indoeuro65nw = "B16004_056",      # 65+ - speak English "not well"
                                        # Asian or Pacific Island Language
                                        API17noa = "B16004_018",       # Under 17 -speak English "not at all"
                                        API1864noa = "B16004_040",     # 18 - 64 - speak English "not at all"
                                        API65noa = "B16004_062",       # 65+ - speak English  "not at all"
                                        API17nw = "B16004_017",        # Under 17 -speak English "not well"
                                        API1864nw = "B16004_039",      # 18 - 64 - speak English "not well"
                                        API65nw = "B16004_061",        # 65+ - speak English "not well"
                                        # Some Other Language
                                        Other17noa = "B16004_023",     # Under 17 -speak English "not at all"
                                        Other1864noa = "B16004_045",   # 18 - 64 - speak English "not at all"
                                        Other65noa = "B16004_067",     #65+ - speak English  "not at all"
                                        Other17nw = "B16004_022",      # Under 17 -speak English "not well"
                                        Other1864nw = "B16004_044",    # 18 - 64 - speak English "not well"
                                        Other65nw = "B16004_066"      # 65+ - speak English "not well"
                          )) 
State_Download$year <-2013

for (i in 2014:2021){ 
  temp <- get_acs(year = i,
                  geography = "state", # Get data at Block-Group level (as opposed to tract or place)
                  output = "wide",# report data without duplicating id observations
                  state = "08",   # Set state as colorado
                  variables = c(Total_Pop = "B01003_001",        # Population
                                Total_Families = "B11001_002",   # Number of Family Households
                                Pop_25_and_Over = "B15003_001",  # Population 25 and Over
                                Pop_under_18 = "B09018_001",     # Population under 18 (children & minorities)
                                Pop_16_and_Over = "B23025_001",  # Population 16 and over
                                Children_own = "B09002_001",
                                Pop_Male = "B01001_002",
                                Pop_Female = "B01001_026",
                                
                                Median_Age = "B01002_001",       # Median Age
                                Hispanic_Latino ="B03002_012",   # Hispanic or Latino Origin
                                White_Latino ="B03002_013",      # Hispanic/Latino - White
                                White_Alone ="B02001_002",       # White Alone 
                                Black_Alone ="B02001_003",       # Black Alone
                                AI_or_AN_Alone ="B02001_004",    # American Indian or Alaskan Native
                                Asian_Alone ="B02001_005",       # Asian Alone
                                NH_or_PI_Alone ="B02001_006",    # Native Hawaiian or Pacific Islander
                                Other_race_alone = "B02001_007", # 
                                TwoPlus = "B02001_008",
                                
                                ForeignBorn = "B99051_005",         # Number of individuals born outside the US
                                Veterans = "B21001_002",            # Number of veterans
                                
                                No_Health_Insuranceu19 = "B27010_017",  # Number of individuals under 19 with no insurance
                                No_Health_Insurance1935 = "B27010_033", # Number of individuals ages 19-35 with no insurance
                                No_Health_Insurance3665 = "B27010_050", # Number of individuals ages 35-65 with no insurance
                                No_Health_Insurance65p = "B27010_066",  # Number of individuals over 65 with no insurance
                                Single_Dad = "B09002_009",              # Number of single father households 
                                Single_Mom = "B09002_015",              # Number of single mother households
                                SNAP_Disability = "B22010_006",     # The two disability vars must -  
                                noSnap_Disability = "B22010_003",   # be added together
                                SNAP_noDisability = "B22010_007",  # used to calculate the total number of households
                                
                                ######################
                                ### Economic Data ####
                                ######################
                                Poverty = "B17017_002",                 # Number of individuals with income under the poverty line 
                                Median_Income = "B19013_001",           # Median Household income
                                Median_Earnings = "B20002_001", # Median Earnins
                                public_assistance = "B19057_002",       # Number of households receiving public assistance
                                SNAP_Users = "B19058_002",              # Number of households receiving SNAP benefits
                                notSNAP_User = "B19058_003",            # Number of households not receiving SNAP benefits
                                Employed = "B23025_004", #Number of employed individuals
                                Unemployed = "B23025_005",              # Number of unemployed individuals
                                LaborForce = "B23025_002",              # Number of individuals 16+ in the labor force
                                NotInLaborForce = "B23025_007",         # Number of individuals 16+ not in the labor force
                                Armed_Forces = "B23025_006", # Number of individuals employed by the armed forces
                                
                                
                                
                                ######################
                                #### Housing Data ####
                                ######################
                                Total_Households = "B11001_001",      # Number of Households
                                Total_Housing_Units = "B25001_001", # Number of Housing Units
                                mobile_home = "B25024_010",           # Number of Mobile Homes
                                total_vacant = "B25002_003",          # Number of vacant units
                                total_occupied = "B25002_002",
                                owner_occ = "B25003_002",             # Number of Owner-Occupied units
                                renter_occ = "B25003_003",            # Number of Renter-Occupied units
                                avg_hh_size = "B25010_001", # Average HH size
                                #geographic_mobility_past12months = "",
                                med_home_value = "B25077_001",       # Median (self-reported) house value of owner-occupied housing
                                med_owner_costs = "B25088_002",  # Median Homeowner Selected Costs
                                med_owner_costs_pctOfIncome = "B25092_002", # Median Homeowner Selected Costs as Percentage of Income
                                #med_contract_rent = "", # Median Contract Rent
                                med_gross_rent = "B25064_001",              # Median Gross Rent
                                med_rent_asPctofInc = "B25071_001", # Median gross rent as a percent of income
                                #substandard_housing = "", # Housing considered substandard *Tract only
                                no_vehicles_r = "B25045_012",         # Number of renter households with no vehicles available
                                no_vehicles_o = "B25045_003",
                                
                                
                                ######################
                                ### Education Data ###
                                ######################
                                Undergrad_enrollment = "B14007_017",# College student (undergrad)
                                Grad_enrollment = "B14007_018",     # College student (Grad)
                                
                                High_School_Only = "B15003_017", # Number of individuals over 25 with 
                                ged_Only = "B15003_018", # Number of individuals over 25 with 
                                Bachelors_Degree = "B15003_022", # Number of individuals over 25 with 
                                Masters_Degree = "B15003_023", # Number of individuals over 25 with 
                                Professional_Degree = "B15003_024", # Number of individuals over 25 with 
                                Doctorate_Degree = "B15003_025", # Number of individuals over 25 with 
                                
                                # English speaking ability is currently under education, but it can also be placed in Demographic info.
                                # Here all the different fields for poor English are downloaded by language
                                # All these variables will be added by language to give, for example, number of Spanish speakers who speak English less than well
                                # Those variables are then summed to give the number of individuals who speak English less than well. 
                                #Pop_over_5 = "B16005_001", #Population 5 years and over (for percentage calculations)
                                # Spanish
                                Spanish17noa = "B16004_008",   # Under 17 -speak English "not at all"
                                Spanish1864noa = "B16004_030", # 18 - 64 - speak English "not at all"
                                Spanish65noa = "B16004_052",   # 65+ - speak English  "not at all"
                                Spanish17nw = "B16004_007",    # Under 17 -speak English "not well"
                                Spanish1864nw = "B16004_029",  # 18 - 64 - speak English "not well"
                                Spanish65nw = "B16004_051",    # 65+ - speak English "not well"
                                # IndoEuropean Language
                                Indoeuro17noa = "B16004_013",     # Under 17 -speak English "not at all"
                                Indoeuro1864noa = "B16004_035",   # 18 - 64 - speak English "not at all"
                                Indoeuro65noa = "B16004_057",# 65+ - speak English  "not at all"
                                Indoeuro17nw = "B16004_012",      # Under 17 -speak English "not well"
                                Indoeuro1864nw = "B16004_034",    # 18 - 64 - speak English "not well"
                                Indoeuro65nw = "B16004_056",      # 65+ - speak English "not well"
                                # Asian or Pacific Island Language
                                API17noa = "B16004_018",       # Under 17 -speak English "not at all"
                                API1864noa = "B16004_040",     # 18 - 64 - speak English "not at all"
                                API65noa = "B16004_062",       # 65+ - speak English  "not at all"
                                API17nw = "B16004_017",        # Under 17 -speak English "not well"
                                API1864nw = "B16004_039",      # 18 - 64 - speak English "not well"
                                API65nw = "B16004_061",        # 65+ - speak English "not well"
                                # Some Other Language
                                Other17noa = "B16004_023",     # Under 17 -speak English "not at all"
                                Other1864noa = "B16004_045",   # 18 - 64 - speak English "not at all"
                                Other65noa = "B16004_067",     #65+ - speak English  "not at all"
                                Other17nw = "B16004_022",      # Under 17 -speak English "not well"
                                Other1864nw = "B16004_044",    # 18 - 64 - speak English "not well"
                                Other65nw = "B16004_066"))      # 65+ - speak English "not well"
  temp$year <- i 
  State_Download<-rbind(State_Download,temp)    
  
  
} 



# Drop all the Margin of Error fields, remove the E from Estimate Fields, remove the NAME column
State_Download <- State_Download %>% select(-ends_with("M")) 
names(State_Download) <- sub('E$', '', names(State_Download))
State_Download <- subset(State_Download, select = -c(NAM))



# Drop unnecessary stuff
rm(temp, i)


#~~~~~~~~~~~~~~~~~~~~~~
# Country
#~~~~~~~~~~~~~~~~~~~~~~

US_Download <- get_acs(year = 2013,
                          geography = "us", # Get data at Block-Group level (as opposed to tract or place)
                          output = "wide",# report data without duplicating id observations
                          variables = c(Total_Pop = "B01003_001",        # Population
                                        Total_Families = "B11001_002",   # Number of Family Households
                                        Pop_25_and_Over = "B15003_001",  # Population 25 and Over
                                        Pop_under_18 = "B09018_001",     # Population under 18 (children & minorities)
                                        Pop_16_and_Over = "B23025_001",  # Population 16 and over
                                        Children_own = "B09002_001",
                                        Pop_Male = "B01001_002",
                                        Pop_Female = "B01001_026",
                                        
                                        Median_Age = "B01002_001",       # Median Age
                                        Hispanic_Latino ="B03002_012",   # Hispanic or Latino Origin
                                        White_Latino ="B03002_013",      # Hispanic/Latino - White
                                        White_Alone ="B02001_002",       # White Alone 
                                        Black_Alone ="B02001_003",       # Black Alone
                                        AI_or_AN_Alone ="B02001_004",    # American Indian or Alaskan Native
                                        Asian_Alone ="B02001_005",       # Asian Alone
                                        NH_or_PI_Alone ="B02001_006",    # Native Hawaiian or Pacific Islander
                                        Other_race_alone = "B02001_007", # 
                                        TwoPlus = "B02001_008",
                                        
                                        ForeignBorn = "B99051_005",         # Number of individuals born outside the US
                                        Veterans = "B21001_002",            # Number of veterans
                                        
                                        No_Health_Insuranceu19 = "B27010_017",  # Number of individuals under 19 with no insurance
                                        No_Health_Insurance1935 = "B27010_033", # Number of individuals ages 19-35 with no insurance
                                        No_Health_Insurance3665 = "B27010_050", # Number of individuals ages 35-65 with no insurance
                                        No_Health_Insurance65p = "B27010_066",  # Number of individuals over 65 with no insurance
                                        Single_Dad = "B09002_009",              # Number of single father households 
                                        Single_Mom = "B09002_015",              # Number of single mother households
                                        SNAP_Disability = "B22010_006",     # The two disability vars must -  
                                        noSnap_Disability = "B22010_003",   # be added together
                                        SNAP_noDisability = "B22010_007",  # used to calculate the total number of households
                                        
                                        ### Economic Data ###
                                        Poverty = "B17017_002",                 # Number of individuals with income under the poverty line 
                                        Median_Income = "B19013_001",           # Median Household income
                                        Median_Earnings = "B20002_001", # Median Earnins
                                        public_assistance = "B19057_002",       # Number of households receiving public assistance
                                        SNAP_Users = "B19058_002",              # Number of households receiving SNAP benefits
                                        notSNAP_User = "B19058_003",            # Number of households not receiving SNAP benefits
                                        Employed = "B23025_004", #Number of employed individuals
                                        Unemployed = "B23025_005",              # Number of unemployed individuals
                                        LaborForce = "B23025_002",              # Number of individuals 16+ in the labor force
                                        NotInLaborForce = "B23025_007",         # Number of individuals 16+ not in the labor force
                                        Armed_Forces = "B23025_006", # Number of individuals employed by the armed forces
                                        
                                        ### Housing Data ###
                                        Total_Households = "B11001_001",      # Number of Households
                                        Total_Housing_Units = "B25001_001", # Number of Housing Units
                                        mobile_home = "B25024_010",           # Number of Mobile Homes
                                        total_vacant = "B25002_003",          # Number of vacant units
                                        total_occupied = "B25002_002",
                                        owner_occ = "B25003_002",             # Number of Owner-Occupied units
                                        renter_occ = "B25003_003",            # Number of Renter-Occupied units
                                        avg_hh_size = "B25010_001", # Average HH size
                                        #geographic_mobility_past12months = "",
                                        med_home_value = "B25077_001",       # Median (self-reported) house value of owner-occupied housing
                                        med_owner_costs = "B25088_002",  # Median Homeowner Selected Costs
                                        med_owner_costs_pctOfIncome = "B25092_002", # Median Homeowner Selected Costs as Percentage of Income
                                        #med_contract_rent = "", # Median Contract Rent
                                        med_gross_rent = "B25064_001",              # Median Gross Rent
                                        med_rent_asPctofInc = "B25071_001", # Median gross rent as a percent of income
                                        #substandard_housing = "", # Housing considered substandard *Tract only
                                        no_vehicles_r = "B25045_012",         # Number of renter households with no vehicles available
                                        no_vehicles_o = "B25045_003",
                                        
                                        ### Education Data ###
                                        Undergrad_enrollment = "B14007_017",# College student (undergrad)
                                        Grad_enrollment = "B14007_018",     # College student (Grad)
                                        
                                        High_School_Only = "B15003_017", # Number of individuals over 25 with 
                                        ged_Only = "B15003_018", # Number of individuals over 25 with 
                                        Bachelors_Degree = "B15003_022", # Number of individuals over 25 with 
                                        Masters_Degree = "B15003_023", # Number of individuals over 25 with 
                                        Professional_Degree = "B15003_024", # Number of individuals over 25 with 
                                        Doctorate_Degree = "B15003_025", # Number of individuals over 25 with 
                                        
                                        # English speaking ability is currently under education, but it can also be placed in Demographic info.
                                        # Here all the different fields for poor English are downloaded by language
                                        # All these variables will be added by language to give, for example, number of Spanish speakers who speak English less than well
                                        # Those variables are then summed to give the number of individuals who speak English less than well. 
                                        #Pop_over_5 = "B16005_001", #Population 5 years and over (for percentage calculations)
                                        # Spanish
                                        Spanish17noa = "B16004_008",   # Under 17 -speak English "not at all"
                                        Spanish1864noa = "B16004_030", # 18 - 64 - speak English "not at all"
                                        Spanish65noa = "B16004_052",   # 65+ - speak English  "not at all"
                                        Spanish17nw = "B16004_007",    # Under 17 -speak English "not well"
                                        Spanish1864nw = "B16004_029",  # 18 - 64 - speak English "not well"
                                        Spanish65nw = "B16004_051",    # 65+ - speak English "not well"
                                        # IndoEuropean Language
                                        Indoeuro17noa = "B16004_013",     # Under 17 -speak English "not at all"
                                        Indoeuro1864noa = "B16004_035",   # 18 - 64 - speak English "not at all"
                                        Indoeuro65noa = "B16004_057",# 65+ - speak English  "not at all"
                                        Indoeuro17nw = "B16004_012",      # Under 17 -speak English "not well"
                                        Indoeuro1864nw = "B16004_034",    # 18 - 64 - speak English "not well"
                                        Indoeuro65nw = "B16004_056",      # 65+ - speak English "not well"
                                        # Asian or Pacific Island Language
                                        API17noa = "B16004_018",       # Under 17 -speak English "not at all"
                                        API1864noa = "B16004_040",     # 18 - 64 - speak English "not at all"
                                        API65noa = "B16004_062",       # 65+ - speak English  "not at all"
                                        API17nw = "B16004_017",        # Under 17 -speak English "not well"
                                        API1864nw = "B16004_039",      # 18 - 64 - speak English "not well"
                                        API65nw = "B16004_061",        # 65+ - speak English "not well"
                                        # Some Other Language
                                        Other17noa = "B16004_023",     # Under 17 -speak English "not at all"
                                        Other1864noa = "B16004_045",   # 18 - 64 - speak English "not at all"
                                        Other65noa = "B16004_067",     #65+ - speak English  "not at all"
                                        Other17nw = "B16004_022",      # Under 17 -speak English "not well"
                                        Other1864nw = "B16004_044",    # 18 - 64 - speak English "not well"
                                        Other65nw = "B16004_066"      # 65+ - speak English "not well"
                          )) 
US_Download$year <-2013

for (i in 2014:2021){ 
  temp <- get_acs(year = i,
                  geography = "us", # Get data at Block-Group level (as opposed to tract or place)
                  output = "wide",# report data without duplicating id observations
                  variables = c(Total_Pop = "B01003_001",        # Population
                                Total_Families = "B11001_002",   # Number of Family Households
                                Pop_25_and_Over = "B15003_001",  # Population 25 and Over
                                Pop_under_18 = "B09018_001",     # Population under 18 (children & minorities)
                                Pop_16_and_Over = "B23025_001",  # Population 16 and over
                                Children_own = "B09002_001",
                                Pop_Male = "B01001_002",
                                Pop_Female = "B01001_026",
                                
                                Median_Age = "B01002_001",       # Median Age
                                Hispanic_Latino ="B03002_012",   # Hispanic or Latino Origin
                                White_Latino ="B03002_013",      # Hispanic/Latino - White
                                White_Alone ="B02001_002",       # White Alone 
                                Black_Alone ="B02001_003",       # Black Alone
                                AI_or_AN_Alone ="B02001_004",    # American Indian or Alaskan Native
                                Asian_Alone ="B02001_005",       # Asian Alone
                                NH_or_PI_Alone ="B02001_006",    # Native Hawaiian or Pacific Islander
                                Other_race_alone = "B02001_007", # 
                                TwoPlus = "B02001_008",
                                
                                ForeignBorn = "B99051_005",         # Number of individuals born outside the US
                                Veterans = "B21001_002",            # Number of veterans
                                
                                No_Health_Insuranceu19 = "B27010_017",  # Number of individuals under 19 with no insurance
                                No_Health_Insurance1935 = "B27010_033", # Number of individuals ages 19-35 with no insurance
                                No_Health_Insurance3665 = "B27010_050", # Number of individuals ages 35-65 with no insurance
                                No_Health_Insurance65p = "B27010_066",  # Number of individuals over 65 with no insurance
                                Single_Dad = "B09002_009",              # Number of single father households 
                                Single_Mom = "B09002_015",              # Number of single mother households
                                SNAP_Disability = "B22010_006",     # The two disability vars must -  
                                noSnap_Disability = "B22010_003",   # be added together
                                SNAP_noDisability = "B22010_007",  # used to calculate the total number of households
                                
                                ######################
                                ### Economic Data ####
                                ######################
                                Poverty = "B17017_002",                 # Number of individuals with income under the poverty line 
                                Median_Income = "B19013_001",           # Median Household income
                                Median_Earnings = "B20002_001", # Median Earnins
                                public_assistance = "B19057_002",       # Number of households receiving public assistance
                                SNAP_Users = "B19058_002",              # Number of households receiving SNAP benefits
                                notSNAP_User = "B19058_003",            # Number of households not receiving SNAP benefits
                                Employed = "B23025_004", #Number of employed individuals
                                Unemployed = "B23025_005",              # Number of unemployed individuals
                                LaborForce = "B23025_002",              # Number of individuals 16+ in the labor force
                                NotInLaborForce = "B23025_007",         # Number of individuals 16+ not in the labor force
                                Armed_Forces = "B23025_006", # Number of individuals employed by the armed forces
                                
                                
                                
                                ######################
                                #### Housing Data ####
                                ######################
                                Total_Households = "B11001_001",      # Number of Households
                                Total_Housing_Units = "B25001_001", # Number of Housing Units
                                mobile_home = "B25024_010",           # Number of Mobile Homes
                                total_vacant = "B25002_003",          # Number of vacant units
                                total_occupied = "B25002_002",
                                owner_occ = "B25003_002",             # Number of Owner-Occupied units
                                renter_occ = "B25003_003",            # Number of Renter-Occupied units
                                avg_hh_size = "B25010_001", # Average HH size
                                #geographic_mobility_past12months = "",
                                med_home_value = "B25077_001",       # Median (self-reported) house value of owner-occupied housing
                                med_owner_costs = "B25088_002",  # Median Homeowner Selected Costs
                                med_owner_costs_pctOfIncome = "B25092_002", # Median Homeowner Selected Costs as Percentage of Income
                                #med_contract_rent = "", # Median Contract Rent
                                med_gross_rent = "B25064_001",              # Median Gross Rent
                                med_rent_asPctofInc = "B25071_001", # Median gross rent as a percent of income
                                #substandard_housing = "", # Housing considered substandard *Tract only
                                no_vehicles_r = "B25045_012",         # Number of renter households with no vehicles available
                                no_vehicles_o = "B25045_003",
                                
                                
                                ######################
                                ### Education Data ###
                                ######################
                                Undergrad_enrollment = "B14007_017",# College student (undergrad)
                                Grad_enrollment = "B14007_018",     # College student (Grad)
                                
                                High_School_Only = "B15003_017", # Number of individuals over 25 with 
                                ged_Only = "B15003_018", # Number of individuals over 25 with 
                                Bachelors_Degree = "B15003_022", # Number of individuals over 25 with 
                                Masters_Degree = "B15003_023", # Number of individuals over 25 with 
                                Professional_Degree = "B15003_024", # Number of individuals over 25 with 
                                Doctorate_Degree = "B15003_025", # Number of individuals over 25 with 
                                
                                # English speaking ability is currently under education, but it can also be placed in Demographic info.
                                # Here all the different fields for poor English are downloaded by language
                                # All these variables will be added by language to give, for example, number of Spanish speakers who speak English less than well
                                # Those variables are then summed to give the number of individuals who speak English less than well. 
                                #Pop_over_5 = "B16005_001", #Population 5 years and over (for percentage calculations)
                                # Spanish
                                Spanish17noa = "B16004_008",   # Under 17 -speak English "not at all"
                                Spanish1864noa = "B16004_030", # 18 - 64 - speak English "not at all"
                                Spanish65noa = "B16004_052",   # 65+ - speak English  "not at all"
                                Spanish17nw = "B16004_007",    # Under 17 -speak English "not well"
                                Spanish1864nw = "B16004_029",  # 18 - 64 - speak English "not well"
                                Spanish65nw = "B16004_051",    # 65+ - speak English "not well"
                                # IndoEuropean Language
                                Indoeuro17noa = "B16004_013",     # Under 17 -speak English "not at all"
                                Indoeuro1864noa = "B16004_035",   # 18 - 64 - speak English "not at all"
                                Indoeuro65noa = "B16004_057",# 65+ - speak English  "not at all"
                                Indoeuro17nw = "B16004_012",      # Under 17 -speak English "not well"
                                Indoeuro1864nw = "B16004_034",    # 18 - 64 - speak English "not well"
                                Indoeuro65nw = "B16004_056",      # 65+ - speak English "not well"
                                # Asian or Pacific Island Language
                                API17noa = "B16004_018",       # Under 17 -speak English "not at all"
                                API1864noa = "B16004_040",     # 18 - 64 - speak English "not at all"
                                API65noa = "B16004_062",       # 65+ - speak English  "not at all"
                                API17nw = "B16004_017",        # Under 17 -speak English "not well"
                                API1864nw = "B16004_039",      # 18 - 64 - speak English "not well"
                                API65nw = "B16004_061",        # 65+ - speak English "not well"
                                # Some Other Language
                                Other17noa = "B16004_023",     # Under 17 -speak English "not at all"
                                Other1864noa = "B16004_045",   # 18 - 64 - speak English "not at all"
                                Other65noa = "B16004_067",     #65+ - speak English  "not at all"
                                Other17nw = "B16004_022",      # Under 17 -speak English "not well"
                                Other1864nw = "B16004_044",    # 18 - 64 - speak English "not well"
                                Other65nw = "B16004_066"))      # 65+ - speak English "not well"
  temp$year <- i 
  US_Download<-rbind(US_Download,temp)    
  
  
} 



# Drop all the Margin of Error fields, remove the E from Estimate Fields, remove the NAME column
US_Download <- US_Download %>% select(-ends_with("M")) 
names(US_Download) <- sub('E$', '', names(US_Download))
US_Download <- subset(US_Download, select = -c(NAM))



# Drop unnecessary stuff
rm(temp,i)
#End Dataset Configuration
########################################################

########################################################
# Sum Populations by City/District (or find median)
########################################################

Population <- FC_Stats%>%
  group_by(year) %>%
  summarize(# Demographic
    total_pop = sum(Total_Pop, na.rm = TRUE),
    pop_over_25 = sum(Pop_25_and_Over, na.rm = TRUE),
    white = sum(White_Alone, na.rm = TRUE),
    white_hisplat = sum(White_Latino, na.rm = TRUE),
    hisplat = sum(Hispanic_Latino, na.rm = TRUE),
    children = sum(Pop_under_18, na.rm = TRUE),
    medAge = median(Median_Age, na.rm= TRUE),
    SNAP_Disability = sum(SNAP_Disability, na.rm= TRUE),
    noSnap_Disability= sum(noSnap_Disability, na.rm= TRUE),
    Single_Dad = sum(Single_Dad, na.rm= TRUE),
    Single_Mom = sum(Single_Mom, na.rm= TRUE),
    
    
    black = sum(Black_Alone, na.rm= TRUE),
    native = sum(AI_or_AN_Alone, na.rm= TRUE),
    asian = sum(Asian_Alone, na.rm= TRUE),
    hipi = sum(NH_or_PI_Alone, na.rm= TRUE),
    twoplus = sum(TwoPlus, na.rm = TRUE),
    additional = sum(Other_race_alone, na.rm= TRUE),
    ForeignBorn = sum(ForeignBorn, na.rm= TRUE),
    Veterans = sum(Veterans, na.rm= TRUE),
    
    No_Health_Insuranceu19 = sum(No_Health_Insuranceu19, na.rm= TRUE),
    No_Health_Insurance1935 = sum(No_Health_Insurance1935, na.rm= TRUE),
    No_Health_Insurance3665 = sum(No_Health_Insurance3665, na.rm= TRUE),
    No_Health_Insurance65p = sum(No_Health_Insurance65p, na.rm= TRUE),
    
    # Economic
    medIncome = median(Median_Income, na.rm = TRUE),
    poverty = sum(Poverty, na.rm = TRUE),
    public_ass = sum(public_assistance, na.rm = TRUE),
    unemployed = sum(Unemployed, na.rm = TRUE),
    laborforce = sum(LaborForce, na.rm = TRUE),
    no_vehicles_r = sum(no_vehicles_r, na.rm = TRUE),
    no_vehicles_o = sum(no_vehicles_o, na.rm = TRUE),
    
    # Housing
    med_home_val = median(med_home_value, na.rm = TRUE),
    med_gross_rent = median(med_gross_rent, na.rm = TRUE),
    med_owner_costs = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    Total_Households = sum(Total_Households, na.rm = TRUE),
    Total_Housing_Units =  sum(Total_Housing_Units, na.rm = TRUE),
    mobile_home = sum(mobile_home, na.rm = TRUE),
    total_vacant = sum(total_vacant, na.rm = TRUE),
    total_occupied= sum(total_occupied, na.rm = TRUE),
    owner_occ = sum(owner_occ, na.rm = TRUE),
    renter_occ = sum(renter_occ, na.rm = TRUE),
    med_owner_costs_pctOfIncome = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    med_rent_asPctofInc = median(med_rent_asPctofInc, na.rm = TRUE),
    avg_hh_size= mean(avg_hh_size, na.rm = TRUE),
    # Education
    grads = sum(Grad_enrollment, na.rm = TRUE),
    undergrads = sum(Undergrad_enrollment, na.rm = TRUE),
    hs_only = sum(High_School_Only, na.rm = TRUE),
    bachelors = sum(Bachelors_Degree, na.rm = TRUE),
    masters = sum(Masters_Degree, na.rm = TRUE),
    professional = sum(Professional_Degree, na.rm = TRUE),
    doctorate = sum(Doctorate_Degree, na.rm = TRUE),
    #language
    Spanish17noa= sum(Spanish17noa, na.rm = TRUE),
    Spanish1864noa= sum(Spanish1864noa, na.rm = TRUE),
    Spanish65noa= sum(Spanish65noa, na.rm = TRUE),
    Spanish17nw= sum(Spanish17nw, na.rm = TRUE),
    Spanish1864nw= sum(Spanish1864nw, na.rm = TRUE),
    Spanish65nw= sum(Spanish65nw, na.rm = TRUE)
    
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Group by District
#~~~~~~~~~~~~~~~~~~~~~~~~~~
FC_Stat_Temp <- FC_Stats[FC_Stats$year == 2020,]
obs <- FC_Stat_Temp %>% group_by(GEOID) %>% summarize(count=n())
FC_Stat_DistAmmendment <- FC_Stat_Temp[FC_Stat_Temp$GEOID != "080690016054",]
FC_Stat_DistAmmendment2 <- FC_Stat_Temp[FC_Stat_Temp$GEOID == "080690016054",]
ProblemChild$GEOID <- "080690016054"
ProblemChild <- subset(ProblemChild, select = c(DIST, GEOID, PERCENTAGE))
FC_Stat_DistAmmendment <- subset(FC_Stat_DistAmmendment, select = - c(90:103))
FC_Stat_DistAmmendment <- subset(FC_Stat_DistAmmendment, select = - c(91:96))
# Calculate the fields as a percentage of the block group that the city overlaps
FC_Stat_DistAmmendment2 <- subset(FC_Stat_DistAmmendment2, select = - c(90:110))
temporary <- sapply(FC_Stat_DistAmmendment2[,-c(1,31,32,83,84,85,86,87,88,89)], '*', ProblemChild$PERCENTAGE) 
PC <- cbind(ProblemChild, temporary)
PC <- subset(PC, select = -c(PERCENTAGE))
temp <- FC_Stat_DistAmmendment2[,c(31,32,83,84,85,86,87,88,89)]
temp <- rbind(temp, temp[rep(1, 2), ])
PC <- cbind(PC,temp)
dsfw <- rbind(FC_Stat_DistAmmendment, PC)


District <- FC_Stats%>%
  group_by(year, DIST) %>%
  summarize(# Demographic
    total_pop = sum(Total_Pop, na.rm = TRUE),
    pop_over_25 = sum(Pop_25_and_Over, na.rm = TRUE),
    white = sum(White_Alone, na.rm = TRUE),
    white_hisplat = sum(White_Latino, na.rm = TRUE),
    hisplat = sum(Hispanic_Latino, na.rm = TRUE),
    children = sum(Pop_under_18, na.rm = TRUE),
    medAge = median(Median_Age, na.rm= TRUE),
    SNAP_Disability = sum(SNAP_Disability, na.rm= TRUE),
    noSnap_Disability= sum(noSnap_Disability, na.rm= TRUE),
    Single_Dad = sum(Single_Dad, na.rm= TRUE),
    Single_Mom = sum(Single_Mom, na.rm= TRUE),
    
    
    black = sum(Black_Alone, na.rm= TRUE),
    native = sum(AI_or_AN_Alone, na.rm= TRUE),
    asian = sum(Asian_Alone, na.rm= TRUE),
    hipi = sum(NH_or_PI_Alone, na.rm= TRUE),
    twoplus = sum(TwoPlus, na.rm = TRUE),
    additional = sum(Other_race_alone, na.rm= TRUE),
    ForeignBorn = sum(ForeignBorn, na.rm= TRUE),
    Veterans = sum(Veterans, na.rm= TRUE),
    
    No_Health_Insuranceu19 = sum(No_Health_Insuranceu19, na.rm= TRUE),
    No_Health_Insurance1935 = sum(No_Health_Insurance1935, na.rm= TRUE),
    No_Health_Insurance3665 = sum(No_Health_Insurance3665, na.rm= TRUE),
    No_Health_Insurance65p = sum(No_Health_Insurance65p, na.rm= TRUE),
    
    # Economic
    medIncome = median(Median_Income, na.rm = TRUE),
    poverty = sum(Poverty, na.rm = TRUE),
    public_ass = sum(public_assistance, na.rm = TRUE),
    unemployed = sum(Unemployed, na.rm = TRUE),
    laborforce = sum(LaborForce, na.rm = TRUE),
    no_vehicles_r = sum(no_vehicles_r, na.rm = TRUE),
    no_vehicles_o = sum(no_vehicles_o, na.rm = TRUE),
    
    # Housing
    med_home_val = median(med_home_value, na.rm = TRUE),
    med_gross_rent = median(med_gross_rent, na.rm = TRUE),
    med_owner_costs = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    Total_Households = sum(Total_Households, na.rm = TRUE),
    Total_Housing_Units =  sum(Total_Housing_Units, na.rm = TRUE),
    mobile_home = sum(mobile_home, na.rm = TRUE),
    total_vacant = sum(total_vacant, na.rm = TRUE),
    total_occupied= sum(total_occupied, na.rm = TRUE),
    owner_occ = sum(owner_occ, na.rm = TRUE),
    renter_occ = sum(renter_occ, na.rm = TRUE),
    med_owner_costs_pctOfIncome = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    med_rent_asPctofInc = median(med_rent_asPctofInc, na.rm = TRUE),
    avg_hh_size= mean(avg_hh_size, na.rm = TRUE),
    # Education
    grads = sum(Grad_enrollment, na.rm = TRUE),
    undergrads = sum(Undergrad_enrollment, na.rm = TRUE),
    hs_only = sum(High_School_Only, na.rm = TRUE),
    bachelors = sum(Bachelors_Degree, na.rm = TRUE),
    masters = sum(Masters_Degree, na.rm = TRUE),
    professional = sum(Professional_Degree, na.rm = TRUE),
    doctorate = sum(Doctorate_Degree, na.rm = TRUE),
    #language
    Spanish17noa= sum(Spanish17noa, na.rm = TRUE),
    Spanish1864noa= sum(Spanish1864noa, na.rm = TRUE),
    Spanish65noa= sum(Spanish65noa, na.rm = TRUE),
    Spanish17nw= sum(Spanish17nw, na.rm = TRUE),
    Spanish1864nw= sum(Spanish1864nw, na.rm = TRUE),
    Spanish65nw= sum(Spanish65nw, na.rm = TRUE)
    
  )

District$pct_renting

#End Summation by Geography
########################################################

########################################################
# Sum Populations by County/State (or find median)
########################################################

County <- County_Download%>%
  group_by(year) %>%
  summarize(# Demographic
    total_pop = sum(Total_Pop, na.rm = TRUE),
    white = sum(White_Alone, na.rm = TRUE),
    white_hisplat = sum(White_Latino, na.rm = TRUE),
    hisplat = sum(Hispanic_Latino, na.rm = TRUE),
    children = sum(Pop_under_18, na.rm = TRUE),
    medAge = median(Median_Age, na.rm= TRUE),
    SNAP_Disability = sum(SNAP_Disability, na.rm= TRUE),
    noSnap_Disability= sum(noSnap_Disability, na.rm= TRUE),
    Single_Dad = sum(Single_Dad, na.rm= TRUE),
    Single_Mom = sum(Single_Mom, na.rm= TRUE),
    
    
    black = sum(Black_Alone, na.rm= TRUE),
    native = sum(AI_or_AN_Alone, na.rm= TRUE),
    asian = sum(Asian_Alone, na.rm= TRUE),
    hipi = sum(NH_or_PI_Alone, na.rm= TRUE),
    twoplus = sum(TwoPlus, na.rm = TRUE),
    additional = sum(Other_race_alone, na.rm= TRUE),
    ForeignBorn = sum(ForeignBorn, na.rm= TRUE),
    Veterans = sum(Veterans, na.rm= TRUE),
    
    No_Health_Insuranceu19 = sum(No_Health_Insuranceu19, na.rm= TRUE),
    No_Health_Insurance1935 = sum(No_Health_Insurance1935, na.rm= TRUE),
    No_Health_Insurance3665 = sum(No_Health_Insurance3665, na.rm= TRUE),
    No_Health_Insurance65p = sum(No_Health_Insurance65p, na.rm= TRUE),
    
    # Economic
    medIncome = median(Median_Income, na.rm = TRUE),
    poverty = sum(Poverty, na.rm = TRUE),
    public_ass = sum(public_assistance, na.rm = TRUE),
    unemployed = sum(Unemployed, na.rm = TRUE),
    laborforce = sum(LaborForce, na.rm = TRUE),
    no_vehicles_r = sum(no_vehicles_r, na.rm = TRUE),
    no_vehicles_o = sum(no_vehicles_o, na.rm = TRUE),
    
    # Housing
    med_home_val = median(med_home_value, na.rm = TRUE),
    med_gross_rent = median(med_gross_rent, na.rm = TRUE),
    med_owner_costs = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    Total_Households = sum(Total_Households, na.rm = TRUE),
    Total_Housing_Units =  sum(Total_Housing_Units, na.rm = TRUE),
    mobile_home = sum(mobile_home, na.rm = TRUE),
    total_vacant = sum(total_vacant, na.rm = TRUE),
    total_occupied= sum(total_occupied, na.rm = TRUE),
    owner_occ = sum(owner_occ, na.rm = TRUE),
    renter_occ = sum(renter_occ, na.rm = TRUE),
    med_owner_costs_pctOfIncome = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    med_rent_asPctofInc = median(med_rent_asPctofInc, na.rm = TRUE),
    avg_hh_size= mean(avg_hh_size, na.rm = TRUE),
    # Education
    grads = sum(Grad_enrollment, na.rm = TRUE),
    undergrads = sum(Undergrad_enrollment, na.rm = TRUE),
    bachelors = sum(Bachelors_Degree, na.rm = TRUE),
    masters = sum(Masters_Degree, na.rm = TRUE),
    professional = sum(Professional_Degree, na.rm = TRUE),
    doctorate = sum(Doctorate_Degree, na.rm = TRUE),
    #language
    Spanish17noa= sum(Spanish17noa, na.rm = TRUE),
    Spanish1864noa= sum(Spanish1864noa, na.rm = TRUE),
    Spanish65noa= sum(Spanish65noa, na.rm = TRUE),
    Spanish17nw= sum(Spanish17nw, na.rm = TRUE),
    Spanish1864nw= sum(Spanish1864nw, na.rm = TRUE),
    Spanish65nw= sum(Spanish65nw, na.rm = TRUE)
    
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Group by State
#~~~~~~~~~~~~~~~~~~~~~~~~~~

State <- State_Download%>%
  group_by(year) %>%
  summarize(# Demographic
    total_pop = sum(Total_Pop, na.rm = TRUE),
    white = sum(White_Alone, na.rm = TRUE),
    white_hisplat = sum(White_Latino, na.rm = TRUE),
    hisplat = sum(Hispanic_Latino, na.rm = TRUE),
    children = sum(Pop_under_18, na.rm = TRUE),
    medAge = median(Median_Age, na.rm= TRUE),
    SNAP_Disability = sum(SNAP_Disability, na.rm= TRUE),
    noSnap_Disability= sum(noSnap_Disability, na.rm= TRUE),
    Single_Dad = sum(Single_Dad, na.rm= TRUE),
    Single_Mom = sum(Single_Mom, na.rm= TRUE),
    
    
    black = sum(Black_Alone, na.rm= TRUE),
    native = sum(AI_or_AN_Alone, na.rm= TRUE),
    asian = sum(Asian_Alone, na.rm= TRUE),
    hipi = sum(NH_or_PI_Alone, na.rm= TRUE),
    twoplus = sum(TwoPlus, na.rm = TRUE),
    additional = sum(Other_race_alone, na.rm= TRUE),
    ForeignBorn = sum(ForeignBorn, na.rm= TRUE),
    Veterans = sum(Veterans, na.rm= TRUE),
    
    No_Health_Insuranceu19 = sum(No_Health_Insuranceu19, na.rm= TRUE),
    No_Health_Insurance1935 = sum(No_Health_Insurance1935, na.rm= TRUE),
    No_Health_Insurance3665 = sum(No_Health_Insurance3665, na.rm= TRUE),
    No_Health_Insurance65p = sum(No_Health_Insurance65p, na.rm= TRUE),
    
    # Economic
    medIncome = median(Median_Income, na.rm = TRUE),
    poverty = sum(Poverty, na.rm = TRUE),
    public_ass = sum(public_assistance, na.rm = TRUE),
    unemployed = sum(Unemployed, na.rm = TRUE),
    laborforce = sum(LaborForce, na.rm = TRUE),
    no_vehicles_r = sum(no_vehicles_r, na.rm = TRUE),
    no_vehicles_o = sum(no_vehicles_o, na.rm = TRUE),
    
    # Housing
    med_home_val = median(med_home_value, na.rm = TRUE),
    med_gross_rent = median(med_gross_rent, na.rm = TRUE),
    med_owner_costs = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    Total_Households = sum(Total_Households, na.rm = TRUE),
    Total_Housing_Units =  sum(Total_Housing_Units, na.rm = TRUE),
    mobile_home = sum(mobile_home, na.rm = TRUE),
    total_vacant = sum(total_vacant, na.rm = TRUE),
    total_occupied= sum(total_occupied, na.rm = TRUE),
    owner_occ = sum(owner_occ, na.rm = TRUE),
    renter_occ = sum(renter_occ, na.rm = TRUE),
    med_owner_costs_pctOfIncome = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    med_rent_asPctofInc = median(med_rent_asPctofInc, na.rm = TRUE),
    avg_hh_size= mean(avg_hh_size, na.rm = TRUE),
    # Education
    grads = sum(Grad_enrollment, na.rm = TRUE),
    undergrads = sum(Undergrad_enrollment, na.rm = TRUE),
    bachelors = sum(Bachelors_Degree, na.rm = TRUE),
    masters = sum(Masters_Degree, na.rm = TRUE),
    professional = sum(Professional_Degree, na.rm = TRUE),
    doctorate = sum(Doctorate_Degree, na.rm = TRUE),
    #language
    Spanish17noa= sum(Spanish17noa, na.rm = TRUE),
    Spanish1864noa= sum(Spanish1864noa, na.rm = TRUE),
    Spanish65noa= sum(Spanish65noa, na.rm = TRUE),
    Spanish17nw= sum(Spanish17nw, na.rm = TRUE),
    Spanish1864nw= sum(Spanish1864nw, na.rm = TRUE),
    Spanish65nw= sum(Spanish65nw, na.rm = TRUE)
    
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Group by State
#~~~~~~~~~~~~~~~~~~~~~~~~~~

Country <- US_Download%>%
  group_by(year) %>%
  summarize(# Demographic
    total_pop = sum(Total_Pop, na.rm = TRUE),
    white = sum(White_Alone, na.rm = TRUE),
    white_hisplat = sum(White_Latino, na.rm = TRUE),
    hisplat = sum(Hispanic_Latino, na.rm = TRUE),
    children = sum(Pop_under_18, na.rm = TRUE),
    medAge = median(Median_Age, na.rm= TRUE),
    SNAP_Disability = sum(SNAP_Disability, na.rm= TRUE),
    noSnap_Disability= sum(noSnap_Disability, na.rm= TRUE),
    Single_Dad = sum(Single_Dad, na.rm= TRUE),
    Single_Mom = sum(Single_Mom, na.rm= TRUE),
    
    
    black = sum(Black_Alone, na.rm= TRUE),
    native = sum(AI_or_AN_Alone, na.rm= TRUE),
    asian = sum(Asian_Alone, na.rm= TRUE),
    hipi = sum(NH_or_PI_Alone, na.rm= TRUE),
    twoplus = sum(TwoPlus, na.rm = TRUE),
    additional = sum(Other_race_alone, na.rm= TRUE),
    ForeignBorn = sum(ForeignBorn, na.rm= TRUE),
    Veterans = sum(Veterans, na.rm= TRUE),
    
    No_Health_Insuranceu19 = sum(No_Health_Insuranceu19, na.rm= TRUE),
    No_Health_Insurance1935 = sum(No_Health_Insurance1935, na.rm= TRUE),
    No_Health_Insurance3665 = sum(No_Health_Insurance3665, na.rm= TRUE),
    No_Health_Insurance65p = sum(No_Health_Insurance65p, na.rm= TRUE),
    
    # Economic
    medIncome = median(Median_Income, na.rm = TRUE),
    poverty = sum(Poverty, na.rm = TRUE),
    public_ass = sum(public_assistance, na.rm = TRUE),
    unemployed = sum(Unemployed, na.rm = TRUE),
    laborforce = sum(LaborForce, na.rm = TRUE),
    no_vehicles_r = sum(no_vehicles_r, na.rm = TRUE),
    no_vehicles_o = sum(no_vehicles_o, na.rm = TRUE),
    
    # Housing
    med_home_val = median(med_home_value, na.rm = TRUE),
    med_gross_rent = median(med_gross_rent, na.rm = TRUE),
    med_owner_costs = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    Total_Households = sum(Total_Households, na.rm = TRUE),
    Total_Housing_Units =  sum(Total_Housing_Units, na.rm = TRUE),
    mobile_home = sum(mobile_home, na.rm = TRUE),
    total_vacant = sum(total_vacant, na.rm = TRUE),
    total_occupied= sum(total_occupied, na.rm = TRUE),
    owner_occ = sum(owner_occ, na.rm = TRUE),
    renter_occ = sum(renter_occ, na.rm = TRUE),
    med_owner_costs_pctOfIncome = median(med_owner_costs_pctOfIncome, na.rm = TRUE),
    med_rent_asPctofInc = median(med_rent_asPctofInc, na.rm = TRUE),
    avg_hh_size= mean(avg_hh_size, na.rm = TRUE),
    # Education
    grads = sum(Grad_enrollment, na.rm = TRUE),
    undergrads = sum(Undergrad_enrollment, na.rm = TRUE),
    bachelors = sum(Bachelors_Degree, na.rm = TRUE),
    masters = sum(Masters_Degree, na.rm = TRUE),
    professional = sum(Professional_Degree, na.rm = TRUE),
    doctorate = sum(Doctorate_Degree, na.rm = TRUE),
    #language
    Spanish17noa= sum(Spanish17noa, na.rm = TRUE),
    Spanish1864noa= sum(Spanish1864noa, na.rm = TRUE),
    Spanish65noa= sum(Spanish65noa, na.rm = TRUE),
    Spanish17nw= sum(Spanish17nw, na.rm = TRUE),
    Spanish1864nw= sum(Spanish1864nw, na.rm = TRUE),
    Spanish65nw= sum(Spanish65nw, na.rm = TRUE)
    
  )
#End Summation by Geography
########################################################
rm(City_Download, County_Download, State_Download, US_Download)
########################################################
# Variable Creation - City
########################################################

#~~~~~~~~~~~~~~~~
# City Variables
#~~~~~~~~~~~~~~~~

# Demographic
Population$minority <- Population$total_pop - Population$white + Population$white_hisplat
Population$pct_minority <- 100*Population$minority/Population$total_pop
Population$pct_hisplat <- 100*Population$hisplat/Population$total_pop
Population$pct_whitehisplat <- 100*Population$white_hisplat/Population$total_pop
Population$pct_white <- 100*Population$white/Population$total_pop
Population$pct_black <- 100*Population$black/Population$total_pop
Population$pct_native <- 100*Population$native/Population$total_pop
Population$pct_asian <- 100*Population$asian/Population$total_pop
Population$pct_hipi <- 100*Population$hipi/Population$total_pop
Population$pct_additional <- 100*Population$additional/Population$total_pop
Population$pct_twoplus <- 100*Population$twoplus/Population$total_pop
Population$pct_kids <- 100*Population$children/Population$total_pop
Population$pct_disability <- 100*(Population$SNAP_Disability + Population$noSnap_Disability)/Population$total_pop # number of households with at least one member with a disability
Population$SingleParent <- Population$Single_Dad + Population$Single_Mom # Number of Single parent households

#Economic 
Population$No_Health_Insurance <-  Population$No_Health_Insuranceu19 +  Population$No_Health_Insurance1935 + Population$No_Health_Insurance3665 +  Population$No_Health_Insurance65p # Total number uninsured

# Education
Population$inCollege <- 100*(Population$undergrads + Population$grads)/Population$total_pop
Population$hasDegree <- 100*(Population$doctorate+Population$professional+Population$masters+Population$bachelors)/Population$pop_over_25
Population$pct_hs_only <- 100*(Population$hs_only)/Population$pop_over_25
Population$pct_bachelors <- 100*(Population$bachelors)/Population$pop_over_25
Population$pct_masters <- 100*(Population$masters+Population$doctorate+Population$professional)/Population$pop_over_25
Population$pct_prof <- 100*(Population$doctorate+Population$professional)/Population$pop_over_25

# The languages are calculated such that these are the "Percent of individuals 5 years and older who speak English less than well whose native tongue is ____________"
Population$pct_spanish_speakers <- 100*(Population$Spanish17noa + Population$Spanish1864noa + Population$Spanish65noa + Population$Spanish17nw + Population$Spanish1864nw + Population$Spanish65nw)/Population$total_pop #Spanish


# HousingPopulation$no_vehicles <- 100*(Population$no_vehicles_r + Population$no_vehicles_o)/Population$Total_Households # Sum renter and homeowner units with no vehicles available for the total residences with no vehicle
Population$pct_poverty <- 100*Population$poverty/Population$Total_Households
Population$pct_publicAss <- 100*Population$public_ass/Population$total_pop
Population$unemployment <- 100*Population$unemployed/Population$laborforce
Population$LFPR <- 100*Population$laborforce/(Population$total_pop-Population$children)
Population$pct_rent <- 100*Population$renter_occ/Population$total_occupied
Population$pct_own <- 100*Population$owner_occ/Population$total_occupied

#~~~~~~~~~~~~~~~~
# District Variables
#~~~~~~~~~~~~~~~~
# Temp for weird block group without formal solution yet
# Calculate the fields as a percentage of the block group that the city overlaps


# Demographic
District$minority <- District$total_pop - District$white + District$white_hisplat
District$pct_minority <- 100*District$minority/District$total_pop
District$pct_hisplat <- 100*District$hisplat/District$total_pop
District$pct_whitehisplat <- 100*District$white_hisplat/District$total_pop
District$pct_white <- 100*District$white/District$total_pop
District$pct_black <- 100*District$black/District$total_pop
District$pct_native <- 100*District$native/District$total_pop
District$pct_asian <- 100*District$asian/District$total_pop
District$pct_hipi <- 100*District$hipi/District$total_pop
District$pct_additional <- 100*District$additional/District$total_pop
District$pct_twoplus <- 100*District$twoplus/District$total_pop
District$pct_kids <- 100*District$children/District$total_pop
District$pct_disability <- 100*(District$SNAP_Disability + District$noSnap_Disability)/District$total_pop # number of households with at least one member with a disability
District$SingleParent <- District$Single_Dad + District$Single_Mom # Number of Single parent households

#Economic 
District$No_Health_Insurance <-  District$No_Health_Insuranceu19 +  District$No_Health_Insurance1935 + District$No_Health_Insurance3665 +  District$No_Health_Insurance65p # Total number uninsured

# Education
District$inCollege <- 100*(District$undergrads + District$grads)/District$total_pop
District$hasDegree <- 100*(District$doctorate+District$professional+District$masters+District$bachelors)/District$pop_over_25
District$pct_hs_only <- 100*(District$hs_only)/District$pop_over_25
District$pct_bachelors <- 100*(District$bachelors)/District$pop_over_25
District$pct_masters <- 100*(District$masters+District$doctorate+District$professional)/District$pop_over_25
District$pct_prof <- 100*(District$doctorate+District$professional)/District$pop_over_25

# The languages are calculated such that these are the "Percent of individuals 5 years and older who speak English less than well whose native tongue is ____________"
District$pct_spanish_speakers <- 100*(District$Spanish17noa + District$Spanish1864noa + District$Spanish65noa + District$Spanish17nw + District$Spanish1864nw + District$Spanish65nw)/District$total_pop #Spanish


# HousingDistrict$no_vehicles <- 100*(District$no_vehicles_r + District$no_vehicles_o)/District$Total_Households # Sum renter and homeowner units with no vehicles available for the total residences with no vehicle
District$pct_poverty <- 100*District$poverty/District$Total_Households
District$pct_publicAss <- 100*District$public_ass/District$total_pop
District$unemployment <- 100*District$unemployed/District$laborforce
District$LFPR <- 100*District$laborforce/(District$total_pop-District$children)
District$pct_rent <- 100*District$renter_occ/District$total_occupied
District$pct_own <- 100*District$owner_occ/District$total_occupied

# If only 2020 is necessary
Population20 <- Population[Population$year == 2020,]
Districts20 <- District[District$year == 2020,]



view(subset(Population20,select = c(hasDegree, pct_hs_only, pct_bachelors, pct_masters, pct_prof)))


########################################################
# Variable Creation - County and State
########################################################

#~~~~~~~~~~~~~~~~
# County Variables
#~~~~~~~~~~~~~~~~

# Demographic
County$minority <- County$total_pop - County$white + County$white_hisplat
County$pct_minority <- 100*County$minority/County$total_pop
County$pct_hisplat <- 100*County$hisplat/County$total_pop
County$pct_whitehisplat <- 100*County$white_hisplat/County$total_pop
County$pct_white <- 100*County$white/County$total_pop
County$pct_black <- 100*County$black/County$total_pop
County$pct_native <- 100*County$native/County$total_pop
County$pct_asian <- 100*County$asian/County$total_pop
County$pct_hipi <- 100*County$hipi/County$total_pop
County$pct_additional <- 100*County$additional/County$total_pop
County$pct_twoplus <- 100*County$twoplus/County$total_pop
County$pct_kids <- 100*County$children/County$total_pop
County$pct_disability <- 100*(County$SNAP_Disability + County$noSnap_Disability)/County$total_pop # number of households with at least one member with a disability
County$SingleParent <- County$Single_Dad + County$Single_Mom # Number of Single parent households

#Economic 
County$No_Health_Insurance <-  County$No_Health_Insuranceu19 +  County$No_Health_Insurance1935 + County$No_Health_Insurance3665 +  County$No_Health_Insurance65p # Total number uninsured

# Education
County$inCollege <- 100*(County$undergrads + County$grads)/County$total_pop
County$hasDegree <- 100*(County$doctorate+County$professional+County$masters+County$bachelors)/County$total_pop
# The languages are calculated such that these are the "Percent of individuals 5 years and older who speak English less than well whose native tongue is ____________"
County$pct_spanish_speakers <- 100*(County$Spanish17noa + County$Spanish1864noa + County$Spanish65noa + County$Spanish17nw + County$Spanish1864nw + County$Spanish65nw)/County$total_pop #Spanish


# HousingCounty$no_vehicles <- 100*(County$no_vehicles_r + County$no_vehicles_o)/County$Total_Households # Sum renter and homeowner units with no vehicles available for the total residences with no vehicle
County$pct_poverty <- 100*County$poverty/County$Total_Households
County$pct_publicAss <- 100*County$public_ass/County$total_pop
County$unemployment <- 100*County$unemployed/County$laborforce
County$LFPR <- 100*County$laborforce/(County$total_pop-County$children)
County$pct_rent <- 100*County$renter_occ/County$total_occupied
County$pct_own <- 100*County$owner_occ/County$total_occupied

#~~~~~~~~~~~~~~~~
# State Variables
#~~~~~~~~~~~~~~~~
# Demographic
State$minority <- State$total_pop - State$white + State$white_hisplat
State$pct_minority <- 100*State$minority/State$total_pop
State$pct_hisplat <- 100*State$hisplat/State$total_pop
State$pct_whitehisplat <- 100*State$white_hisplat/State$total_pop
State$pct_white <- 100*State$white/State$total_pop
State$pct_black <- 100*State$black/State$total_pop
State$pct_native <- 100*State$native/State$total_pop
State$pct_asian <- 100*State$asian/State$total_pop
State$pct_hipi <- 100*State$hipi/State$total_pop
State$pct_additional <- 100*State$additional/State$total_pop
State$pct_twoplus <- 100*State$twoplus/State$total_pop
State$pct_kids <- 100*State$children/State$total_pop
State$pct_disability <- 100*(State$SNAP_Disability + State$noSnap_Disability)/State$total_pop # number of households with at least one member with a disability
State$SingleParent <- State$Single_Dad + State$Single_Mom # Number of Single parent households

#Economic 
State$No_Health_Insurance <-  State$No_Health_Insuranceu19 +  State$No_Health_Insurance1935 + State$No_Health_Insurance3665 +  State$No_Health_Insurance65p # Total number uninsured

# Education
State$inCollege <- 100*(State$undergrads + State$grads)/State$total_pop
State$hasDegree <- 100*(State$doctorate+State$professional+State$masters+State$bachelors)/State$total_pop
# The languages are calculated such that these are the "Percent of individuals 5 years and older who speak English less than well whose native tongue is ____________"
State$pct_spanish_speakers <- 100*(State$Spanish17noa + State$Spanish1864noa + State$Spanish65noa + State$Spanish17nw + State$Spanish1864nw + State$Spanish65nw)/State$total_pop #Spanish


# HousingState$no_vehicles <- 100*(State$no_vehicles_r + State$no_vehicles_o)/State$Total_Households # Sum renter and homeowner units with no vehicles available for the total residences with no vehicle
State$pct_poverty <- 100*State$poverty/State$Total_Households
State$pct_publicAss <- 100*State$public_ass/State$total_pop
State$unemployment <- 100*State$unemployed/State$laborforce
State$LFPR <- 100*State$laborforce/(State$total_pop-State$children)
State$pct_rent <- 100*State$renter_occ/State$total_occupied
State$pct_own <- 100*State$owner_occ/State$total_occupied


#~~~~~~~~~~~~~~~~
# US Variables
#~~~~~~~~~~~~~~~~
# Demographic
Country$minority <- Country$total_pop - Country$white + Country$white_hisplat
Country$pct_minority <- 100*Country$minority/Country$total_pop
Country$pct_hisplat <- 100*Country$hisplat/Country$total_pop
Country$pct_whitehisplat <- 100*Country$white_hisplat/Country$total_pop
Country$pct_white <- 100*Country$white/Country$total_pop
Country$pct_black <- 100*Country$black/Country$total_pop
Country$pct_native <- 100*Country$native/Country$total_pop
Country$pct_asian <- 100*Country$asian/Country$total_pop
Country$pct_hipi <- 100*Country$hipi/Country$total_pop
Country$pct_additional <- 100*Country$additional/Country$total_pop
Country$pct_twoplus <- 100*Country$twoplus/Country$total_pop
Country$pct_kids <- 100*Country$children/Country$total_pop
Country$pct_disability <- 100*(Country$SNAP_Disability + Country$noSnap_Disability)/Country$total_pop # number of households with at least one member with a disability
Country$SingleParent <- Country$Single_Dad + Country$Single_Mom # Number of Single parent households

#Economic 
Country$No_Health_Insurance <-  Country$No_Health_Insuranceu19 +  Country$No_Health_Insurance1935 + Country$No_Health_Insurance3665 +  Country$No_Health_Insurance65p # Total number uninsured

# Education
Country$inCollege <- 100*(Country$undergrads + Country$grads)/Country$total_pop
Country$hasDegree <- 100*(Country$doctorate+Country$professional+Country$masters+Country$bachelors)/Country$total_pop
# The languages are calculated such that these are the "Percent of individuals 5 years and older who speak English less than well whose native tongue is ____________"
Country$pct_spanish_speakers <- 100*(Country$Spanish17noa + Country$Spanish1864noa + Country$Spanish65noa + Country$Spanish17nw + Country$Spanish1864nw + Country$Spanish65nw)/Country$total_pop #Spanish


# HousingCountry$no_vehicles <- 100*(Country$no_vehicles_r + Country$no_vehicles_o)/Country$Total_Households # Sum renter and homeowner units with no vehicles available for the total residences with no vehicle
Country$pct_poverty <- 100*Country$poverty/Country$Total_Households
Country$pct_publicAss <- 100*Country$public_ass/Country$total_pop
Country$unemployment <- 100*Country$unemployed/Country$laborforce
Country$LFPR <- 100*Country$laborforce/(Country$total_pop-Country$children)
Country$pct_rent <- 100*Country$renter_occ/Country$total_occupied
Country$pct_own <- 100*Country$owner_occ/Country$total_occupied

# If only 2020 is necessary
County20 <- County[County$year == 2020,]
State20 <- State[State$year == 2020,]
US20 <- Country[Country$year == 2020,]




########################################################
# Graphics - City/District
########################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Demographic Graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# STATIC CHARTS
#~~~~~~~~~~~~~~


# DYNAMIC CHARTS
#~~~~~~~~~~~~~~
### Change in population over time ###
ggplot(Population, aes(x = year, y=total_pop)) +
  geom_line( color = "#00477F", size = 1.5) + 
  scale_y_continuous(limits = c(100000,200000)) +
  scale_x_continuous("Year", labels = c("2013","2014","2015","2016","2017","2018","2019","2020","2021"), breaks = c(2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  ggtitle("Change in Population") + 
  xlab("") + 
  ylab("Population")+
  theme(text=element_text(size=16,  family="Arial"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )

ggplot(District, aes(x = year, y=total_pop)) +
  geom_line( color = "#00477F", size = 1.5) + 
  scale_y_continuous(limits = c(0,35000)) +
  scale_x_continuous("Year", labels = c("2013","2015","2017","2019","2021"), breaks = c(2013,2015,2017,2019,2021)) + 
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  ggtitle("Change in Population") + 
  xlab("") + 
  ylab("Population")+
  theme(text=element_text(size=16,  family="Arial"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Economic Graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# STATIC CHARTS
#~~~~~~~~~~~~~~
### Population by poverty status ###
poverty <- subset(Districts20, select = c(DIST, total_pop, poverty))
poverty$nonPoverty <- poverty$total_pop - poverty$poverty
data_long <- gather(poverty, "status", "total",3:4)

ggplot(data_long, aes(x = DIST, y = total, fill = status)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab('District') +
  ylab('Population') +
  ggtitle( "Population Below Poverty Line by District") +
  scale_fill_discrete(labels=c('Pop. Above Poverty Line', 'Pop. Below Poverty Line'))

rm(data_long, poverty)

# DYNAMIC CHARTS
#~~~~~~~~~~~~~~

### Poverty over time ###
# District-level
ggplot() + 
  geom_line(data = District, aes(x = year, y = pct_poverty), color = "#00477F", size = 1.5) +
  scale_y_continuous(limits = c(0,35)) +
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  xlab('Year') +
  ylab('Percentage') +
  ggtitle( "Poverty Over Time")+
  theme_minimal()

# City-level
ggplot() + 
  geom_line(data = Population, aes(x = year, y = pct_poverty), color = "red") +
  scale_y_continuous(limits = c(0,20)) +
  xlab('Year') +
  ylab('Percentage') +
  ggtitle( "Poverty Over Time")


# Labor Force Participation Rate


### Unemployment over Time ###

# District-Level
ggplot() + 
  geom_line(data = District, aes(x = year, y = unemployment), color = "#00477F", size = 1.5) +
  geom_line(data = District, aes(x = year, y = LFPR), color = "blue") +
  scale_y_continuous(limits = c(0,15)) +
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  xlab('Year') +
  ylab('Rate') +
  ggtitle( "Unemployment Over Time")+
  theme_minimal()

# City-level
ggplot() + 
  geom_line(data = Population, aes(x = year, y = unemployment), color = "#00477F", size = 1.5) +
  scale_y_continuous(limits = c(0,10)) +
  xlab('Year') +
  ylab('Rate') +
  ggtitle( "Unemployment Over Time")+
  theme_minimal()


### Labor Force Participation Rate over Time ###
# District-Level
ggplot() + 
  geom_line(data = District, aes(x = year, y = LFPR), color = "blue") +
  scale_y_continuous(limits = c(60,80)) +
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  xlab('Year') +
  ylab('Rate') +
  ggtitle( "Unemployment Over Time")

# City-Level
ggplot() + 
  geom_line(data = Population, aes(x = year, y = LFPR), color = "blue") +
  scale_y_continuous(limits = c(60,80)) +
  xlab('Year') +
  ylab('Rate') +
  ggtitle( "Unemployment Over Time")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Education Graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# STATIC CHARTS
#~~~~~~~~~~~~~~
### Population by enrollment in College ###
college <- subset(Districts20, select = c(DIST, total_pop, undergrads, grads))
college$student <- college$grads + college$undergrads
college$notStudent <- college$total_pop- college$grads - college$undergrads
data_long <- gather(college, "status", "total",5:6)

ggplot(data_long, aes(x = DIST, y = total, fill = status)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab('District') +
  ylab('Population') +
  ggtitle( "Student Population by District") +
  scale_fill_discrete(labels=c('Non-Student Population', 'Enrolled in College'))
rm(data_long, college)

# Population by Degree from College
college <- subset(Districts20, select = c(DIST, doctorate, professional, masters,bachelors, total_pop))
college$degree <- college$bachelors + college$masters+ college$professional+ college$doctorate
college$nondegree <- college$total_pop- college$degree
data_long <- gather(college, "status", "total",7:8)

ggplot(data_long, aes(x = DIST, y = total, fill = status)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab('District') +
  ylab('Population') +
  ggtitle( "Population with College Degree by District") +
  scale_fill_discrete(labels=c('Degree Holding', 'Non Degree-Holding'))

rm(data_long, college)

# DYNAMIC CHARTS
#~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Housing Graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# STATIC CHARTS
#~~~~~~~~~~~~~~

### Amount of Housing by Tenure ###
# District-level
tenure <- subset(Districts20, select = c(DIST, owner_occ, renter_occ))
data_long <- gather(tenure, "tenure", "total", 2:3)

ggplot(data_long, aes(x = DIST, y = total, fill = tenure)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab('District') +
  ylab('Number of Housing Units') +
  ggtitle( "Housing Tenure by District")+
  scale_fill_discrete(labels=c('Owner Occupied', 'Renter Occupied'))

rm(tenure, data_long)


# DYNAMIC CHARTS
#~~~~~~~~~~~~~~

### Housing Cost vs. Income over time ### 
# City-level
Population$rent_adj <- Population$med_gross_rent*10
ggplot() + 
  geom_line(data = Population, aes(x = year, y = medIncome), color = "#00477F", size = 1.5) +
  geom_line(data = Population, aes(x = year, y = med_home_val), color = "#549FD3", size = 1.5) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3), limits = c(0,510000)) +
  scale_x_continuous("Year", labels = c("2013","2014","2015","2016","2017","2018","2019","2020","2021"), breaks = c(2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  xlab('Year') +
  ylab('Housing Cost ($)') +
  ggtitle( "Housing Cost vs. Income Over Time")+
  theme(text=element_text(size=16,  family="Arial"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )

# District-level
District$rent_adj <- District$med_gross_rent*10
ggplot() + 
  geom_line(data = District, aes(x = year, y = medIncome), color = "#00477F", size = 1.5) +
  geom_line(data = District, aes(x = year, y = med_home_val), color = "#549FD3", size = 1.5) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3), limits = c(0,510000)) +
  scale_x_continuous("Year", labels = c("2013","2015","2017","2019","2021"), breaks = c(2013,2015,2017,2019,2021)) + 
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  xlab('Year') +
  ylab('Housing Cost ($)') +
  ggtitle( "Housing Cost vs. Income Over Time")+
  theme(text=element_text(size=16,  family="Arial"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )



### Rental Cost vs. Income over time ### 
# City-level
Population$rent_adj <- Population$med_gross_rent*12
ggplot() + 
  geom_line(data = Population, aes(x = year, y = medIncome), color = "#00477F", size = 1.5) +
  geom_line(data = Population, aes(x = year, y = rent_adj), color = "#549FD3", size = 1.5) +
  scale_y_continuous(limits = c(0,75000)) +
  scale_x_continuous("Year", labels = c("2013","2014","2015","2016","2017","2018","2019","2020","2021"), breaks = c(2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  xlab('Year') +
  ylab('Dollars') +
  ggtitle( "Yearly Rental Rates vs. Income Over Time")+
  theme(text=element_text(size=16,  family="Arial"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )

# District-level
District$rent_adj <- District$med_gross_rent*10
ggplot() + 
  geom_line(data = District, aes(x = year, y = medIncome), color = "#00477F", size = 1.5) +
  geom_line(data = District, aes(x = year, y = rent_adj), color = "#549FD3", size = 1.5) +
  scale_y_continuous( limits = c(0,150000)) +
  scale_x_continuous("Year", labels = c("2013","2015","2017","2019","2021"), breaks = c(2013,2015,2017,2019,2021)) + 
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  xlab('Year') +
  ylab("Dollars") +
  ggtitle( "Yearly Rental Rates vs. Icome Over Time")+
  theme(text=element_text(size=16,  family="Arial"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_line(colour = "#E9EFEF"),
        panel.grid.minor = element_line(colour = "#E9EFEF"),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")
  )
####



########################################################
# Graphics - County/State
########################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Demographic Graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# STATIC CHARTS
#~~~~~~~~~~~~~~

# DYNAMIC CHARTS
#~~~~~~~~~~~~~~
### Change in population over time ###
ggplot(State, aes(x = year, y=total_pop)) +
  geom_line() + 
  scale_y_continuous(limits = c(100000,6000000)) + 
  ggtitle("Change in Population by Council District") + 
  xlab("") + 
  ylab("Population")

### Change in population over time ###
ggplot(County, aes(x = year, y=total_pop)) +
  geom_line() + 
  scale_y_continuous(limits = c(100000,500000)) + 
  ggtitle("Change in Population by Council District") + 
  xlab("") + 
  ylab("Population")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Economic Graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# STATIC CHARTS
#~~~~~~~~~~~~~~
### Population by poverty status ###
poverty <- subset(Districts20, select = c(DIST, total_pop, poverty))
poverty$nonPoverty <- poverty$total_pop - poverty$poverty
data_long <- gather(poverty, "status", "total",3:4)

ggplot(data_long, aes(x = DIST, y = total, fill = status)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab('District') +
  ylab('Population') +
  ggtitle( "Population Below Poverty Line by District") +
  scale_fill_discrete(labels=c('Pop. Above Poverty Line', 'Pop. Below Poverty Line'))

rm(data_long, poverty)

# DYNAMIC CHARTS
#~~~~~~~~~~~~~~

### Poverty over time ###
# District-level
ggplot() + 
  geom_line(data = District, aes(x = year, y = pct_poverty), color = "red") +
  scale_y_continuous(limits = c(0,35)) +
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  xlab('Year') +
  ylab('Percentage') +
  ggtitle( "Poverty Over Time")

# City-level
ggplot() + 
  geom_line(data = Population, aes(x = year, y = pct_poverty), color = "red") +
  scale_y_continuous(limits = c(0,20)) +
  xlab('Year') +
  ylab('Percentage') +
  ggtitle( "Poverty Over Time")


# Labor Force Participation Rate


### Unemployment over Time ###

# District-Level
ggplot() + 
  geom_line(data = District, aes(x = year, y = unemployment), color = "red") +
  geom_line(data = District, aes(x = year, y = LFPR), color = "blue") +
  scale_y_continuous(limits = c(0,15)) +
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  xlab('Year') +
  ylab('Rate') +
  ggtitle( "Unemployment Over Time")

# City-level
ggplot() + 
  geom_line(data = Population, aes(x = year, y = unemployment), color = "red") +
  scale_y_continuous(limits = c(0,10)) +
  xlab('Year') +
  ylab('Rate') +
  ggtitle( "Unemployment Over Time")


### Labor Force Participation Rate over Time ###
# District-Level
ggplot() + 
  geom_line(data = District, aes(x = year, y = LFPR), color = "blue") +
  scale_y_continuous(limits = c(60,80)) +
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  xlab('Year') +
  ylab('Rate') +
  ggtitle( "Unemployment Over Time")

# City-Level
ggplot() + 
  geom_line(data = Population, aes(x = year, y = LFPR), color = "blue") +
  scale_y_continuous(limits = c(60,80)) +
  xlab('Year') +
  ylab('Rate') +
  ggtitle( "Unemployment Over Time")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Education Graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# STATIC CHARTS
#~~~~~~~~~~~~~~
### Population by enrollment in College ###
college <- subset(Districts20, select = c(DIST, total_pop, undergrads, grads))
college$student <- college$grads + college$undergrads
college$notStudent <- college$total_pop- college$grads - college$undergrads
data_long <- gather(college, "status", "total",5:6)

ggplot(data_long, aes(x = DIST, y = total, fill = status)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab('District') +
  ylab('Population') +
  ggtitle( "Student Population by District") +
  scale_fill_discrete(labels=c('Non-Student Population', 'Enrolled in College'))
rm(data_long, college)

# Population by Degree from College
college <- subset(Districts20, select = c(DIST, doctorate, professional, masters,bachelors, total_pop))
college$degree <- college$bachelors + college$masters+ college$professional+ college$doctorate
college$nondegree <- college$total_pop- college$degree
data_long <- gather(college, "status", "total",7:8)

ggplot(data_long, aes(x = DIST, y = total, fill = status)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab('District') +
  ylab('Population') +
  ggtitle( "Population with College Degree by District") +
  scale_fill_discrete(labels=c('Degree Holding', 'Non Degree-Holding'))

rm(data_long, college)

# DYNAMIC CHARTS
#~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Housing Graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# STATIC CHARTS
#~~~~~~~~~~~~~~

### Amount of Housing by Tenure ###
# District-level
tenure <- subset(Districts20, select = c(DIST, owner_occ, renter_occ))
data_long <- gather(tenure, "tenure", "total", 2:3)

ggplot(data_long, aes(x = DIST, y = total, fill = tenure)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab('District') +
  ylab('Number of Housing Units') +
  ggtitle( "Housing Tenure by District")+
  scale_fill_discrete(labels=c('Owner Occupied', 'Renter Occupied'))

rm(tenure, data_long)


# DYNAMIC CHARTS
#~~~~~~~~~~~~~~

### Housing Cost vs. Income over time ### 
# City-level
Population$rent_adj <- Population$med_gross_rent*10
ggplot() + 
  geom_line(data = Population, aes(x = year, y = medIncome), color = "red") +
  geom_line(data = Population, aes(x = year, y = med_home_val), color = "blue") +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3), limits = c(0,510000)) +
  xlab('Year') +
  ylab('Housing Cost ($)') +
  ggtitle( "Housing Cost vs. Income Over Time")

# District-level
District$rent_adj <- District$med_gross_rent*10
ggplot() + 
  geom_line(data = District, aes(x = year, y = medIncome), color = "red") +
  geom_line(data = District, aes(x = year, y = med_home_val), color = "blue") +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3), limits = c(0,510000)) +
  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) +
  xlab('Year') +
  ylab('Housing Cost ($)') +
  ggtitle( "Housing Cost vs. Income Over Time")

####






County20$med_home_val
State20$med_home_val
County20$med_gross_rent
State20$med_gross_rent
County20$med_owner_costs_pctOfIncome
State20$med_owner_costs_pctOfIncome
County20$med_rent_asPctofInc
State20$med_rent_asPctofInc
County20$Total_Housing_Units
State20$Total_Housing_Units
County20$Total_Households
State20$Total_Households