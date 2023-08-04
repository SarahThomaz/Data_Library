install.packages("tidycensus")
remotes::install_github("walkerke/tidycensus")
install.packages("forcats")
library(forcats)
library(tidycensus)
library(tidyverse)
library(readxl)

dec <- load_variables(year = 2020, dataset = "dhc",cache = TRUE)
results <- dec[grep(x = dec$label, "ouse"), c("name", "label", "concept")]


total_population_10 <- get_decennial( geography = "block",
                                      state = "08",
                                      county = "069",
                                      output = "wide",
                                      year = 2020,
                                      sumfile = "dhc",
                                      variables = c(  #Demographic Variables
                                                      Total_Pop = "P1_001N",
                                                      Median_Age = "P13_001N",
                                                      Children = "P15_002N",
                                                      
                                                      HispLat = "P4_003N",
                                                      White = "P3_002N",
                                                      White_HispLat = "P5_011N",
                                                      Black = "P3_003N",
                                                      AIAN = "P3_004N",
                                                      Asian = "P3_005N",
                                                      NHPI = "P3_006N",
                                                      Additional = "P3_007N",
                                                      TwoPlusRace = "P3_008N",
                                                      
                                                      #Housing Variables
                                                      Renter_occ = "H4_004N",
                                                      Owner_noMort = "H4_003N",
                                                      Owner_Mort = "H4_002N", 
                                                      Households = "H9_001N",
                                                      HH_1 = "H9_002N",
                                                      HH_2 = "H9_003N",
                                                      HH_3 = "H9_004N",
                                                      HH_4 = "H9_005N",
                                                      HH_5 = "H9_006N",
                                                      HH_6 = "H9_007N",
                                                      HH_7 = "H9_008N", 
                                                      HH_owner = "H12_002N",
                                                      HH_1o =  "H12_003N",
                                                      HH_2o = "H12_004N",
                                                      HH_3o = "H12_005N",
                                                      HH_4o = "H12_006N",
                                                      HH_5o = "H12_007N",
                                                      HH_6o = "H12_008N",
                                                      HH_7o = "H12_009N",
                                                      HH_renter = "H12_010N",
                                                      HH_1r = "H12_011N",
                                                      HH_2r = "H12_012N",
                                                      HH_3r = "H12_013N",
                                                      HH_4r = "H12_014N",
                                                      HH_5r = "H12_015N",
                                                      HH_6r = "H12_016N",
                                                      HH_7r = "H12_017N"
                                                      
                                                      
                                                      #Economic Variables
                                                      
                                                      )
                                    
                                    )


BlockDist <- read_excel("~/ArcGIS/Projects/Dashboard/BlocksByDistrict.xls")
BlockDist <- BlockDist %>%  rename( "GEOID" = "GEOID20")
BlockDist <- subset(BlockDist, select = c(DIST, GEOID))
Pop <- left_join(BlockDist, total_population_10, "GEOID")
Pop <- subset(Pop, select = -c(NAME))

population <- Pop %>% summarize(total_pop = sum(Total_Pop, na.rm = TRUE))
#Report Numbers at the City level
City <- Pop%>%
  group_by(DIST) %>%
  summarize(total_pop = sum(Total_Pop, na.rm = TRUE),
            median_age = median(Median_Age, na.rm = TRUE),
            children = sum(Children, na.rm = TRUE),
            # Racial Mix
            white = sum(White, na.rm = TRUE),
            white_hisplat = sum(White_HispLat, na.rm = TRUE),
            hisplat = sum( HispLat, na.rm = TRUE),
            black = sum(Black, na.rm = TRUE),
            native = sum(AIAN, na.rm = TRUE),
            asian = sum(Asian, na.rm= TRUE),
            hipi = sum(NHPI, na.rm= TRUE),
            additional = sum(Additional, na.rm = TRUE),
            twoplus = sum(TwoPlusRace, na.rm = TRUE),
            
            # Household
            renter = sum(Renter_occ, na.rm = TRUE),
            onwer_nomort = sum(Owner_noMort, na.rm = TRUE),
            owner_mort = sum(Owner_Mort, na.rm = TRUE),
            households = sum(Households, na.rm = TRUE),
            hh_1 = sum(HH_1, na.rm = TRUE),
            hh_2 = sum(HH_2, na.rm = TRUE),
            hh_3 = sum(HH_3, na.rm = TRUE),
            hh_4 = sum(HH_4, na.rm = TRUE),
            hh_5 = sum(HH_5, na.rm = TRUE),
            hh_6 = sum(HH_6, na.rm = TRUE),
            hh_7 = sum(HH_7, na.rm = TRUE),
            hh_1o = sum(HH_1o, na.rm = TRUE),
            hh_2o = sum(HH_2o, na.rm = TRUE),
            hh_3o = sum(HH_3o, na.rm = TRUE),
            hh_4o = sum(HH_4o, na.rm = TRUE),
            hh_5o = sum(HH_5o, na.rm = TRUE),
            hh_6o = sum(HH_6o, na.rm = TRUE),
            hh_7o = sum(HH_7o, na.rm = TRUE),
            hh_1r = sum(HH_1r, na.rm = TRUE),
            hh_2r = sum(HH_2r, na.rm = TRUE),
            hh_3r = sum(HH_3r, na.rm = TRUE),
            hh_4r = sum(HH_4r, na.rm = TRUE),
            hh_5r = sum(HH_5r, na.rm = TRUE),
            hh_6r = sum(HH_6r, na.rm = TRUE),
            hh_7r = sum(HH_7r, na.rm = TRUE)
  )


# Racial/Ethnic Mix Variables
City$minority <- City$total_pop - City$white + City$white_hisplat
City$pct_minority <- 100*City$minority/City$total_pop
City$pct_hisplat <- 100*City$hisplat/City$total_pop
City$pct_whitehisplat <- 100*City$white_hisplat/City$total_pop
City$pct_white <- 100*City$white/City$total_pop
City$pct_black <- 100*City$black/City$total_pop
City$pct_native <- 100*City$native/City$total_pop
City$pct_asian <- 100*City$asian/City$total_pop
City$pct_hipi <- 100*City$hipi/City$total_pop
City$pct_additional <- 100*City$additional/City$total_pop
City$pct_twoplus <- 100*City$twoplus/City$total_pop

# Housing Variables
City$pct_renter <- 100*City$renter/City$total_pop
City$pct_owner <- 100*City$white/City$total_pop
City$pct_hh1 <- 100*City$hh_1/City$households
City$pct_hh2 <- 100*City$hh_2/City$households
City$pct_hh3 <- 100*City$hh_3/City$households
City$pct_hh4 <- 100*City$hh_4/City$households
City$pct_hh5 <- 100*City$hh_5/City$households
City$pct_hh6 <- 100*City$hh_6/City$households
City$pct_hh7 <- 100*City$hh_7/City$households

# Social/Demographic
City$pct_children <- 100*City$children/City$total_pop
City <- City[City$DIST != 0,]
#Charts and Tables

City$total_pop
view(subset(City, select = c(DIST, pct_minority, pct_hisplat, median_age)))
BlockDist %>% count(DIST)
#Racial Mix Bar Chart

# Modify them like this.
rm(data_long)
City <- City[!is.na(City$DIST),]
race <- subset(Population20, select = c( pct_minority, pct_hisplat, pct_white, pct_black, pct_native, pct_asian, pct_hipi, pct_additional, pct_twoplus))
data_long <- gather(race, "race", "percentage", 1:9)
data_long$percentage <- as.double(data_long$percentage)
data_long$percentage <- format(round(data_long$percentage, 0), nsmall = 0)
data_long$race <- fct_recode(data_long$race,
                             "Minority" = "pct_minority",
                             "Hispanic/Latino" = "pct_hisplat",
                             "White" = "pct_white",
                             "Black" = "pct_black",
                             "Native/Indigenous" = "pct_native",
                             "Asian" = "pct_asian",
                             "Hawaiian/Pac. Is." = "pct_hipi",
                             "Additional Races" = "pct_additional",
                             "Two or More Races" = "pct_twoplus"
                             )
data_long$label_pct <- as.character(data_long$percentage)
data_long$label_pct <- paste(data_long$label_pct, "%", sep = "")



ggplot(data = data_long, aes(y = percentage, x = fct_reorder(race, percentage), width=0.6)) +
  geom_bar(position="stack",stat = 'identity', fill="#00467F") + scale_color_discrete(labels = labels)+coord_flip()+
  geom_text(aes(label = label_pct), hjust = -0.02)+
  ggtitle("Racial/Ethnic Mix in Fort Collins") + 
  xlab("") + 
  ylab("Percentage")

  facet_wrap(~DIST, labeller = labeller(DIST = 
                                          c("1" = "District 1",
                                            "2" = "District 2",
                                            "3" = "District 3",
                                            "4" = "District 4",
                                            "5" = "District 5",
                                            "6" = "District 6" ))) 

ggplot(r2, aes(fct_rev(fct_infreq(Gender)), fill = Gender))
# Renter vs owner 


# Household by size
hhs <- subset(City, select = c(pct_hh1, pct_hh2, pct_hh3, pct_hh4, pct_hh5, pct_hh6, pct_hh7))
hhs_long <- gather(hhs, "hh_size", "percentage", 1:7)

ggplot(hhs_long, aes(x = percentage, y = hh_size)) + 
  geom_col()



poverty <- subset(Districts20, select = c(DIST, total_pop, poverty))
poverty$nonPoverty <- poverty$total_pop - poverty$poverty
data_long <- gather(poverty, "status", "total",3:4)

ggplot(City, aes( x = DIST, y = total_pop, fill =)) +
  geom_bar(position = "stack", stat = "identity") 
rm(data_long, poverty)


