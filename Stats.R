library(ggplot2)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(naniar)

######PUMS Data #####

#Import the Data by year
pums09 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_09.csv')
pums10 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_10.csv')
pums12 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_12.csv')
pums13 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_13.csv')
pums14 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_14.csv')
pums15 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_15.csv')
pums16 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_16.csv')
pums17 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_17.csv')
pums18 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_18.csv')
pums19 = read.csv('C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/pums_pih_19.csv')

#add a "year" column
pums19$year = rep("2019", length(pums19$prog))
pums18$year = rep("2018", length(pums18$prog))
pums17$year = rep("2017", length(pums17$prog))
pums16$year = rep("2016", length(pums16$prog))
pums15$year = rep("2015", length(pums15$prog))
pums14$year = rep("2014", length(pums14$prog))
pums13$year = rep("2013", length(pums13$prog))
pums12$year = rep("2012", length(pums12$prog))
pums10$year = rep("2010", length(pums10$prog))
pums09$year = rep("2009", length(pums09$prog))

#Combine the data into a singe data frame
HCV_df = as.data.frame(
  rbind(
  as.matrix(pums09),
  as.matrix(pums10),
  as.matrix(pums12),
  as.matrix(pums13),
  as.matrix(pums14),
  as.matrix(pums15),
  as.matrix(pums16),
  as.matrix(pums17),
  as.matrix(pums18),
  as.matrix(pums19)
  ),
  colnames(pums09)
)

#Align data for poverty column.Strings were entered in slightly different formats (extra spaces).
HCV_df$poverty = replace(HCV_df$poverty, which(HCV_df$poverty == "0% - 9%"), "0%-9%")
HCV_df$poverty = replace(HCV_df$poverty, which(HCV_df$poverty == "10% - 19%"), "10%-19%")
HCV_df$poverty = replace(HCV_df$poverty, which(HCV_df$poverty == "20% - 29%"), "20%-29%")
HCV_df$poverty = replace(HCV_df$poverty, which(HCV_df$poverty == "30% - 39%"), "30%-39%")
HCV_df$poverty = replace(HCV_df$poverty, which(HCV_df$poverty == "40%  and above"), "40% and above")

#Remove rows without poverty level outcome.
HCV_df = subset(HCV_df, poverty != "")

#Only keep data that deals with HCVs.
HCV_df = subset(HCV_df, prog == "V")

#Washington and Oregon
WA_df = subset(HCV_df, state == "Washington")
OR_df = subset(HCV_df, state == "Oregon")

table(subset(HCV_df, State = "Washington")$poverty)
prop.table(table(HCV_df$poverty))

pov_lev = c("0 - 10%", "11 - 19%", "20 - 29%", "30 - 39%", "40% +")
pov_col = c("light blue", "blue", "purple", "red", "dark red")

#Importing FIPS codes

#fips_df = read.csv("C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/Housing_Choice_Vouchers_by_Tract.csv")

######Subsidized Household Data MO - WY#####

subh12 = read.csv("C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/TRACT_MO_WY_2012.csv")
subh13 = read.csv("C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/TRACT_MO_WY_2013.csv")
subh14 = read.csv("C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/TRACT_MO_WY_2014.csv")
subh15 = read.csv("C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/TRACT_MO_WY_2015.csv")
subh16 = read.csv("C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/TRACT_MO_WY_2016.csv")
subh17 = read.csv("C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/TRACT_MO_WY_2017.csv")
subh18 = read.csv("C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/TRACT_MO_WY_2018.csv")
subh19 = read.csv("C:/Users/David/Documents/Econ - Public Finance/Project Proposal/Data Sets/TRACT_MO_WY_2019.csv")

#####Consolidating Mo - WY Data #####
#Variables are not consistent across all years. We will first need to identify differences in variable names, that transform the data so that these dataframes can be linked.

#Set operations to see what elements are different

#12 and 13 each have 66 columns.Now, we see if the same is true for their intersection.
intersect(colnames(subh12), colnames(subh13))
#We see that the number of columns stay the same, meaning that column names are uniform across these two dataframes.

#14, 15, 16, 17, 18

length(intersect(colnames(subh13), colnames(subh14)))
length(intersect(colnames(subh14), colnames(subh15)))
length(intersect(colnames(subh15), colnames(subh16)))
length(intersect(colnames(subh16), colnames(subh17)))
length(intersect(colnames(subh17), colnames(subh18)))
length(intersect(colnames(subh18), colnames(subh19)))

#Let's go in descending order. Reconcile 15 and 16

#We look at the differences between 15 and 16. Note that the operation setdiff() is not symmetric.
setdiff(colnames(subh15),intersect(colnames(subh15),colnames(subh16)) )
setdiff(colnames(subh16),intersect(colnames(subh15),colnames(subh16)) )

#Change Names
subh15 = rename(subh15, state = UNIT_STATE_CD)

#Check Class
class(subh15$state)
class(subh16$state)

#check if values are input in compatible manners.
unique(subh15$state)
unique(subh16$state)

##subh15$state = replace(subh15$state, replace = list(x = "XX"))

#15 and 14

setdiff(colnames(subh14),intersect(colnames(subh14),colnames(subh15)) )
setdiff(colnames(subh15),intersect(colnames(subh14),colnames(subh15)) )

#15 (and 16 - 19) have the value "Quarter" not present in 14. Will define a variable to keep track.

rem_vec = "Quarter"

#Now, 13 and 14

setdiff(colnames(subh13),intersect(colnames(subh13),colnames(subh14)) )
setdiff(colnames(subh14),intersect(colnames(subh13),colnames(subh14)) )

#Let's change labels that have cosmetic differences.
subh13 = rename(subh13, name = NAME)
subh13 = rename(subh13, state = STATE)
subh13 = rename(subh13, place = PLACE)
subh13 = rename(subh13, cbsa = CBSA)
subh13 = rename(subh13, pct_black_nonhsp = pct_black)
subh13 = rename(subh13, pct_native_american_nonhsp = pct_native_american)
subh13 = rename(subh13, pct_asian_pacific_nonhsp= pct_asian_pacific)


#add the vectors that don't match to rem_vec
rem_vec = c(rem_vec,
            "pct_white_nothsp",
            "pct_black_hsp", 
            "pct_wht_hsp", 
            "pct_oth_hsp", 
            "pct_multi")

#12 and 13

setdiff(colnames(subh12),intersect(colnames(subh12),colnames(subh13)) )
setdiff(colnames(subh13),intersect(colnames(subh12),colnames(subh13)) )

#Update labels

subh12 = rename(subh12, name = NAME)
subh12 = rename(subh12, state = STATE)
subh12 = rename(subh12, place = PLACE)
subh12 = rename(subh12, cbsa = CBSA)
subh12 = rename(subh12, pct_black_nonhsp = pct_black)
subh12 = rename(subh12, pct_native_american_nonhsp = pct_native_american)
subh12 = rename(subh12, pct_asian_pacific_nonhsp= pct_asian_pacific)

#Now that all of our column names are  updated, we will remove those columns that do not correspond to all data frames.
subh14 = subh14[, setdiff(colnames(subh14),rem_vec)]
subh15 = subh15[, setdiff(colnames(subh15),rem_vec)]
subh16 = subh16[, setdiff(colnames(subh16),rem_vec)]
subh17 = subh17[, setdiff(colnames(subh17),rem_vec)]
subh18 = subh18[, setdiff(colnames(subh18),rem_vec)]
subh19 = subh19[, setdiff(colnames(subh19),rem_vec)]

#Add a year variable to each data frame.
subh12$year = rep("2012", length(subh12$gsl))
subh13$year = rep("2013", length(subh13$gsl))
subh14$year = rep("2014", length(subh14$gsl))
subh15$year = rep("2015", length(subh15$gsl))
subh16$year = rep("2016", length(subh16$gsl))
subh17$year = rep("2017", length(subh17$gsl))
subh18$year = rep("2018", length(subh18$gsl))
subh19$year = rep("2019", length(subh19$gsl))

#We will join the columns
subh_df = as.data.frame(
  rbind(subh12,
        subh13,
        subh14,
        subh15,
        subh16,
        subh17,
        subh18,
        subh19)
)

#####Subsetting and dropping incomplete records
#Limiting to HCV
subh_df = subset(subh_df, program_label == "Housing Choice Vouchers")

#We will need to clean the data of missing records for those columns that we wish to preserve.Our problem is that our data is aggregates at the census tract level. We have information on census tract poverty rates, % of minorities, % women, and %62 plus.We must also makes sure that we have raw population counts.

subh_df = subset(subh_df,
                 pct_minority >= 0 & 
                   pct_female_head >= 0 &
                   pct_age62plus >= 0 &
                   people_total)

#####Adding Census Tract Indicator Variables ####

#Placing census tract poverty rates into ranges.
subh_df$pov_range = rep(NA, length(subh_df$tpoverty))
subh_df$pov_range = replace(subh_df$pov_range, subh_df$tpoverty >= 0, "0 - 9")
subh_df$pov_range = replace(subh_df$pov_range, subh_df$tpoverty >= 10, "10 - 19")
subh_df$pov_range = replace(subh_df$pov_range, subh_df$tpoverty >= 20, "20 - 29")
subh_df$pov_range = replace(subh_df$pov_range, subh_df$tpoverty >= 30, "30 - 39")
subh_df$pov_range = replace(subh_df$pov_range, subh_df$tpoverty >= 40, "40+")

#Indicator variables for low poverty vs moderate-to-high poverty. We construct different measures of low-poverty, with varying cutoffs. First, we must construct a new variable column in the data frame, then replace values for those entries that fall within the range.
subh_df$lpov_09 = rep(NA, length(subh_df$tpoverty))
subh_df$lpov_09 = replace(subh_df$lpov_09, subh_df$tpoverty >= 0, 1)
subh_df$lpov_09 = replace(subh_df$lpov_09, subh_df$tpoverty >= 10, 0)

subh_df$lpov_11 = rep(NA, length(subh_df$tpoverty))
subh_df$lpov_11 = replace(subh_df$lpov_11, 
                          subh_df$tpoverty >= 0 & subh_df$tpoverty <= 11, 
                          1)
subh_df$lpov_11 = replace(subh_df$lpov_11, subh_df$tpoverty >= 12, 0)


subh_df$lpov_13 = rep(NA, length(subh_df$tpoverty))
subh_df$lpov_13 = replace(subh_df$lpov_13, 
                          subh_df$tpoverty >= 0 & subh_df$tpoverty <= 13, 
                          1)
subh_df$lpov_13 = replace(subh_df$lpov_13, subh_df$tpoverty >= 14, 0)


subh_df$lpov_15 = rep(NA, length(subh_df$tpoverty))
subh_df$lpov_15 = replace(subh_df$lpov_15, 
                          subh_df$tpoverty >= 0 & subh_df$tpoverty <= 15, 
                          1)
subh_df$lpov_15 = replace(subh_df$lpov_15, subh_df$tpoverty >= 16, 0)


#Adding a Post Variable
subh_df$post = rep(0, length(subh_df$gsl) )
subh_df$post = replace(subh_df$post, subh_df$year >= 2014 & subh_df$state == "OR", 1)
subh_df$post = replace(subh_df$post, subh_df$year >= 2018 & subh_df$state == "WA", 1)



##### Aggregating people across Census Tracts within States#####
#Oregon
st_yr_pop = rep(NA, length(subh_df$gsl)) #create vector
subh_df$st_yr_pop = st_yr_pop #add vector to dataframe as a variable

#replace values within the given state and year with state-level population, aggregated from the individual census tract entries. Note that some of the entries are coded as -5, -4, or -1 to represent various types of missing values.
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "OR" & subh_df$year == "2012" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2012" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "OR" & subh_df$year == "2013" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2013" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "OR" & subh_df$year == "2014" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2014" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "OR" & subh_df$year == "2015" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2015" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "OR" & subh_df$year == "2016" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2016" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "OR" & subh_df$year == "2017" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2017" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "OR" & subh_df$year == "2018" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2018" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "OR" & subh_df$year == "2019" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2019" & subh_df$people_total >= 0)$people_total))



#Washington

subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "WA" & subh_df$year == "2012" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "WA" & subh_df$year == "2012" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "WA" & subh_df$year == "2013" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "WA" & subh_df$year == "2013" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "WA" & subh_df$year == "2014" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "WA" & subh_df$year == "2014" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "WA" & subh_df$year == "2015" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "WA" & subh_df$year == "2015" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "WA" & subh_df$year == "2016" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "WA" & subh_df$year == "2016" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "WA" & subh_df$year == "2017" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "WA" & subh_df$year == "2017" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "WA" & subh_df$year == "2018" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "WA" & subh_df$year == "2018" & subh_df$people_total >= 0)$people_total))
subh_df$st_yr_pop = replace(subh_df$st_yr_pop,
                            subh_df$state == "WA" & subh_df$year == "2019" & subh_df$people_total >= 0,
                            sum(subset(subh_df,subh_df$state == "WA" & subh_df$year == "2019" & subh_df$people_total >= 0)$people_total))

#We will create a data frame to feed into ggplot so that we can graph voucher usage over time.

##### Aggregating people/tract/pov under 9% #####
st_yr_09 = rep(NA, length(subh_df$gsl)) #create vector
subh_df$st_yr_09 = st_yr_09 #add vector to dataframe as a variable

#set the amount to the total pop, switch those areas do that are low poverty to the low poverty agg.
subh_df$st_yr_09 = subh_df$st_yr_pop

#We need to have the entries that correspond to 0-9% aggregate to the amounts listed below. For those that do not, they will have the amounts listed below, minus the total.

subh_df$st_yr_09 = replace(subh_df$st_yr_09,
                            subh_df$state == "OR" & subh_df$year == "2012" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2012" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1)$people_total))

subh_df$st_yr_09 = replace(subh_df$st_yr_09,
                            subh_df$state == "OR" & subh_df$year == "2013" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2013" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1)$people_total))

subh_df$st_yr_09 = replace(subh_df$st_yr_09,
                            subh_df$state == "OR" & subh_df$year == "2014" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2014" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1)$people_total))

subh_df$st_yr_09 = replace(subh_df$st_yr_09,
                            subh_df$state == "OR" & subh_df$year == "2015" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2015" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1)$people_total))

subh_df$st_yr_09 = replace(subh_df$st_yr_09,
                            subh_df$state == "OR" & subh_df$year == "2016" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2016" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1)$people_total))

subh_df$st_yr_09 = replace(subh_df$st_yr_09,
                            subh_df$state == "OR" & subh_df$year == "2017" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2017" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1)$people_total))

subh_df$st_yr_09 = replace(subh_df$st_yr_09,
                            subh_df$state == "OR" & subh_df$year == "2018" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2018" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1)$people_total))

subh_df$st_yr_09 = replace(subh_df$st_yr_09,
                            subh_df$state == "OR" & subh_df$year == "2019" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1,
                            sum(subset(subh_df,subh_df$state == "OR" & subh_df$year == "2019" & subh_df$people_total >= 0 & subh_df$lpov_09 == 1)$people_total))

#Here, we set everything that does not correspond to lpov_09 to the total pop minus the lpov_09 amount



######State Subsets #####
subhOR_df = subset(subh_df, state == "OR")
subhWA_df = subset(subh_df, state == "WA")

#Tabulate counts for selected covariates. These will be plotted on a line graph
#Here, st_yr_09 gives the population for those areas in low poverty census tracts by year.
OR_WA_df = count(subh_df, st_yr_09, year, state, lpov_09)

#Remove everything that is not Orgeon or Washington
OR_WA_df = subset(OR_WA_df, OR_WA_df$state == "OR" | OR_WA_df$state == "WA")

#Now, we will transform the counts into a percentage (low poverty vs mod-high pov)
OR_WA_df2 = count(subh_df, st_yr_pop, year, state, lpov_09)
OR_WA_df2 = subset(OR_WA_df2, OR_WA_df2$state == "OR" | OR_WA_df2$state == "WA")


OR_Voucher_agg_df = subset(OR_WA_df, OR_WA_df$state == "OR")
WA_Voucher_agg_df = subset(OR_WA_df, OR_WA_df$state == "WA")


######Oregon Plots #####

#Voucher counts by year (ct agg)

ggplot(data = OR_Voucher_agg_df, aes(x = year, y = st_yr_09, group = lpov_09, color = lpov_09)) +
  geom_line() +
  ggtitle("Fig. 1: OR Voucher Usage by year (grouped by poverty range)")  +
  xlab("Year") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

yearly_countsOR = count(subset(HCV_df, state == "Oregon" & prog == "V"), poverty, year)
yearly_countsUS = count(subset(HCV_df, state != "Oregon" & prog == "V"), poverty, year)


ggplot(data = yearly_countsOR, aes(x = year, y = n, group = poverty, color = poverty)) +
  geom_line() +
  ggtitle("Fig. 1: OR Voucher Usage by year (grouped by poverty range)")  +
  xlab("Year") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(aes(xintercept = "2014"))

ggplot(data = yearly_countsUS, aes(x = year, y = n, group = poverty, color = poverty)) +
  geom_line() +
  ggtitle("Fig. 2: US Voucher Usage by year (grouped by poverty range)")  +
  xlab("Year") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(aes(xintercept = "2014"))

#Voucher spending by unit-month in the pre and post period.
ggplot(subset(subhOR_df, spending_per_month >= 0), aes(x=spending_per_month, color= post, group = post)) +
  geom_density()

#Percent move-in from last year in pre and post period.
ggplot(subset(subhOR_df, pct_movein >= 0), aes(x=pct_movein, color= post, group = post)) +
  geom_density()

ggplot(subset(subhOR_df, months_waiting >= 0), aes(x=months_waiting, color= post, group = post)) +
  geom_density()

#Number of people served
ggplot(subset(subhOR_df, people_total >= 0), aes(x=people_total, color= post, group = post)) +
  geom_density()

##By Povety level
#ggplot(OR_df, aes(year, fill = factor(poverty))) + 
#  geom_bar() +
#  ggtitle("Fig. 2a: OR Poverty Concentration by year") +
#  scale_fill_manual(values = pov_col, name = "Poverty Level", labels = pov_lev) +
#  xlab("Year") +
#  ylab("Count") +
#  theme(plot.title = element_text(hjust = 0.5))

###### Washington Plots #####
#Voucher counts by year (ct agg)
yearly_countsWA = count(subset(subhWA_df, pov_range), pov_range, year)

ggplot(data = yearly_countsWA, aes(x = year, y = n, group = pov_range, color = pov_range)) +
  geom_line() +
  ggtitle("Fig. 2: WA Voucher Usage by year (grouped by poverty range)")  +
  xlab("Year") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

#Voucher spending by unit-month in the pre and post period.
ggplot(subset(subhWA_df, spending_per_month >= 0 & spending_per_month < 2500), aes(x=spending_per_month, color= post, group = post)) +
  geom_density()

ggplot(subset(subhWA_df, pct_movein >= 0), aes(x=pct_movein, color= post, group = post)) +
  geom_density()


ggplot(subset(subhWA_df, months_waiting >= 0), aes(x=months_waiting, color= post, group = post)) +
  geom_density()

#Number of people served
ggplot(subset(subhWA_df, people_total >= 0), aes(x=people_total, color= post, group = post)) +
  geom_density()


######California Plots #####
ggplot(subset(HCV_df, state == "California"), aes(year, fill = factor(poverty))) + 
  geom_bar() +
  ggtitle("Fig. 3a: CA Poverty Concentration by year") +
  scale_fill_manual(values = pov_col, name = "Poverty Level", labels = pov_lev) +
  xlab("Year") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(HCV_df, state == "California"), aes(year, fill = factor(poverty))) + 
  geom_bar(position = "fill") +
  ggtitle("Fig. 3b: CA Poverty Concentration by year as percent") +
  scale_fill_manual(values = pov_col, name = "Poverty Level", labels = pov_lev) +
  xlab("Year") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))



##### Regressions #####

lm1 = lm(tpoverty ~
           as.factor(post) +
           state +
           as.factor(post)*state +
           pct_minority +
           pct_female_head +
           pct_age62plus +
           as.factor(code)
           , subset(subh_df, state == "OR" | state == "WA"))