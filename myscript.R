democracy <- read.csv("/ameliabadamjav/Downloads/dataverse_files(1)/Conceptualizing and Measuring Subnational Democracy Across Indian States_raw")
library(haven)
Conceptualizing_and_Measuring_Subnational_Democracy_Across_Indian_States_raw <- read_dta("Conceptualizing and Measuring Subnational Democracy Across Indian States_raw.dta")
View(Conceptualizing_and_Measuring_Subnational_Democracy_Across_Indian_States_raw)
library(readxl)
RAI_region_april_2021_final <- read_excel("~/Downloads/RAI_region-april-2021-final.xlsx")
View(RAI_region_april_2021_final)
rai = Conceptualizing_and_Measuring_Subnational_Democracy_Across_Indian_States_raw
democracy = rai
rai = RAI_region_april_2021_final
data = merge(rai, democracy, by = year)
data
savehistory("myscript.R")
grouped_policyautonomy <- data
grouped_policyautonomy %>% group_by(policyautonomy) %>% summarise(average = mean(enpl))
grouped_policyautonomy <- grouped_policyautonomy %>% group_by(policyautonomy) %>% summarise(average = mean(enpl))
ggplot(grouped_policyautonomy, aes(x=average, y=policyautonomy)) + geom_point(alpha=0.5) + geom_smooth(method="lm", se = FALSE) + labs(
  +     title="Political Competition and Political Autonomy in India", x="Effective Number of Parties",
  +     y="Political Autonomy")
enpl_pa <- lm(policyautonomy ~ average, data = grouped_policyautonomy)
summary(enpl_pa)