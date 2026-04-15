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
ggplot(grouped_policyautonomy, aes(x=average, y=policyautonomy)) + geom_point(alpha=0.5) + geom_smooth(method="lm", se = FALSE) + labs(
  +     title="Political Competition and Political Autonomy in India", x="Effective Number of Parties",
  +     y="Political Autonomy")
ggplot(data, aes(x=enpl, y=mov)) + geom_point(alpha=0.5) + geom_smooth(method="lm", se = FALSE) + labs(
  +     title="Political Competition and Margin of Victory in India", x="Effective Number of Parties",
  +     y="Margin of Victory")
enpl_mov <- lm(mov ~ enpl, data = data)
summary(enpl_mov)
ggplot(data, aes(x=sop, y=mov)) + geom_point(alpha=0.5) + geom_smooth(method="lm", se = FALSE) + labs(
  +     title="Strength of Opposition and Margin of Victory in India", x="Strength of Opposition",
  +     y="Margin of Victory")
sop_mov <- lm(mov ~ sop, data = data)
summary(sop_mov)
ggplot(data, aes(x=pr, y=RAI)) + geom_point(alpha=0.5) + geom_smooth(method="lm", se = FALSE) + labs(
  +     title="Strength of Opposition and Margin of Victory in India", x="Strength of Opposition",
  +     y="Margin of Victory")
library(readxl)
API_IND_DS2_en_excel_v2_2507 <- read_excel("~/Downloads/API_IND_DS2_en_excel_v2_2507.xls")