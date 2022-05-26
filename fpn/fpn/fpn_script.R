library(tidyverse)
library(readxl)
library(showtext)
library(pdftools)

#Load pdf and extract pages with data.
pdf  <- pdf_text("https://cdn.prgloo.com/media/fefef3f0ea8241018b9bda2d33fa95be.pdf")
p1 <- pdf[21]
p2 <- pdf[22]

#Isolate just the table from each page
p1 <- strsplit(p1, "\\n") %>%unlist() %>%as.data.frame() %>%
  slice(20:38)
p2 <- strsplit(p2, "\\n") %>%unlist() %>%as.data.frame() %>%
  slice(1:22)

#Bind together and rename columns.  Convert large spaces to commas so we can isolate area name.
df <- bind_rows(p1,p2) %>%
  rename(column_info = 1) %>%
  mutate(column_info = gsub("  ",",",column_info))
rm(p1,p2)

#Split column based on comma data. Indentify data columns.
df <- str_split_fixed(df$column_info, ",", 2) %>% as.data.frame() %>%
  rename(force = 1,
         data = 2) %>%
  mutate(data = gsub(","," ",data),
         data = str_squish(data)) %>% 
  tidyr::separate(data, into=c("N1","N2","N3","N4","N5","N6","N7","N8","N9","N10"), sep= " ")

#Convery all the data columns to numeric.
df[,2:11] <- sapply(df[,2:11],as.numeric)

#Convert non-numeric points to zero. Subtract pre-May 2020 FPNs from the data as this was pre-party gate.
df <- df %>%
  replace(is.na(.), 0) %>%
  mutate(FPN = rowSums(across(where(is.numeric))) - N1,
         force = str_squish(force),
         force = ifelse(force == "Avon & Somerset", "Avon and Somerset",force),
         force = ifelse(force == "Metropolitan","Metropolitan Police",force))

#from" https://data.gov.uk/dataset/d014d7d2-1836-468f-97b8-bb7d0b061bf7/local-authority-district-to-community-safety-partnerships-to-police-force-areas-december-2016-lookup-in-england-and-wales
lookup <- read.csv("/Users/danielgray/Desktop/fpn/Local_Authority_District_to_Community_Safety_Partnerships_to_Police_Force_Areas_(December_2016)_Lookup_in_England_and_Wales.csv") %>%
          rename(force = PFA16NM) %>% select(force,LAD16CD)

df <- left_join(df,lookup, by = c("force")) %>%
      select(force, LAD16CD, FPN)

#from https://www.ons.gov.uk/releases/mappingincomedeprivationatalocalauthoritylevel2019
lookup_2 <- read_excel(path = "/Users/danielgray/Desktop/fpn/localincomedeprivationdata.xlsx", sheet = "Local authorities") %>%
            rename(LAD16CD = 1,
                   deprivation = 3) %>%
            select(LAD16CD,deprivation)

#from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
lookup_3 <- read_excel(path = "/Users/danielgray/Desktop/fpn/ukpopestimatesmid2020on2021geography.xls", sheet = "MYE2 - Persons") %>%
  rename(pop = 4,
         LAD16CD = 1) %>%
  select(LAD16CD,pop)

df <- left_join(df,lookup_2, by = c("LAD16CD")) %>%
      left_join(.,lookup_3, by = c("LAD16CD"))

#Create a weighted-estimate of deprivation by police force area.
short <- df %>% group_by(force) %>% mutate(depxpop = deprivation * as.numeric(pop)) %>%
                          summarise(depxpop = sum(depxpop,na.rm = T),
                                           FPN = mean(FPN,na.rm =T),
                                           pop = sum(as.numeric(pop), na.rm = T)) %>%
                                     mutate(deprivation = depxpop / pop) %>%
                                  filter(pop != 0) %>%
                            mutate(quantile = ntile(deprivation, 5))

#Calculate FPNs per person.
results <- short %>% group_by(quantile) %>% summarise(total_pop = sum(pop, na.rm = T),
                                                      total_FPN = sum(FPN, na.rm = T)) %>%
                                            mutate(per_pop = (total_FPN / total_pop)*1000)


#Add in font for chart.
font_add(famil = "mulish-2",regular = "/Users/danielgray/Desktop/Mulish-2/static/Mulish-Regular.ttf")
showtext::showtext_auto()

#Create chart
ggplot(data = results, aes(x = quantile, y = per_pop)) + geom_col(fill="#db3e5c") +
       theme(
        text = element_text(family = "mulish-2"),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.7, "cm"), ends = "both")),
        axis.title.x = element_text(angle = 0),
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black" ),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0))+ 
  labs(title = "Covid-19 fixed penalty notices per 1000 people.",
       subtitle = "England only. Areas split into five groups, from least-deprived to most-deprived.",
       y = "",
       x = "Least Deprived Areas                                                                     Most Deprived Areas",
       caption = "\n Produced by @DataLobo. \n FPN data from NPCC. Deprivation and population data from ONS. Based on 41 Police Force Areas.") 

#Tweet
"The #SueGray report suggests @BorisJohnson was lucky to receive just a single fixed-penalty notice (FPN). The less privileged weren't so lucky - our analysis shows the police issued more FPNs in deprived areas:"

