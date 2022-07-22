View(world_bank_pop)
world_bank_pop %>% count(country)

# inspection code to get the sum of values in each column 
#Checking large population values (for code inspection- to understand abnormally large population values)
world_bank_pop %>% filter(indicator=='SP.POP.TOTL') %>%
  select(-country,-indicator) %>%
  map_dbl(.,sum,na.rm=TRUE)

#lines not needed, used for code testing
world_bank_test <- world_bank_pop %>% filter(indicator=='SP.POP.TOTL')
View(world_bank_pop %>% filter(indicator=='SP.POP.TOTL' & `2017`>150000000))
View(world_bank_pop %>% filter(startsWith(.$country,'N')))



#Get only urban and Total population
world_bank_pop_2= filter(world_bank_pop,indicator == "SP.URB.TOTL" | indicator=="SP.POP.TOTL")

nrow(world_bank_pop_2)
n_distinct(world_bank_pop$country)
View(world_bank_pop_2)


#Change the indicator names to something more understandable
world_bank_pop_2 <- world_bank_pop_2 %>% 
  mutate(indicator= str_replace(indicator,'SP.URB.TOTL','Urban Population'))

world_bank_pop_2 <- world_bank_pop_2 %>% 
  mutate(indicator= str_replace(indicator,'SP.POP.TOTL','Total Population'))

#Change the variable names to title case (capitalize the first letter of the word)
world_bank_pop_2 <- world_bank_pop_2 %>% rename_with(.,str_to_title)

#change from wide to long format
world_bank_pop_2 <- world_bank_pop_2 %>% gather(`2000`:`2017`,key="Year",value = "Population")

#create csv file to export for tableau viz (first version;further prep done)
write_csv(world_bank_pop_2,'World bank population data.csv')

#Checking large population values (for code inspection- to understand abnormally large population values)
world_bank_pop_2 %>% filter(Indicator=="Total Population") %>%
  group_by(Year) %>% summarise("Total population"= sum(Population, na.rm=TRUE))


#read in additional python-processed data (continent,region,income group,country name)
CountryCode <- read_csv("countryCode.csv")

View(CountryCode)

#joining the two tables into one 
world_bank_pop_3 <-  left_join(CountryCode,world_bank_pop_2, by=c('Country ISO3'='Country'))

# inspecting NA values 
View(world_bank_pop_3 %>% filter(is.na(Population)))
world_bank_pop_3$Indicator %>% is.na() %>% sum

#dropping reviewed NA values (1st output --pop4)
world_bank_pop_4 <- world_bank_pop_3 %>% drop_na(Population)

#Creating the 'Others' country category

Total_World_population <- as_tibble(world_bank_pop %>% filter(country=='WLD',indicator=='SP.POP.TOTL') %>% select(-country,-indicator) %>% t)
Total_Urban_population <- as_tibble(world_bank_pop %>% filter(country=='WLD',indicator=='SP.URB.TOTL') %>% select(-country,-indicator) %>% t)

Known_World_population <-world_bank_pop_4 %>% group_by(Year) %>% 
  filter(Indicator=='Total Population') %>%
  summarise(knownPop= sum(Population)) %>% select(knownPop)

Known_Urban_population <-world_bank_pop_4 %>% group_by(Year) %>% 
  filter(Indicator=='Urban Population') %>%
  summarise(knownPop= sum(Population)) %>% select(knownPop)

totalPopDiff <- Total_World_population - Known_World_population
UrbanPopDiff <- Total_Urban_population - Known_Urban_population
View(totalPopDiff)
totalPopDiff <- totalPopDiff  %>% 
  mutate(Country_Name = 'Others',Indicator ='Total Population',Year=2000:2017,Country_ISO3='OTH', Region = NA, Continent = NA,Income_Group = NA) %>% 
rename('Population'='V1') 

UrbanPopDiff <- UrbanPopDiff  %>% 
  mutate(Country_Name = 'Others',Indicator ='Urban Population',Year=2000:2017,Country_ISO3='OTH', Region = NA, Continent = NA,Income_Group = NA) %>% 
  rename('Population'='V1')   

View(UrbanPopDiff)
world_bank_pop_4 <- world_bank_pop_4 %>% rename('Country_ISO3'='Country ISO3','Income_Group'='Income Group')

world_bank_pop_4 <- rbind(world_bank_pop_4,totalPopDiff,UrbanPopDiff)

View(world_bank_pop_4)
tail(world_bank_pop_4)
write.csv(world_bank_pop_4, 'World bank population data (cleaned).csv')

ggplot(filter(world_bank_pop_4,Indicator=='Total Population' & Year==2017))+geom_boxplot(aes(x=reorder(Continent,Population,median),y=Population,fill=Continent),show.legend = FALSE) + scale_y_log10()+
  theme(axis.text.x=element_text(angle=35,hjust=0.9)) +labs(title = 'Population distribution by continent',x='Continent')

popgrowth <- world_bank_pop_4 %>% filter(Indicator=='Total Population') %>% group_by(Year) %>% summarise(Totalpop = sum(Population))

ggplot(popgrowth) + geom_point(aes(x=Year,y=Totalpop))+geom_line(aes(x=Year,y=Totalpop))+ scale_y_log10()

#ggplot(world_bank_pop_4) + geom_point(aes(x=Year,y=Population),group=Year)