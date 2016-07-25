#Project to see how the galleries participating in the October art fairs (London) are distributed globally

setwd("~/Documents/R Data/")

library(rvest)
library(dplyr)
library(stringi)
library(stringr)
library(data.table)
library(countrycode)
library(maps)

##PAD Art + Design 3-9 OCTOBER 2016

#Scrape the PAD site for exhibitors
padURL <- "https://www.pad-fairs.com/london/exhibitors/exhibitors-by-a-to-z/"

padlist <- padURL %>% 
  read_html() %>%
  html_nodes(xpath = '//*[(@id = "post-9")]//a') %>%
  html_text(trim = TRUE)

padlist

#The Netherlands was the only country with more than one word
padlist <- sub("The Netherlands", "Netherlands", padlist)

#Separate the countries listed after each country
pad <- as.data.table(padlist)
pad
pad <- pad[, tstrsplit(padlist, ",", fixed = TRUE)]
pad[, "Country_1" := word(V1, start = -1)]
pad[, "V1" := word(V1, start = 1, end = -2) ]
pad
setnames(pad, old = c("V1", "V2", "V3", "V4"), new = c("Gallery", "Country_2", "Country_3", "Country_4"))

#Wide to long
pad <- melt(pad, id = 1, na.rm = TRUE, variable.name = "Number.of.Countries", value.name = "Country")
pad[, "Country" := str_trim(Country)]

#Align the country names to countrycode_data set
paduni <- unique(pad$Country)
paduni[!(paduni %in% countrycode_data$country.name)] 

pad[Country == "USA", Country := "United States"]
pad[Country == "UK", Country := "United Kingdom"]
pad[Country == "Hong-Kong", Country := "Hong Kong"]
pad[Country == "Chine", Country := "China"]
pad[Country == "Brasil", Country := "Brazil"]

pad[, Number.of.Countries := NULL]

#Add fair name to dataset for later 
pad[, "Art.Fair" := "PAD"]
pad


##1-54 Contemporary African Art Fair 6 - 9 October 2016

#Scrape exhibitor information from website
one54URL <- "http://1-54.com/london/gallery-list-announced-for-2016-edition-of-the-fair/"

one54list <- one54URL %>%
  read_html() %>%
  html_nodes( 'h1+ p') %>%
  html_text(trim = TRUE) 
one54list

#Organize text blob to a list then data table
one54list <- as.list(strsplit(one54list, "\n"))
one54list

one54c.start <- str_locate(one54list[[1]], ",")
one54c.end <- nchar(one54list[[1]])
one54countries <- str_sub(one54list[[1]], start = one54c.start[ ,1] +2, 
                          end = one54c.end - 1)
one54countries
  
one54g.end <- stri_locate_last_fixed(one54list[[1]], "(")

one54galleries <- str_sub(one54list[[1]], start = 1, 
                          end = one54g.end[ ,1] - 2)
one54galleries

one54 <- data.table("Gallery" = one54galleries, "Country" = one54countries)
one54

#Align the country names to countrycode_data set
oneuni <- unique(one54$Country)
oneuni
oneuni[!(oneuni %in% countrycode_data$country.name)]

one54[Country == "Côte d’Ivoire", Country := "Cote d'Ivoire"]

#Add fair name to dataset for later 
one54[, "Art.Fair" := "1:54"]
one54


##Frieze Masters Ancient to Modern Art 6 – 9 October 2016

#Get a list of the exhibitor pages
fri.m.pages <- c(1:6)
fri.m.n <- length(fri.m.pages)
fri.m.links <- vector("list", fri.m.n)

for(i in 1:fri.m.n){
  fri.m.links[[1]] <- "https://frieze.com/fairs/frieze-masters/galleries"
  fri.m.links[[i + 1]] <- paste0("https://frieze.com/fairs/frieze-masters/galleries?title_field_value=&city=&field_gallery_fl_fair_section_value=All&field_gallery_fm_fair_section_value=All&field_gallery_fny_fair_section_value=All&page=", fri.m.pages[i])
}
fri.m.links

#Scrape pages for exhibitor list
fri.m.lists <- vector("list", 7)

for(i in 1:length(fri.m.lists)){
fri.m.lists[[i]] <- read_html(fri.m.links[[i]]) %>%
  html_nodes(xpath = '//*[(@id = "block-views-fair-galleries-block")]//h4 | //*[(@id = "block-views-fair-galleries-block")]//*[contains(concat( " ", @class, " " ), concat( " ", "field-type-text", " " ))]//a') %>%
  html_text(trim = TRUE)
}
fri.m.lists

#Separate cities from galleries

fri.m.unlist <- unlist(fri.m.lists)
fri.m.unlist

odds <- seq(1, length(fri.m.unlist) - 1, 2)
evens <- seq(2, length(fri.m.unlist), 2)

fri.m <- data.table(Gallery = fri.m.unlist[odds], name = fri.m.unlist[evens])
fri.m    #133 galleries

#Need to look up cities using "maps" package
#City is "name" column from world.cities data table. Country is "country.etc"
fri.m.cities <- unique(fri.m$name)
fri.m.cities
fri.m.cities[!(fri.m.cities %in% world.cities$name)] #Change Seoul, Mumbai, São Paulo, Düsseldorf

fri.m[name == "Seoul", name := "Soul"]
fri.m[name == "Mumbai", name := "Bombay"]
fri.m[name == "São Paulo", name := "Sao Paulo"]
fri.m[name == "Düsseldorf", name := "Dusseldorf"]

#Redo city check
fri.m.cities <- unique(fri.m$name)
fri.m.cities[!(fri.m.cities %in% world.cities$name)] 

world.cities2 <- subset(world.cities, pop > 100000, c(name, country.etc))
world.cities2 <- subset(world.cities2, world.cities2$name %in% fri.m$name)
world.cities2[duplicated(world.cities2$name),]

#Get rid of duplicated cities
world.cities2 <- world.cities2[world.cities2$country.etc != "Venezuela" & world.cities2$country.etc != "Canada",] #The Valencian gallery is in Spain
world.cities2$name <- str_trim(world.cities2$name)
world.cities2

#Match city to country
setkey(fri.m, name)

fri.m.merge <- merge(fri.m, world.cities2)
fri.m.merge
setnames(fri.m.merge, old = "country.etc", new =  "Country")
fri.m.merge[, name := NULL]
fri.m.merge

#I manually filled in the gallery locations for the entries with "Multiple Locations" on a spreadsheet
fri.m.mult <- read.csv("Frieze Masters - Sheet1.csv")
fri.m.mult

fri.m.list <- list(fri.m.mult, fri.m.merge)
fri.masters <- rbindlist(fri.m.list)
fri.masters

#Trim spaces
fri.masters[, Gallery := str_trim(Gallery)]

#Check correct number of galleries
uni.fri.masters <- unique(fri.masters$Gallery)
uni.fri.m <- unique(fri.m$Gallery)

uni.fri.masters[!(uni.fri.masters %in% uni.fri.m)]

summary(fri.masters$Country)

#Fix country names
fri.masters[Country == "Korea South", Country := "Korea"]
fri.masters[Country == "USA", Country := "United States"]
fri.masters[Country == "UK", Country := "United Kingdom"]

fri.masters[, "Art.Fair" := "Frieze Masters"]
fri.masters


##Frieze Contemporary Art 6 – 9 October 2016

#Get a list of the exhibitor pages
fri.pages <- c(1:8)
fri.n <- length(fri.pages)

fri.links <- vector("list", fri.n)

for(i in 1:fri.n){
  fri.links[[1]] <- "https://frieze.com/fairs/frieze-london/galleries"
  fri.links[[i + 1]] <- paste0("https://frieze.com/fairs/frieze-london/galleries?title_field_value=&city=&field_gallery_fl_fair_section_value=All&field_gallery_fm_fair_section_value=All&field_gallery_fny_fair_section_value=All&page=", fri.pages[i])
}
fri.links

#Scrape pages for exhibitor list
fri.lists <- vector("list", 9)

for(i in 1:length(fri.lists)){
  fri.lists[[i]] <- read_html(fri.links[[i]]) %>%
    html_nodes(xpath = '//*[(@id = "block-views-fair-galleries-block")]//h4 | //*[(@id = "block-views-fair-galleries-block")]//*[contains(concat( " ", @class, " " ), concat( " ", "field-type-text", " " ))]//a') %>%
    html_text(trim = TRUE)
}
fri.lists

#Make seperate city/gallery lists
fri.unlist <- unlist(fri.lists)
fri.unlist

odds <- seq(1, length(fri.unlist) - 1, 2)
evens <- seq(2, length(fri.unlist), 2)

fri <- data.table(Gallery = fri.unlist[odds], name = fri.unlist[evens])
fri
unique(fri$Gallery) #165 galleries

#Subset world.cities dataset, almost same as for Frieze Masters above
world.cities2 <- subset(world.cities, pop > 150000, c(name, country.etc))
world.cities2 <- subset(world.cities2, world.cities2$name %in% fri$name)
world.cities2[duplicated(world.cities2$name),]
world.cities2 <- world.cities2[world.cities2$country.etc != "Canada",]
world.cities2

fri.cities <- unique(fri$name)
fri.cities
fri.cities[!(fri.cities %in% world.cities2$name)]

fri[name == "Seoul", name := "Soul"]
fri[name == "Mumbai", name := "Bombay"]
fri[name == "São Paulo", name := "Sao Paulo"]
fri[name == "Düsseldorf", name := "Dusseldorf"]
fri[name == "Tel Aviv", name := "Tel Aviv-Yafo"]
fri[name == "Bogotá", name := "Bogota"]
fri[name == "Guatemala City", name := "Guatemala"]

#Repeat world.cities list and fri.cities check above
world.cities2 <- subset(world.cities, pop > 150000, c(name, country.etc))
world.cities2 <- subset(world.cities2, world.cities2$name %in% fri$name)
world.cities2[duplicated(world.cities2$name),]
world.cities2 <- world.cities2[world.cities2$country.etc != "Canada",]
world.cities2

fri.cities <- unique(fri$name)
fri.cities[!(fri.cities %in% world.cities2$name)]

MissingCities <- fri.cities[!(fri.cities %in% world.cities2$name)]
MissingCities
fri[name %in% MissingCities] #45 entries
fri[!(name %in% MissingCities)] #120 entries

#Combine data tables into one
setkey(fri, name)

fri.merge <- merge(fri, world.cities2, by = )
fri.merge 

setnames(fri.merge, old = "country.etc", new =  "Country")

fri.merge[, name := NULL]
fri.merge 

fri.Missing <- fri[name %in% MissingCities]
fri.Missing 

setkey(fri.Missing, Gallery)
fri.Missing.merge <- merge(fri.Missing, fri.masters, all.x = TRUE)
fri.Missing.merge
unique(fri.Missing.merge$Gallery)

fri.Missing.merge[name == "St. Moritz", Country := "Switzerland"]
fri.Missing.merge[is.na(Country)] #34 entries - will look up manually
fri.Missing.merge <- fri.Missing.merge[!(is.na(Country))]
fri.Missing.merge[, c("name", "Art.Fair") := NULL]
fri.Missing.merge

#combine everything
fri.mult <- read.csv("Frieze - Sheet1.csv")
fri.mult

fri.comb.list <- list(fri.mult, fri.Missing.merge)
fri.comb.bind <- rbindlist(fri.comb.list)
fri.comb.bind
summary(fri.comb.bind)

unique(fri.comb.bind$Gallery) #45
unique(fri.merge$Gallery) #120

fri.list <- list(fri.comb.bind, fri.merge)
frieze <- rbindlist(fri.list)
frieze

frieze[, Gallery := str_trim(Gallery)]

uni.frieze <- unique(frieze$Gallery)
uni.frieze

summary(frieze$Country)

frieze[Country == "Korea South", Country := "Korea"]
frieze[Country == "USA", Country := "United States"]
frieze[Country == "UK", Country := "United Kingdom"]

frieze[, "Art.Fair" := "Frieze"]
frieze
unique(frieze$Gallery)


##Combine all art fair data sets

LondonFairList <- list(frieze, fri.masters, one54, pad)
LondonFairsAll <- rbindlist(LondonFairList)

LondonFairsAll

London1 <- LondonFairsAll[, Number.Galleries := .N, by = .(Art.Fair, Country)]

London1

setkey(London1, Art.Fair, Country)
London1 <- London1[, .SD[1], by = .(Art.Fair, Country)]
London1[, Gallery := NULL]


uniLon <- unique(London1$Country)
uniLon[!(uniLon %in% countrycode_data$country.name)]

London1[Country %like% "Korea", Country := "Korea"]
London1[Country %like% "Taiwan", Country := "Taiwan, Province of China"]
London1[, Country.code := countrycode(Country, "country.name", "iso3c")]
London1

LondonCondensed <- London1[, Number.Galleries2 := sum(Number.Galleries), by = Country]
LondonCondensed <- LondonCondensed[, .SD[1], by = Country][, c("Art.Fair", "Number.Galleries") := NULL]
LondonCondensed
