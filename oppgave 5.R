library(jsonlite)
library(httr)
library(jsonstat)
library(rjstat)
library(ggplot2)

##--- OPPGAVE 5.1.1


url <- 'https://data.ssb.no/api/v0/no/table/05185/'

data <- '{ "query": [ { "code": "Kjonn", "selection": { "filter": "item", "values": [ "1", "2" ] } }, { "code": "Landbakgrunn", "selection": { "filter": "agg:Verdensdel2", "values": [ "b0", "b11", "b12", "b13", "b14", "b2", "b3", "b4", "b5", "b6", "b8", "b9" ] } }, { "code": "Tid", "selection": { "filter": "item", "values": [ "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022" ] } } ], "response": { "format": "json-stat2" } }'

d <- POST(url , body = data, encode = "json", verbose())

data <- fromJSONstat(content(d, "text"))
data <- as_tibble(data)


data <- data %>%  
  group_by(landbakgrunn, statistikkvariabel, år) %>% 
  summarise(Antall_innvandrere = sum(value))

data %>% 
  ggplot(aes(år, Antall_innvandrere, group = landbakgrunn, col = landbakgrunn)) +
  geom_line()+
  theme(axis.text.x = element_text(angle=45, vjust=0.6))+
  xlab('Årstall')+
  ylab('Antall innvandrere i Norge')+
  
  
  
##-----OPPGAVE 5.1.2


url2 <- 'https://data.ssb.no/api/v0/no/table/13215/'

data2 <- '{ "query": [ { "code": "Kjonn", "selection": { "filter": "item", "values": [ "0" ] } }, { "code": "Alder", "selection": { "filter": "item", "values": [ "15-74" ] } }, { "code": "InnvandrKat", "selection": { "filter": "item", "values": [ "B" ] } }, { "code": "Landbakgrunn", "selection": { "filter": "item", "values": [ "015a" ] } }, { "code": "NACE2007", "selection": { "filter": "agg:NACE260InnvGrupp2", "values": [ "SNI-00-99", "SNI-01-03", "SNI-05-09", "SNI-10-33", "SNI-35-39", "SNI-41-43", "SNI-45-47", "SNI-49-53", "SNI-49.3", "SNI-55", "SNI-56", "SNI-58-63", "SNI-64-66", "SNI-68-75", "SNI-77-82", "SNI-78.2", "SNI-81.2", "SNI-84", "SNI-85", "SNI-86-88", "SNI-90-99", "SNI-00" ] } }, { "code": "Tid", "selection": { "filter": "item", "values": [ "2021" ] } } ], "response": { "format": "json-stat2" } }'

d2 <- POST(url2 , body = data2, encode = "json", verbose())

data2 <- fromJSONstat(content(d2, "text"))
data2 <- as_tibble(data2)

data2$`næring (SN2007)` <- gsub("[0-9,-]", "", data2$`næring (SN2007)`)
data2 <- data2[-1,]

data2 %>% 
  ggplot()+
  geom_col(aes(x = `næring (SN2007)`, y = value, col = `næring (SN2007)`),show.legend = FALSE)+
  theme_bw()+
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=90, hjust=1))+
  xlab('Næring')+
  ylab('Antall arbeidere fra Øst-Europa')
