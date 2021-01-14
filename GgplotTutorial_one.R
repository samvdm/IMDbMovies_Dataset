library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
#load data
IMDb_movies <- fread('~/TheMoviesDataset/data/archive (5)/IMDb movies.csv')
IMDb_movies <- IMDb_movies[order(-year)]

######################Data cleaning
#clean dates
IMDb_movies[, year := ifelse(year == 'TV Movie 2019', '2019', year)]
View(head(IMDb_movies))

#convert to date
IMDb_movies$date_published <- try(as.Date(IMDb_movies$date_published, format="%Y-%m-%d"))

#remove $ sign from revenue variable
IMDb_movies[, worlwide_gross_income := gsub('\\$ ', '', worlwide_gross_income)] 
IMDb_movies$worlwide_gross_income <- as.numeric(IMDb_movies$worlwide_gross_income)
#convert to revenue in millions 
IMDb_movies[, worlwide_gross_income := worlwide_gross_income/1000000] 

######################trend over time
#filter to only years interested in
group_years <- as.character(seq(1990, 2020, by =1))
my_movies2 <- IMDb_movies[year %in% group_years]

#count number of moviews released grouped by year 
agg_dat <- my_movies2[, .(count = .N, worldwide_gross_income = sum(worlwide_gross_income, na.rm = TRUE)), by = year]

####convert year to date (ggplot needs to recognise the date formate to plot a proper trend)
agg_dat$year <- as.integer(agg_dat$year)
agg_dat[, months := ifelse(year < 2020, 12, 10)] #normalize based on month as 2020 data is incomplete
agg_dat[,avg_monthly_gross := worldwide_gross_income/months]
agg_dat[, Date := as.Date(ISOdate(year, 1, 1))]
agg_dat[,mycount:= count/months]
View(agg_dat)

#plot Movies released over time
p <- ggplot(agg_dat, aes(x=Date, y=mycount)) +
  geom_line(color = '#FF1493', size =1) + geom_point(color = "#696969", size = 1) + ggtitle("Number of movies released by time (IMDb)") + theme_light() + 
  theme(legend.position = "none") + scale_y_continuous(name = "Number of movies released (month avg)")
p

#saves image in high resolution
ggsave(p, file = "content.png", dpi = 700)

#plot worldwide revenue over time
q <- ggplot(agg_dat, aes(x=Date, y=avg_monthly_gross))+ scale_y_continuous(name = "Movies worldwide gross income (mil, month avg)") + geom_line() +
  ggtitle("Movies worldwide gross income by time (IMDb)") + theme_light() +
  geom_line(color = '#FF1493', size =1) + geom_point(color = "#696969", size = 1)
q

ggsave(q, file = "grossworldwide.png", dpi = 700)


###Highest rated movies of 2020
movies2020 <- IMDb_movies[year == '2020']
movies2020 <- movies2020[language == 'English']
movies2020 <- movies2020[ !(is.na(reviews_from_critics))]
movies2020 <- movies2020[ reviews_from_users > 400]
movies2020 <- movies2020[order(-avg_vote)]
View(head(movies2020, 10))
movies2020_2 <- movies2020[order(-metascore)]
View(head(movies2020_2, 10))

############by production company
### pie chart
companies <- c("Walt Disney Pictures",'Netflix' ,"Columbia Pictures", "Universal Pictures", "Paramount Pictures")
movies2020 <- IMDb_movies[year == '2020']
movies2020 <- movies2020[production_company %in% companies]

data <- movies2020 %>% 
  group_by(production_company) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(production_company))
data$label <- scales::percent(data$per)


s <- ggplot(data=data)+
  geom_bar(aes(x="", y=per, fill=production_company), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label)) +theme(legend.position = "none")

s
ggsave(s, file = "productioncompany2019.png", dpi = 700)

##### Netflix
data_netflix <- movies2020[production_company == 'Netflix']
data_netflix <- data_netflix[language == 'English']
data_netflix <- data_netflix[ !(is.na(reviews_from_critics))]
data_netflix <- data_netflix[ reviews_from_users > 400]


data_netflix <- data_netflix[order(-avg_vote)]
View(head(data_netflix,10))
View(data_netflix)



