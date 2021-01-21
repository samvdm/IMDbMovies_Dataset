library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)

#load data
############# rotten tomatoes data
rotten_movies <- fread('~/TheMoviesDataset/data/archive (6)/rotten_tomatoes_movies.csv')
critics_review <- fread('~/TheMoviesDataset/data/archive (6)/rotten_tomatoes_critic_reviews.csv')
rotten_movies$movie_title <- tolower(rotten_movies$movie_title)
rotten_movies <- setnames(rotten_movies, old = 'movie_title', new = 'title')

######################Data cleaning
#convert to date
rotten_movies$original_release_date <- as.Date(rotten_movies$original_release_date)
rotten_movies[, year := year(original_release_date)]

#check last entry of 2020
rotten2020 <- rotten_movies[year == 2020]
max(rotten2020$original_release_date)
length(unique(rotten_movies$year)) #check total number of years

######################trend over time
#filter to only years interested in
group_years <- as.character(seq(1990, 2020, by =1))
my_movies2 <- rotten_movies[year %in% group_years]

#count number of moviews released grouped by year 
agg_dat <- my_movies2[, .(count = .N, tomatometer_rating = mean(tomatometer_rating, na.rm = TRUE)), by = year]

####convert year to date (ggplot needs to recognise the date formate to plot a proper trend)
agg_dat$year <- as.integer(agg_dat$year)
agg_dat[, months := ifelse(year < 2020, 12, 9)] #normalize based on month as 2020 data is incomplete
agg_dat[, Date := as.Date(ISOdate(year, 1, 1))]
agg_dat[,mycount:= count/months]
View(agg_dat)

#plot Movies released over time
p <- ggplot(agg_dat, aes(x=Date, y=mycount)) +
  geom_line(color = '#FF1493', size =1) + geom_point(color = "#696969", size = 1) + ggtitle("Number of movies released by time") + theme_light() + 
  theme(legend.position = "none") + scale_y_continuous(name = "Number of movies released (month avg)")
p

#saves image in high resolution
ggsave(p, file = "content.png", dpi = 700)


###Highest rated movies of 2020
rotten2020 <- rotten_movies[year == 2020]
rotten2020 <- rotten2020[ audience_count > 1000]
rotten2020 <- rotten2020[order(-tomatometer_rating)]
View(head(rotten2020, 20))

############by production company
group_years <- as.character(seq(2015, 2020, by =1))
my_movies <- rotten_movies[year %in% group_years]
agg_dat <- my_movies[, .(count = .N, tomatometer_rating = mean(tomatometer_rating, na.rm = TRUE)), by =  production_company]
agg_dat <- agg_dat[order(-count)]
View(head(agg_dat, 20))

companies <- c("Universal Pictures",'Disney/Pixar' ,"Warner Bros. Pictures", "Columbia Pictures", "Paramount Pictures", '20th Century Fox', 'IFC Films',
               'Sony Pictures Classics', 'Netflix', 'Lionsgate', 'New Line Cinema', 'Walt Disney Pictures')


##stacked bar
my_movies2 <- rotten_movies[year %in% group_years]
my_movies2 <- my_movies2[production_company %in% companies]
my_movies2[, production_company := ifelse(production_company =='Disney/Pixar', 'Walt Disney Pictures', production_company)]
agg_dat_2 <- my_movies2[, .(count = .N, tomatometer_rating = mean(tomatometer_rating, na.rm = TRUE)), by = list(year, production_company)]

q <- ggplot(agg_dat_2, aes(fill=production_company, y=count, x=year)) + 
  geom_bar(position="dodge", stat="identity") + theme_light()

q
r <- ggplot(agg_dat_2, aes( y=count, x=year, fill ='year')) + 
  geom_bar(stat="identity") + theme_light()

r
### pie chart

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
data_netflix <- movies2020[production_company == 'Walt Disney Pictures']
data_netflix <- data_netflix[language == 'English']
data_netflix <- data_netflix[ !(is.na(reviews_from_critics))]
data_netflix <- data_netflix[ reviews_from_users > 400]

data_netflix <- data_netflix[order(-avg_vote)]
View(head(data_netflix,10))
View(data_netflix)





