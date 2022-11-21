library(tidyverse)
library(stringr)
library(xml2)
library(rvest)
library(dplyr)

#get the website (IMDb Top 100)
web=read_html('https://www.imdb.com/search/title/?groups=top_100&sort=user_rating,desc&count=100')

get.webtitle=html_nodes(web,'.lister-item-header a')
get.title=html_text(get.webtitle)

#get the release year of the film
get.webyear=html_nodes(web,'.text-muted.unbold')
get.year=html_text(get.webyear)
get.year[56]=substring(get.year,1,6)
get.year[61]=substring(get.year,1,6)
get.year=substring(get.year,2,5)
year=as.numeric(get.year)

#get the gross of the film
get.webgross=html_nodes(web,'.sort-num_votes-visible')
get.gross=html_text(get.webgross)
get.gross=substring(get.gross, 90,100)
get.gross=str_remove_all(get.gross,' ')
get.gross=gsub("M"," ",get.gross)
get.gross=gsub("\n"," ",get.gross)              
get.gross=substring(get.gross,2,6)
gross=as.numeric(get.gross)
gross[is.na(gross)]=0

#get film genre
get.webgenre=html_nodes(web,'.genre')
get.genre=html_text(get.webgenre)
get.genre=str_remove_all(get.genre,' ')
get.genre=gsub('\n','',get.genre)
genre1=gsub('.*,','',get.genre)
genre1=as.factor(genre1)

#get film rate
get.webrate=html_nodes(web,'.ratings-imdb-rating strong')
get.rate=html_text(get.webrate)
rate=as.numeric(get.rate)

#make a data frame
data=data.frame(title=get.title, year=year, rate=rate, genre1=genre1, gross=gross)

#suggest title with most viewed genre
gen.mode=function(x){
  u=unique(x)
  tab=tabulate(match(x,u))
  u[tab==max(tab)]
}

data%>%filter(str_detect(genre1, as.character(gen.mode(data$genre1))))

#visualize data in plot
ggplot(data,aes(x=year,y=gross))+geom_point(aes(size=rate,col=genre1))
