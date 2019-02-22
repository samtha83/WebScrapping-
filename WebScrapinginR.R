
install.packages('rvest')

#Loading the rvest package
library('rvest')


#Specifying the url for desired website to be scraped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
webpage <- read_html(url)
item_content <- html_nodes(webpage, '.lister-item-content')


html_text2 <- function(checknode) {
  if (length(checknode)==0) return(NA)
  #print("ans\n")
  #print(html_text(checknode))
  return(html_text(checknode))
}


rank_data<-NULL
#rank_data_html<-NULL
rm(rank_data_html)
title_data<-NULL
#tile_data_html <- NULL
desc_data <- NULL
#desc_data_html <- NULL
runtime_data<-NULL
#runtime_data_html <- NULL
genre_data<- NULL
#genre_data_html <- NULL
rating_data<- NULL
#rating_data_html<- NULL
metascore_data<- NULL
#metascore_data_html <- NULL
votes_data<- NULL
#votes_data_html <- NULL
earn_data<- NULL
#earn_data_html <- NULL
dir_data<-NULL
#dir_data_html <- NULL
act_data<- NULL
#act_data_html <- NULL

for( i in 1:length(item_content))
{
  ################ Rank: The rank of the film from 1 to 100 on the list of 100 most popular feature films released in 2016.
  rank_data_html <- html_nodes(item_content[i],'.text-primary')
  #rank_data_html <- html_nodes(item_content,'.text-primary')
  
  #print(rank_data_html)
  rank_data[i]<-html_text2(rank_data_html)
  #rank_data <- html_text(rank_data_html)

#webpage <- read_html(url)
#Using CSS selectors to scrap the rankings section
#rank_data_html <- html_nodes(webpage,'.text-primary')
#Converting the ranking data to text
#rank_data <- html_text(rank_data_html)
#Let's have a look at the rankings
#head(rank_data)

################ Title: The title of the feature film.
title_data_html  <- html_nodes(item_content[i],'.lister-item-header a')
title_data[i] <- html_text2(title_data_html)


############ Description: The description of the feature film.
desc_data_html <- html_nodes(item_content[i],'.ratings-bar+ .text-muted')
desc_data[i] <- html_text2(desc_data_html)
desc_data[i] <- gsub("\n    ","",desc_data[i])



########################## Runtime: The duration of the feature film.
runtime_data_html <- html_nodes(item_content[i],'.runtime')
runtime_data[i] <- html_text2(runtime_data_html)
runtime_data[i] <- gsub(" min","",runtime_data[i])
#runtime_data[i]<- as.numeric(runtime_data[i])

################### Genre: The genre of the feature film,
genre_data_html  <- html_nodes(item_content[i],'.genre')
genre_data[i] <- html_text2(genre_data_html )
#genre_data[i] <- as.character(genre_data[i])
#genre_data[i] <-gsub("\n","",genre_data[i] )
#genre_data[i] <- gsub(",.*","",genre_data[i] )
#genre_data[i]  <- gsub("            ","",genre_data[i] )
#genre_data[i] <- as.factor(genre_data[i])
#################### Rating: The IMDb rating of the feature film.
rating_data_html <- html_nodes(item_content[i],'.ratings-imdb-rating strong')
rating_data[i] <-html_text2(rating_data_html)
#rating_data[i]<- as.numeric(rating_data[i])


######### Metascore: The metascore on IMDb website for the feature film.
#metascore_data_html <- html_nodes(webpage,'.ratings-metascore')
#item_content[68]

metascore_data_html <- html_nodes(item_content[i],'.ratings-metascore')
metascore_data[i] <- html_text2(metascore_data_html)
metascore_data[i] <- gsub("\n","",metascore_data[i])
metascore_data[i] <- gsub("Metascore            ","",metascore_data[i])
metascore_data[i] <- gsub("                ","",metascore_data[i])

#############Votes: Votes cast in favor of the feature film.
votes_data_html <- html_nodes(item_content[i],'.sort-num_votes-visible span:nth-child(2)')
votes_data[i] <- html_text2(votes_data_html)
#html_text(votes_data_html)
votes_data[i] <-gsub(",","",votes_data[i])
#votes_data[i]<- as.numeric(votes_data[i])


###########Gross_Earning_in_Mil: The gross earnings of the feature film in millions.
earn_data_html <- html_nodes(item_content[i],'.ghost~ .text-muted+ span')
earn_data[i] <- html_text2(earn_data_html)
#earn_data[i] <- gsub("[$]","",earn_data[i])
#earn_data[i] <- gsub('M',"",earn_data[i])
############Director: The main director of the feature film. Note, in case of multiple directors, I'll take only the first.


dir_data_html <- html_nodes(item_content[i],'.text-muted+ p a:nth-child(1)')
dir_data[i] <- html_text2(dir_data_html)


###########Actor: The main actor of the feature film. Note, in case of multiple actors, I'll take only the first.
act_data_html<- html_nodes( item_content[i],'.ghost+ a')
act_data[i] <- html_text2(act_data_html)

} # end of for loop
##########################################


x <-list(rank_data,title_data,desc_data,runtime_data,genre_data,rating_data,metascore_data,votes_data,earn_data,dir_data,act_data)

# lapply  return list of list
#lapply(x,FUN = length) # metascore_data,earn_data are less than 100
#imp note checking by metascore_data_html[50] does not work as 51st position films data get stored  metascore_data_html[50] as 50th metascore data it does not automatically set to NA 

# sapply returns a list
sapply(x,FUN = length)


#creating the final dataframe
movies_df <- data.frame(Rank=rank_data,Title = title_data,desc_data,Runtime =runtime_data,Genre = genre_data,Rating = rating_data,Metascore = metascore_data,
                        Votes=votes_data,Gross=earn_data,Director =dir_data,Actor =act_data)






library('ggplot2')
?qplot
qplot(data = movies_df,Runtime,fill = Genre,bins = 30)

qplot(data = movies_df,Runtime,bins = 30)


item_content[72]


earn_data_html<- html_nodes(item_content,'.ghost~ .text-muted+ span')
earn_data <- html_text2(earn_data_html)
#earn_data[i] <- gsub("[$]","",earn_data[i])
#earn_data[i] <- gsub('M',"",earn_data[i])