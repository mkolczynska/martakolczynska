library(tidyverse) # data manipulation and cleaning
#library(UpSetR)
#library(openssl)
#library(httpuv)
#devtools::install_github("mkearney/rtweet")
library(rtweet) # get tweets
library(wordcloud)
library(ggplot2) # plots
library(maps) # maps
library(tidytext) # tokenize
library(lubridate) # dealing with dates
library(DescTools) # Lorenz curve
library(reldist) # gini index


# I had something else prepared for the first post, but 3 hours spent at Toronto's Pearson airport 
# led to a change of plans.


# For a detailed description of how to connect to the Twitter API see e.g. this post: 
#   https://compsocialscience.github.io/summer-institute/2018/materials/day2-digital-trace-data/apis/rmarkdown/SICSS_APIs_markdown.html



# ```{r Twitter connect, echo = FALSE}
appname= "jashte_tokesore"
consumer_key= "dDZgvH9YZLbNrUPPwO8okUOdT"
consumer_secret= "va0kmR4uGSKHV2YbNEFd3t5pf5b4It8wogDzbO3p8SJxKu7dbK"
access_token = "123591587-Nuq20DV0hRjGfEk4JdMmPgmyLpx2jvIXxNrt6VRm"
access_secret = "EiQ1sekvfOgFIVx9Ae7UX1DPR7yu0il22PfA7IbROTwCE"

create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret
)
```


# isa <- search_tweets("#isa2018wcs OR #isa18wcs", since = "2018-06-01", 
#                       n = 100000, type="recent", include_rts=TRUE)

# library(data.table)
# fwrite(isa, file = "C:/Users/mkolc/Google Drive/Academia/Blog/martakolczynska/drafts/isa_tweets.csv")
# 
isa1 <- read.csv("C:/Users/mkolc/Google Drive/Academia/Blog/martakolczynska/drafts/isa_tweets.csv", 
                  stringsAsFactors = FALSE)

# Put in local time
isa$created_at <-  with_tz(isa$created_at, 'America/Toronto')

min(isa$created_at)
max(isa$created_at)

head(isa$text)


ts_plot(isa, "1 hour") +
  labs(x = NULL, y = NULL,
    title = "Frequency of #isa2018wcs OR #isa18wcs tweets since July 12, 2018",
    subtitle = "Twitter counts by hour",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

isa %>% filter(as.Date(created_at) >= "2018-07-15" & as.Date(created_at) <= "2018-07-21") %>%
  ts_plot(., "1 hour") + 
    labs(x = NULL, y = NULL,
       title = "Frequency of #isa2018wcs OR #isa18wcs tweets since July 12, 2018",
       subtitle = "Twitter counts by hour",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


geocoded <- lat_lng(isa[isa$is_retweet == FALSE,])

par(mar = c(0, 0, 0, 0))
maps::map("world", lwd = .25)
with(geocoded, points(lng, lat, pch = 20, cex = 2, col = "forestgreen"))


###
data("stop_words")

isa_words <- isa %>%
  unnest_tokens(word, text) %>%
  select(status_id, created_at, word) %>%
  anti_join(stop_words) %>%
  filter(!grepl("https|t.co|amp|rt|isa18wcs|isa2018wcs|isa_sociology|sociology|isa|de", 
              word)) %>%
  filter(!grepl("\\b\\d+\\b", word))


isa_words %>%
  count(word) %>%
  arrange(desc(n))



### rc

isa_rc <- isa_words %>% filter(str_detect(tolower(word), "rc[0-9]{2}"))
head(isa_rc, 10)

isa_rc$rc <- str_extract(isa_rc$word, "rc[0-9]{2}")
isa_rc <- isa_rc %>% group_by(status_id, created_at, rc) %>% summarise()

wordcloud(isa_rc$rc, min.freq=1)

isa_rc_n <- isa_rc %>% group_by(rc) %>% tally() %>% arrange(desc(n))

plot(Lc(isa_rc_n$n, col="darkred",lwd=2))

gini(isa_rc_n$n)

top_3 <- isa_rc %>% group_by(rc) %>% tally() %>% arrange(desc(n)) %>% top_n(3) %>% select(rc) %>% as_vector()

isa_rc %>% filter(as.Date(created_at) >= "2018-07-15" & as.Date(created_at) <= "2018-07-21",
                  rc %in% top_3) %>%
  ggplot(., aes(x = created_at, col = rc)) + geom_freqpoly(size= 1.5) + 
  ylab("Number of tweets") + 
  xlab("Date") + 
  ggtitle("Tweets by Research Committee (top 3 RCs)") +
  scale_color_manual(name="Research committee", 
                     labels = c("RC22 Religion",
                                "RC24 Environment",
                                "RC34 Youth"), 
                     values = c("rc22"="gold2",
                                "rc24"="deeppink3",
                                "rc34"="turquoise4"))




