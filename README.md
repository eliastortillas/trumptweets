Trump’s Tweets
================
Elias M. Guerra
4/19/2018

``` r
library(rjson)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)

trump.tweets <- fromJSON(file = "~/Documents/R/math311/condensed_2018.json")
n <- length(trump.tweets) 
trump <- matrix(nrow = n, ncol = 2)
for (i in 1:n) {
  trump[i,] <- c(trump.tweets[[i]]$created_at,
                 trump.tweets[[i]]$text)
}
trump <- as.data.frame(trump)
colnames(trump) <- c("date.time", "text")
trump$date <- as.Date(substr(trump$date.time, 1, 10), format = "%a %b %d %t")
trump$time <- substr(trump$date.time, 12, 19) 
```

Let's take a peak at our dataset.

``` r
library(knitr)
kable(trump[1:3,])
```

| date.time                      | text                                                                                                                                                                                                                                                                            | date       | time     |
|:-------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------|:---------|
| Thu Apr 19 15:48:38 +0000 2018 | Governor Jerry Brown announced he will deploy “up to 400 National Guard Troops” to do nothing. The crime rate in California is high enough, and the Federal Government will not be paying for Governor Brown’s charade. We need border security and action, not words!          | 2018-04-19 | 15:48:38 |
| Thu Apr 19 15:23:22 +0000 2018 | Thank you San Diego County for defending the rule of law and supporting our lawsuit against California's illegal and unconstitutional 'Sanctuary' policies. California's dangerous policies release violent criminals back into our communities, putting all Americans at risk. | 2018-04-19 | 15:23:22 |
| Thu Apr 19 14:45:14 +0000 2018 | Great meeting with Prime Minister Abe of Japan, who has just left Florida. Talked in depth about North Korea, Military and Trade. Good things will happen!                                                                                                                      | 2018-04-19 | 14:45:14 |

We'll start by answer the question of who Trump is talking about the most. We'll look at the most frequent mentions.

``` r
# Who does Trump mention most on Twitter
mentions <- NULL
for (i in 1:nrow(trump)) {
  x <- str_split(trump$text[i], pattern = " ", simplify = T)
  mentions <- c(mentions, str_subset(x, pattern = "^@"))
}
bad <- c("!",",", ":")
for (i in 1:3) {
  mentions <- str_replace(mentions, bad[i], "")
}
most.mentions <- 
  mentions %>%
  table %>%
  data.frame %>%
  filter(Freq >= 3) %>%
  arrange(Freq)
most.mentions$. <- factor(most.mentions$., levels = most.mentions$.)
ggplot(most.mentions) + 
  geom_bar(aes(., Freq), stat = "identity") +
  ggtitle("Who does Trump mention most on Twitter?",
          subtitle = expression("no. of mentions" >= 3)) +
  coord_flip()
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

``` r
# When does trump use positive or negative language?
trump.words <- read_csv("~/Documents/R/math311/trump_twitter_words.csv")
trump.words$date <- as.Date(trump.words$date)
nrc <- get_sentiments("nrc")
trump.sentiment <- 
  trump.words %>%
  inner_join(nrc, by = "word")
trump.sentiment %>%
  group_by(sentiment) %>%
  summarize(freq = length(date)) %>%
  ggplot + geom_bar(aes(x = sentiment, y = freq), stat = "identity") +
  coord_flip() + 
  ggtitle("What emotions does Trump use?")
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

``` r
trump.sentiment %>%
  group_by(date, sentiment) %>%
  summarize(freq = length(date)) %>%
  filter(sentiment %in% c("positive", "negative", "trust", "fear", "anticipation", "anger")) %>%
  ggplot() + 
  geom_smooth(aes(x = date, y = freq, color = sentiment), span = .1, se = F) + 
  ggtitle("How do the emotions of Trump's tweets change over time?")
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-2.png)

IDEAS:

1.  When does Trump speak about people throughout the year?

2.  What words does he use to speak about them?

3.  Correlate with news/Google Trends.

4.  Correlate with popularity. See 538.

5.  Most commonly used words over time.

6.  Capitalization: Which words does he put in all caps?
