Trump’s Tweets
================
Elias M. Guerra
4/19/2018

``` r
library(rjson)
trump.tweets <- fromJSON(file = "~/Documents/R/math311/condensed_2017.json")
length(trump.tweets) # 2605
```

    ## [1] 2605

``` r
trump <- matrix(nrow = 2605, ncol = 2)
for (i in 1:2605) {
  trump[i,] <- c(trump.tweets[[i]]$created_at,
                 trump.tweets[[i]]$text)
}
trump <- as.data.frame(trump)
colnames(trump) <- c("date.time", "text")
trump$date <- as.Date(substr(trump$date.time, 1, 10), format = "%a %b %d %t")
trump$time <- substr(trump$date.time, 12, 19)
```

Let's take a peak at our dataset:

| date.time                      | text                                                                                                                                                                                                                                                                                     | date       | time     |
|:-------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------|:---------|
| Mon Jan 01 13:37:52 +0000 2018 | Will be leaving Florida for Washington (D.C.) today at 4:00 P.M. Much work to be done, but it will be a great New Year!                                                                                                                                                                  | 2018-01-01 | 13:37:52 |
| Mon Jan 01 12:44:40 +0000 2018 | Iran is failing at every level despite the terrible deal made with them by the Obama Administration. The great Iranian people have been repressed for many years. They are hungry for food & for freedom. Along with human rights, the wealth of Iran is being looted. TIME FOR CHANGE!  | 2018-01-01 | 12:44:40 |
| Mon Jan 01 12:12:00 +0000 2018 | The United States has foolishly given Pakistan more than 33 billion dollars in aid over the last 15 years, and they have given us nothing but lies & deceit, thinking of our leaders as fools. They give safe haven to the terrorists we hunt in Afghanistan, with little help. No more! | 2018-01-01 | 12:12:00 |
| Sun Dec 31 23:43:04 +0000 2017 | HAPPY NEW YEAR! We are MAKING AMERICA GREAT AGAIN, and much faster than anyone thought possible!                                                                                                                                                                                         | 2018-12-31 | 23:43:04 |
| Sun Dec 31 22:18:20 +0000 2017 | As our Country rapidly grows stronger and smarter, I want to wish all of my friends, supporters, enemies, haters, and even the very dishonest Fake News Media, a Happy and Healthy New Year. 2018 will be a great year for America!                                                      | 2018-12-31 | 22:18:20 |
| Sun Dec 31 22:00:21 +0000 2017 | Iran, the Number One State of Sponsored Terror with numerous violations of Human Rights occurring on an hourly basis, has now closed down the Internet so that peaceful demonstrators cannot communicate. Not good!                                                                      | 2018-12-31 | 22:00:21 |
| Sun Dec 31 19:06:52 +0000 2017 | What a year it’s been, and we're just getting started. Together, we are MAKING AMERICA GREAT AGAIN! Happy New Year!! <https://t.co/qsMNyN1UJG>                                                                                                                                           | 2018-12-31 | 19:06:52 |
| Sun Dec 31 18:36:28 +0000 2017 | My deepest condolences to the victims of the terrible shooting in Douglas County @DCSheriff, and their families. We love our police and law enforcement - God Bless them all! \#LESM                                                                                                     | 2018-12-31 | 18:36:28 |
| Sun Dec 31 13:36:32 +0000 2017 | Why would smart voters want to put Democrats in Congress in 2018 Election when their policies will totally kill the great wealth created during the months since the Election. People are much better off now not to mention ISIS, VA, Judges, Strong Border, 2nd A, Tax Cuts & more?    | 2018-12-31 | 13:36:32 |
| Sun Dec 31 13:26:29 +0000 2017 | If the Dems (Crooked Hillary) got elected, your stocks would be down 50% from values on Election Day. Now they have a great future - and just beginning! <https://t.co/9TzSC8F8vY>                                                                                                       | 2018-12-31 | 13:26:29 |
