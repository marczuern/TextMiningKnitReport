## Save Reddit dataset first, to be loaded here.
## Install tidytext, wordcloud packages.
library(tidyverse)
library(tidytext)
library(wordcloud)
library(xts)
library(tbl2xts)
library("plotly")
library(dygraphs)
library(DT)
library("formattable")

load(file = "TaylorSwift_50threads.rda")
str(threads_df)

# tidy table: text column to unite thread's title, text, and comments
threads_tbl <- as_tibble(threads_df) %>%
  unite(title, text, text_comments, col = "text", sep = " ")
threads_tbl
threads_tbl$text[1]

# tokenization:
# unnest_tokens removes punctuation, converts text to lower case
threads_words <- threads_tbl %>%
  unnest_tokens(word, text) %>%
  # omit most rare words: keep those occurring more than 10 times
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()
threads_words

# remove stop words (corpus available within tidytext)
head(stop_words$word)
threads_words_clean <- threads_words %>%
  filter(!word %in% stop_words$word) %>%
  filter(!is.na(word))
threads_words_clean

# term frequency (tf)
threads_words_count <- threads_words_clean %>% 
  group_by(word) %>% 
  summarise(count = n(), avg_up_ratio = mean(up_ratio), 
            total_comments = sum(comments), 
            total_upvotes = sum(upvotes),
            total_downvotes = sum(downvotes),
            total_awards = sum(total_awards_received)) %>%
  arrange(desc(count))
threads_words_count

onefive_words <- threads_words_count %>%
  top_n(15, count)
onefive_words

bar_onefive_words <- plot_ly(onefive_words,
               y = ~word,
               x = ~count,
               type = 'bar',
               hovertemplate = ~paste('Word: ', word, 
                                      '\nCount: ', count, 
                                      '\nAverage Up Ratio: ', round(avg_up_ratio, 2),
                                      '\nTotal Comments: ', total_comments)) %>%
  layout(title = "Top 15 Words by Apperance",
         xaxis = list(title = "Apperance Count"),
         yaxis = list(title = "Words"))

bar_onefive_words

## interactive table
onefive_words_v2 <- threads_words_count[,c("word", "count", "avg_up_ratio","total_comments","total_upvotes","total_downvotes")]
datatable(onefive_words)

