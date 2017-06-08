# Analyse the previous several annual  Corporate Reponsibility
# Reports (CRR) using text mining to understand if these reflect changes in
# emphasis or importance of topics to a bank over the years
# Mark Wilcock, June 2017


# next steps:
# - bank buzzword bingo, extract all the acronyms (words with two or more capitals at start)
# - bigram analysis - e.g. plastic bags

# next line required when code transferred to PBI
setwd("C:/Users/markw/Zomalex Ltd/OneDrive - Zomalex Ltd/Demos/CRR Text Mining")

library(tidytext)
library(pdftools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(stringr)
library(wordcloud)
library(ggthemes)
library(readxl)
library(readr)
library(purrr)
library(vwr) # contains a dictionary of english words
library(wordcloud)
library(RColorBrewer)


# read hot words from the Excel source file
hot_list <-  read_excel("CRR Custom Words.xlsx", sheet = "hot_words")
hot_words <-
  hot_list %>% 
  gather(key = "category", value = "word") %>% 
  filter(!is.na(word))

crr_stop_words <-  
  read_excel("CRR Custom Words.xlsx", sheet = "stop_words") %>% 
  select(word = stop_words) %>% 
  mutate(lexicon = "CRR")

all_stop_words <- bind_rows(stop_words, crr_stop_words)


get_text_from_crr_pdf <- function(year, to_lower = TRUE) {
# Returns a data frame with 3 columns
# - pagenumber - of the page of the CRR report
# - year of the CRR report 
# - word (all stop words removed and any other unigram that is not made of of letters)
    filename = paste0("PDFs/csg-crr-", year, "-en.pdf")
  
    data_frame(page = pdf_text(filename)) %>%
    mutate(
      pagenumber = row_number(), 
      year = year) %>%
      unnest_tokens(word, page, to_lower = to_lower) %>% 
      mutate(word = str_extract(word, "[A-Za-z']+")) %>%  # clean up of any PDF chars not removed by previous operations
      filter(!is.na(word)) %>% # previous operation may have created NAs if all chars were not letters
      anti_join(all_stop_words)
}

useful_words <- bind_rows(
  get_text_from_crr_pdf('2016'),
  get_text_from_crr_pdf('2015'),
  get_text_from_crr_pdf('2014'),
  get_text_from_crr_pdf('2013'),
  get_text_from_crr_pdf('2012'),
  get_text_from_crr_pdf('2011'),
  get_text_from_crr_pdf('2010'))

#  There reports were over 18,000 words in 13 & 14, but quite a bit shorter (under 14,000 words) in 15 and 16 
# total_words_per_year <- 
# useful_words %>% count(year)


# get the top N words across all reports  
top_words <- useful_words %>% 
  count(word, sort = TRUE) %>% 
  mutate(ranking = min_rank(desc(n))) %>%
  ungroup() %>%
  filter(ranking <= 20) 

# Plot simple bar chart of top N words overall ordered by word count
ggplot(top_words, aes(x = fct_reorder(word, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_light() +
  ggtitle(label = "Top N Words Overall", 
          subtitle = "Lots of strong and stable words") +
  xlab("Word") +
  ylab("Number of occurrences over all reports")

top_words_by_year <- 
  useful_words %>% 
  count(year, word, sort = TRUE) %>% 
  mutate(ranking = min_rank(desc(n))) %>%
  ungroup() %>%
  semi_join(top_words, by = "word")

# calculate the proportion of each word in each annual report
# then tag  them with hot word categories

proportions <-  
  useful_words %>%
  count(year, word) %>%
  mutate(proportion = n / sum(n))  %>% 
  ungroup() %>%
  select (-n) %>%
  spread(year, proportion) %>%
  left_join(hot_words) %>% 
  mutate(category = ifelse(is.na(category), 'none', category))

#write.csv(proportions, file="crr-proportions.csv")

# now create a dataset to make it possible to compare  the 2016 proportions 
# against the three earlier years.
# This keeps the 2016 column, but now creates a new year column (with 2013/14/15 values)
# and a new proportion column

proportion_comparison16  <-
  proportions %>% gather(year, proportion, `2010`:`2015`)
#write.csv(proportion_comparison16, file="crr-proportion_comparison16.csv")


# plot a bump chart of the changing ranking of the most frequent words over time
# ggplot(top_words_by_year, aes(x = year, y = ranking, group = word, colour = word)) +
#   geom_line() +
#   geom_text(aes(label = word), check_overlap = TRUE ) +
#   #scale_y_reverse() +
#     ggtitle(label =   "Word popularity in 2016 report against previous years",
#             subtitle = "Profile of top words over each year") +
#   theme_light() +
#   theme(legend.position = "none") 

#ggsave(filename = "crr top words bump.png", device = "png", width = 8, height = 4)

 
year_colours <- brewer.pal(7, "Dark2")
# Plot facted bar charts showing the frequency of the most common words across the years
ggplot(top_words_by_year, aes(x = year, y = n, fill = year)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~word, ncol = 5) +
  ggtitle("Top 20 Most Popular Words Over The Years", 
          "Shows which words increase or reduce in popularity (and therefore importance to the firm?) over the 4 years") +
  scale_y_continuous("Number Of Words1") +
  scale_x_discrete("Year of report") +
  #scale_fill_tableau(palette = "tableau10medium") +
  scale_fill_manual(values = year_colours) +
  #scale_fill_economist()
#scale_fill_manual(values = c("red", "yellow", "green", "blue")) +
  theme_light() +
  theme(legend.position = "right")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # year labels rotated to fit

#ggsave(filename = "crr top 20 words by year bar.png", device = "png", width = 8, height = 4)
  
# let's plot it to discover words that are frequent in the 2016 report but not in the earlier reports
# scatter plot of relative proprtions
ggplot(proportion_comparison16, aes(x = proportion, y = `2016`, color = abs(`2016` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent) +
  scale_y_log10(labels = percent) +
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~year, ncol = 3) +
  labs(y = "2016", x = NULL) +
  theme_light() +
  theme(legend.position = "none") +
  ggtitle(label =   "Word popularity in 2016 report against previous years",
           subtitle = "Words above and to the left of the dotted line are more common in 2016 than previous years.")

#ggsave(filename = "frequency 2016 vs prev years basic.png", device = "png", width = 8, height = 4)


# # plot as before but show category by colour
# ggplot(proportion_comparison16, aes(x = proportion, y = `2016`, color = category)) +
#   geom_abline(color = "gray40", lty = 2) + 
#   geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
#   geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
#   scale_x_log10(labels = percent) +
#   scale_y_log10(labels = percent) +
#   scale_color_manual(values = 
#                        c(climate = "darkred", conduct = "seagreen", diversity = "skyblue", risk = "brown", none = "grey80")) +
#   #scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75") +
#   facet_wrap(~year, ncol = 3) +
#   labs(y = "2016", x = NULL) +
#   theme_light() +
#   #theme(legend.position = "none") +
#   ggtitle(label =   "Word popularity in 2016 report against previous years",
#           subtitle = "Words above and to the left of the dotted line are more common in 2016 than previous years.")

# plot as before but show hot words by colour
# label only the hot words

comp16_short <-  proportion_comparison16 %>% filter(year %in% c("2010", "2012", "2014"))
comp16_short_cold <-  comp16_short %>%  filter(category == "none")
comp16_short_hot <-  comp16_short %>%  filter(category != "none")

p <- brewer.pal(4, "Dark2") 
legend_colours <- c(climate = p[1], conduct = p[2], diversity = p[3], risk = p[4], none = "grey80")
#legend_colours <- c(climate = "coral", conduct = "seagreen4", diversity = "royalblue4", risk = "brown", none = "grey80")
legend_order <- c("climate", "conduct", "diversity", "risk", "none")
#legend_order <- c(climate, conduct, diversity, risk, none)
ggplot(comp16_short, aes(x = proportion, y = `2016`, color = category)) +
  geom_abline(color = "gray40", lty = 2) + 
  # show the cold words as a mainly transparent jittered background
  geom_jitter(data = comp16_short_cold, alpha = 0.3, size = 2.5, width = 0.3, height = 0.3) +
  # show the hot words exactly on location (no jitter) in the category colour
  geom_text(data = comp16_short_hot, aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  # show the hot word labels
  geom_point(data = comp16_short_hot, alpha = 0.8) +
  scale_x_log10(labels = percent) +
  scale_y_log10(labels = percent) +
  scale_color_manual(values = legend_colours, breaks = legend_order)  +
  #scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~year, ncol = 3) +
  labs(y = "2016", x = NULL) +
  theme_light() +
  theme(legend.position = "bottom") +
  ggtitle(label =   "Frequency of hot words in 2016 vs previous year's reports",
          subtitle = "Note that the choice of hot words is subjective and reflects the analyst's view.")


# TF IDF analysis

# There are some issues with the PDF especially the 2010 PDF that need to be
# resolved before we can do successful TF IDF analysis. Some perfectly good
# words become split and hyphenated e.g. S -uisse. The PDF is in a 2 column
# newspaper layout and these become the same row is the parsed text - so
# sentence analysis would be difficult.

# dict_words contains 63K english words + a few specific  stop words
dict_words <- 
  data_frame(word = english.words) %>% 
  bind_rows(select(crr_stop_words, word))

#unusual_words are those words not found thie disctionary above
unusual_words <- useful_words %>% anti_join(dict_words) %>%  select(word) %>% unique

# some of these arise since the PDF parsing splits words
# It is best to ignore these broken words
# so tag these words if they are a fragment of the word in the dictionary 

# returns TRUE if possible_word_part is a fragment of any of the words in dict-words
is_part_of_word <- function(possible_word_part) {
  str_detect(dict_words, possible_word_part)
}

# Next step takes about 15 minutes 
# so have cached the word_fragments in a  file

# is_word_fragment <- map_lgl(unusual_words$word, is_part_of_word)
# 
# word_fragments <-
#   data_frame(word = unusual_words$word, is_word_fragment) %>%
#   filter(is_word_fragment == TRUE) %>%
#   select(-is_word_fragment)
# 
# write_csv(x = word_fragments,path = "word_fragments.csv")

word_fragments <-  read_csv("word_fragments.csv")

proportions_tdf <-
  useful_words %>%
  count(year, word) %>%
  mutate(proportion = n / sum(n))  %>% 
  ungroup() %>% 
  select(-proportion) %>% 
  anti_join(word_fragments) %>%
  bind_tf_idf(word, year, n) %>% 
  group_by(year) %>% 
  mutate(word2 = fct_reorder(word, tf_idf)) %>% 
  arrange(desc(tf_idf)) %>%
  top_n(20) %>% 
  ungroup() 
  
  
  #write_csv(path = "temp.csv")
ggplot(proportions_tdf, aes(x = word2, y = tf_idf, fill = year)) +
    geom_col() +
    scale_y_continuous("TF - IDF", labels = NULL) +
    scale_fill_manual(values = brewer.pal(7, "Dark2")) +
    facet_wrap(~ year, ncol = 7, scales = "free") +
    coord_flip() +
    #labs(y = NULL, x = NULL) +
    theme_light() +
    theme(legend.position = "bottom") +
    ggtitle(label =   "Most frequent words appearing in only one or a few reports only",
          subtitle = "Uses the TF - IDF algorithm.")

# Bank Buzzword Bingo
case_sense_words <- bind_rows(
  get_text_from_crr_pdf('2016', to_lower = FALSE),
  get_text_from_crr_pdf('2015', to_lower = FALSE),
  get_text_from_crr_pdf('2014', to_lower = FALSE),
  get_text_from_crr_pdf('2013', to_lower = FALSE),
  get_text_from_crr_pdf('2012', to_lower = FALSE),
  get_text_from_crr_pdf('2011', to_lower = FALSE),
  get_text_from_crr_pdf('2010', to_lower = FALSE))


acronyms <- case_sense_words %>% 
  filter(str_detect(word, "[A-Z][A-Z]")) %>% 
  count(word, sort = TRUE) %>% 
  mutate(ranking = min_rank(desc(n))) %>%
  ungroup() 

#acronyms %>%  write_csv(path = "acronym.csv")

acronyms %>%
  filter(!word %in% c("USD", "CHF", "AG")) %>% 
  with(wordcloud(word, n, max.words = 400))

# acronyms %>% 
# #  arrange(desc(n)) %>% 
#   top_n(20, n) %>% 
#   ggplot(aes(x = fct_reorder(word, n), y = n)) + 
#   geom_bar(stat = "identity") +
#   ggtitle("Most Popular Acronyms Over The Years", 
#           "How many do you know?") +
#   scale_y_continuous("Number of occurences") +
#   scale_x_discrete("Acronym") +
#   coord_flip() +
#   theme_light()

case_sense_words %>% 
  filter(str_detect(word, "[A-Z][A-Z]")) %>% 
  count(year, word, sort = TRUE) %>%  # counting by year and word this time
  mutate(wordF = fct_reorder(word, n)) %>% 
  #arrange(desc(n)) %>%
  top_n(20, n)  %>% 
  ggplot(aes(x = word, y = n, fill = year)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~year, ncol = 7, scales = "free") +
  ggtitle("Most Popular Acronyms Over The Years", 
          "How many do you know?") +
  scale_y_continuous("Number") +
  scale_x_discrete("Year of report") +
  #scale_fill_tableau(palette = "tableau10medium") +
  scale_fill_manual(values = year_colours) +
  coord_flip() +
  theme_light() +
  theme(legend.position = "bottom")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # year labels rotated to fit



