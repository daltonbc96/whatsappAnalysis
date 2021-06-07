library("rwhatsapp")
library("dplyr")
library("ggplot2")
library("hms")
library("lubridate")
library("stringr")
library('ggthemes')
library("tidyr")
library("stopwords")
library("tidytext")

setwd("~/Documentos/whatsapp analise")
chat <- rwa_read('Bonde.txt') %>% 
  filter(!is.na(author)) %>% 
  filter(text != '<Arquivo de mídia oculto>')


chat$date <- date(chat$time)
chat$date <- ymd(chat$date)
chat$hours <-  as_hms(chat$time)

#chat <- chat[,c("date","hours","author", "text", "emoji")]

chat %>%
  mutate(day = date) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_line(aes(x = day, y = n)) + 
  geom_smooth(aes(x = day, y = n, col = "Average"), se = FALSE, color = "blue") +
  ylab("Quantidade de Mensagens") + xlab("Período de Conversa") +
  ggtitle("Mensagens por dia")


chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 3, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Emojis mais utilizados")


##week 
wd <- chat %>%
  mutate(day = weekdays(date, abbreviate = FALSE)) %>%
  group_by(day) %>%
  summarise(n = n(),
            avg = n()/as.numeric(min(chat$date) %--% max(chat$date) , "weeks"))
wd$day <- ordered(wd$day, levels = c("domingo","segunda","terça","quarta","quinta","sexta","sábado"))
ggplot(wd, aes(x = day, y = avg)) + 
  geom_bar(aes(fill = day), show.legend = FALSE, stat="identity") +
  geom_text(aes(x = day, y = (avg+8), label = round(avg, 1))) +
  labs(x = "Dia da Semana",
       y = "") +
  ggtitle("Média de mensagens por dia da semana") +
  theme_hc() +
  theme(line = element_line(colour = "black"),
        panel.grid.minor.y = element_line(size = 0),
        panel.grid.major.y = element_line(size = 0)) +
  coord_flip() + 
  scale_y_continuous(position = "right")



# Time of day
tod <- chat %>%
  mutate(hour = as.numeric(substr(hours,1,2)),
         min = as.numeric(substr(hours,4,5)),
         sec = as.numeric(substr(hours,7,8)),
         mins = (hour*60)+min) %>%
  count(mins)

ggplot(tod, aes(x=mins, y = n/as.numeric(min(chat$date) %--% max(chat$date) , "days"))) + 
  geom_line(color = "#89f285") + 
  geom_smooth(se = FALSE, color = "blue") +
  labs(x = "Time of Day",
       y = "Average messages (per minute)") +
  scale_x_continuous(breaks = seq(from = 0, to = 1380, by = 60),
                     labels = c("Midnight", paste0(seq(from = 1, to = 11, by = 1), "am"), "Midday",
                                paste0(seq(from = 1, to = 11, by = 1), "pm"))) +
  #scale_y_continuous(breaks = c(0, 0.05, 0.1)) + 
  ggtitle("Volume of messages over the course of the day") +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45))



chat$text <- gsub('[[:punct:] ]+',' ',chat$text )
chat$text <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", chat$text))
chat$text <- gsub('([[:alpha:]])\\1+', '\\1', chat$text )





to_remove <- c(stopwords('pt'), "q", 'w',"htps","pra", "é", "vc","k" ,"kk", "kkkkk", "n", "nao", "sim", "Arquivo", "de", "mídia", "oculto", "arquivo", "c", 'ta', 'to', 'vou', 'tá', 'pq', 'eh', 'tô', 'https', 'ja', 'tb', "tbm", "vai", "iso", "mto", "isso", "acho")

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 3, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Palavras mais frequentemente utilizadas")

# VOCABULARY DIVERSITY
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Diversidade Lexical") +
  xlab("Usuário") +
  ggtitle("Diversidade lexical na conversa") +
  coord_flip()





x <- setNames(data.frame(table(chat$date)),c("Date","Count"))

library(plotly)


plot_ly(x, x = ~Date, y = ~Count, type = 'scatter', mode = 'lines')


#https://cran.r-project.org/web/packages/rwhatsapp/vignettes/Text_Analysis_using_WhatsApp_data.html
#https://github.com/chrisbrownlie/whatsapp-analysis-blogpost/blob/master/script.R
#https://medium.com/data-slice/3-5-years-of-a-relationship-in-whatsapp-messages-4f4c95073c9d
#https://medium.com/analytics-vidhya/chat-analysis-on-whatsapp-part-2-sentiment-analysis-and-data-visualization-with-r-f148592fa1b0