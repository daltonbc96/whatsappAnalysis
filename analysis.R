library("rwhatsapp")
library("dplyr")
library("hms")
library("lubridate")
library("ggplot2")
library("ggthemes")
library("plotly")
library("tidyr")
library("ggimage")

Sys.setlocale("LC_TIME", "Portuguese")
Sys.getlocale("LC_TIME")

#### Carregando os dados ####
chat <- rwa_read('chat.txt') %>% 
  filter(!is.na(author)) %>% 
  filter(text != '<Arquivo de mídia oculto>')%>% 
  mutate(
    # date = ymd(date(time)),
    #  hours = as_hms(time),
    text = iconv(text, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
    text = lapply(text, tolower),
    text = gsub('[[:punct:] ]+',' ',text ),
    text = trimws(gsub("\\w*[0-9]+\\w*\\s*", "", text)),
    text = gsub('([[:alpha:]])\\1+', '\\1', text )
  )


#### Dados descritivos ####


#Primeiro dia
min(chat$date)
#Última dia
max(chat$date)
#Dias ativos
length(unique(chat$date))
#Diferença do último para o primeiro dia
max(chat$date) - min(chat$date)
#Número de semanas 
as.numeric(min(chat$date) %--% max(chat$date), "weeks")



#### Número de mensagens ao longo do tempo ####

fig <-chat %>%
  mutate(day = date) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_line(aes(x = day, y = n), color="#F8766D") + 
  geom_smooth(aes(x = day, y = n, col = "Average"), se = FALSE, color = "#00BFC4", pch=7) +
  ylab("Quantidade de Mensagens") + xlab("Data da Conversa") +
  ggtitle("Número de mensagens do Whatsapp enviadas por dia")+
  theme_hc()
ggplotly(fig)


#### Média de mensagens por dia da semana ####

weeks <- as.numeric(min(chat$date) %--% max(chat$date) , "weeks")

wd <- chat %>%
  mutate(day = weekdays(date, abbreviate = FALSE)) %>%
  group_by(day) %>%
  summarise(n = n(), avg = n() / weeks)%>%
  mutate(day = ordered(day,levels = c("sábado","sexta-feira", "quinta-feira",
                                      "quarta-feira", "terça-feira",
                                      "segunda-feira", "domingo")))

ggplot(wd, aes(x = day, y = avg)) +
  geom_bar(aes(fill = day), show.legend = FALSE, stat = "identity") +
  geom_text(aes(
    x = day,
    y = (avg + 8),
    label = round(avg, 1)
  )) +
  labs(x = "Dia da Semana",
       y = "") +
  theme_hc() +
  theme(
    line = element_line(colour = "black"),
    panel.grid.minor.y = element_line(size = 0),
    panel.grid.major.y = element_line(size = 0)
  ) +
  coord_flip() +
  scale_y_continuous(position = "right")


#### Volume de mensagens ao longo do dia ####

days <- as.numeric(min(chat$date) %--% max(chat$date) , "days")

tod <- chat %>%
  mutate(
    hour = as.numeric(substr(hours, 1, 2)),
    min = as.numeric(substr(hours, 4, 5)),
    sec = as.numeric(substr(hours, 7, 8)),
    Minuto = (hour * 60) + min
  ) %>%
  count(Minuto) %>%
  mutate(Media = n / days)

fig2 <-
  ggplot(tod, aes(x = Minuto,
                  y = Media)) +
  geom_line(color = "#F8766D") +
  geom_smooth(se = FALSE, color = "#00BFC4") +
  labs(x = "Horário do dia",
       y = "Média de mensagens (por minuto)") +
  scale_x_continuous(
    breaks = seq(from = 0, to = 1380, by = 60),
    labels = c(
      "Meia-noite",
      paste0(seq(
        from = 1, to = 11, by = 1
      ), "am"),
      "Meio-dia",
      paste0(seq(
        from = 1, to = 11, by = 1
      ), "pm")
    )
  ) +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45))
ggplotly(fig2)



#### Emojis usados com maior frequência ####

emoji_data <- rwhatsapp::emojis %>%
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>%
  mutate(emoji_url = paste0(
    "https://abs.twimg.com/emoji/v2/72x72/",
    tolower(hex_runes1),
    ".png"
  ))

chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 5, n) %>%
  left_join(emoji_data, by = "emoji") %>%
  ggplot(aes(
    x = reorder(emoji, n),
    y = n,
    fill = author
  )) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~ author, ncol = 2, scales = "free_y") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



#### Nuvem de palavras ####

to_remove <- c(stopwords('pt'),"q",'w',"htps","pra","tÃ¡","vc", 'lindinho', 'pepita',
               "k","n","nao","sim","Arquivo","de","mídia","oculto","arquivo","c",'ta','to','vou','tão','pq','eh','https','ja','tb',"tbm","vai","iso","mto","isso","acho","td","haha","hahaha",'rsrs','kk','há','ti','tá´')

words <- chat %>%
  filter(!is.na(text)) %>% 
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author)

matrix <- acast(words, word ~ author,  fill = 0)
comparison.cloud(matrix,  max.words = 50)


#### Diversidade lexical na conversa ####
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(
    x = reorder(author, lex_diversity),
    y = lex_diversity,
    fill = author
  )) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Diversidade Lexical") +
  xlab("Usuário") +
  coord_flip()

