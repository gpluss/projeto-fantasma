#Análise 1

library(tidyverse)

warner <- read_csv("banco/banco_final.csv")

factor_format <- factor(warner$format, order = TRUE, c("Serie", "CrossOver", "Movie"))
factor_format


ggplot(warner) +
  aes(x=date_aired, y=factor_format, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",size=3) +
  labs(x="Décadas", y="Formatos")

ggplot(warner) +
  aes(x = date_aired, y = format) +
  geom_point(colour = "#A11D21", size = 2) +
  labs(
    x = "data",
    y = "formato"
  )

ggplot(warner) +
  aes(x = date_aired, y = factor_format, group = format, colour = format) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Formato")

view(mpg)

estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )

estat_colors

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5)
      ,
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

trans_drv <- mpg %>%
  mutate(trans = case_when(
    trans %>% str_detect("auto") ~ "auto",
    trans %>% str_detect("manual") ~ "manual"
  )) %>%
  group_by(trans, drv) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

warner$date_aired <- str_extract(warner$date_aired, "\\d{4}")

date_format <- warner %>%
  mutate(format= case_when(
    format %>% str_detect("CrossOver") ~ "CrossOver",
    format %>% str_detect("Movie") ~ "Filme",
    format %>% str_detect("Serie") ~ "Série"
  )) %>%
  group_by(date_aired, format) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

date_format <- date_format %>%
  mutate(format= case_when(
    date_aired %>% str_detect("CrossOver") ~ "CrossOver",
    format %>% str_detect("Movie") ~ "Filme",
    format %>% str_detect("Serie") ~ "Série"
  ))

porcentagens <- str_c(date_format$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(date_format$freq, " (", porcentagens, ")")
)

ggplot(date_format) +
  aes(
    x = date_aired, y = freq,
    fill = format, label = date_aired
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Décadas", y = "Frequência") +
  theme_estat()
ggsave("colunas-ps.pdf", width = 158, height = 93, units = "mm")

---------------------------------------------------------------------------------------------------------------
  #Análise 2
  
#Variação da nota IMDB por temporada dos episódios

Serie <- warner %>% 
  filter(format %in% c("Serie"))

First_Season <- Serie %>% 
  filter(season %in% c(1))

Primeira_média <- round((mean(First_Season$imdb)), digits = 2)
round(sd((First_Season$imdb)), digits = 2)
min(First_Season$imdb)
quantile(First_Season$imdb, 0.25)
median(First_Season$imdb)
quantile(First_Season$imdb, 0.75)
max(First_Season$imdb)


Second_Season <- Serie %>% 
  filter(season %in% c(2))

Segunda <-  round((mean(Second_Season$imdb)), digits = 2)
round(sd((Second_Season$imdb)), digits = 2)
min(Second_Season$imdb)
quantile(Second_Season$imdb, 0.25)
median(Second_Season$imdb)
quantile(Second_Season$imdb, 0.75)
max(Second_Season$imdb)

Third_Season <- Serie %>% 
  filter(season %in% c(3))

Terceira <- round((mean(Third_Season$imdb)), digits = 2)
round(sd((Third_Season$imdb)), digits = 2)
min(Third_Season$imdb)
quantile(Third_Season$imdb, 0.25)
median(Third_Season$imdb)
quantile(Third_Season$imdb, 0.75)
max(Third_Season$imdb)

Fourth_Season <- Serie %>% 
  filter(season %in% c(4))

round(sd((Fourth_Season$imdb)), digits = 2)
min(Fourth_Season$imdb)
quantile(Fourth_Season$imdb, 0.25)
median(Fourth_Season$imdb)
quantile(Fourth_Season$imdb, 0.75)
max(Fourth_Season$imdb)

Quarta <- mean(Fourth_Season$imdb)

df_seasons <- data.frame(Primeira, Segunda, Terceira, Quarta)

Temporadas <- c(1, 2, 3, 4)
Media_IMDB <- c(5.04, 5.19,5.20, 5.24)

df_seasons <- data.frame(Temporadas, Media_IMDB)

Serie <- Serie %>% 
  filter(season %in% c(1, 2, 3, 4))

ggplot(df_seasons) +
  aes(Temporadas) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = ) +
  labs(x = "Temporadas", y = "Médias das notas IMDB") +
  theme_estat()

ggplot(Serie) +
  aes(x = reorder(season, imdb), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Temporadas", y = "Nota IMDB") +
  theme_estat()
ggsave("boxplot_seasons.pdf", width = 158, height = 93, units = "mm")