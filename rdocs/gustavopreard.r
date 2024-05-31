#Análise 1

library(tidyverse)

warner <- read_csv("banco/banco_final.csv")

factor_format <- factor(warner$format, order = TRUE, c("Serie", "CrossOver", "Movie"))
factor_format

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

#Análise 1 Número de lançamentos a cada década por formato de lançamento

```{r}

aone <- data.frame(warner$date_aired, warner$format)

aone <- aone %>%
  mutate(warner.date_aired = case_when(
    warner.date_aired %>% str_detect("196") ~ "1960",
    warner.date_aired %>% str_detect("1970") ~ "1960",
    warner.date_aired %>% str_detect("197") ~ "1970",
    warner.date_aired %>% str_detect("198") ~ "1980",
    warner.date_aired %>% str_detect("199") ~ "1990",
    warner.date_aired %>% str_detect("200") ~ "2000",
    warner.date_aired %>% str_detect("201") ~ "2010",
    warner.date_aired %>% str_detect("202") ~ "2020"
  )) %>% mutate(warner.format = case_when(
    warner.format %>% str_detect("Serie") ~ "Série",
    warner.format %>% str_detect("Movie") ~ "Filme",
    warner.format %>% str_detect("CrossOver") ~ "CrossOver",
     )) %>%
  group_by(warner.date_aired,warner.format)
  
aone <- aone %>% 
  summarise(freq = n()) %>%
mutate(
freq_relativa = round(freq / sum(freq) * 100,1))

aone<- aone %>% 
  rename(warner.caught = warner.caught_fred)


ggplot(aone) +
aes(x = warner.date_aired, y = freq, group = warner.format, colour = warner.format) +
geom_line(size = 1) +
geom_point(size = 2) +
labs(x = "Décadas", y = "Quantidade") +
theme_estat()
ggsave("linhas_aone.pdf", width = 158, height = 93, units = "mm")

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
round(var(First_Season$imdb), digits = 2)

Second_Season <- Serie %>% 
  filter(season %in% c(2))

Segunda <-  round((mean(Second_Season$imdb)), digits = 2)
round(sd((Second_Season$imdb)), digits = 2)
min(Second_Season$imdb)
quantile(Second_Season$imdb, 0.25)
median(Second_Season$imdb)
quantile(Second_Season$imdb, 0.75)
max(Second_Season$imdb)
round(var(Second_Season$imdb), digits = 2)

Third_Season <- Serie %>% 
  filter(season %in% c(3))

Terceira <- round((mean(Third_Season$imdb)), digits = 2)
round(sd((Third_Season$imdb)), digits = 2)
min(Third_Season$imdb)
quantile(Third_Season$imdb, 0.25)
median(Third_Season$imdb)
quantile(Third_Season$imdb, 0.75)
max(Third_Season$imdb)
round(var(Third_Season$imdb), digits = 2)

Fourth_Season <- Serie %>% 
  filter(season %in% c(4))

round(sd((Fourth_Season$imdb)), digits = 2)
min(Fourth_Season$imdb)
quantile(Fourth_Season$imdb, 0.25)
median(Fourth_Season$imdb)
quantile(Fourth_Season$imdb, 0.75)
max(Fourth_Season$imdb)
round(var(Fourth_Season$imdb), digits = 2)

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

---------------------------------------------------------------------------------------------------------------
  
  #Top 3 terrenos mais frequentes pela ativação da armadilha
  
  #setting_terrain e trap_work_first
  
warner <- warner %>% 
  filter(!is.na(trap_work_first))
terreno <- warner$setting_terrain
terreno
factor_terreno <- factor(terreno)
factor_terreno
contagem_terreno <- table(terreno)
contagem_terreno

#Os terrenos mais frequente são "Urban", "Rural" e "Forest"

terrenos <- warner %>% 
  filter(setting_terrain %in% c("Urban", "Rural", "Forest"))

terrenos <- warner %>%
  mutate(setting_terrain = case_when(
    setting_terrain %>% str_detect("Urban") ~ "Urbano",
    setting_terrain %>% str_detect("Rural") ~ "Rural",
    setting_terrain %>% str_detect("Forest") ~ "Floresta"
  )) %>%
  group_by(setting_terrain, trap_work_first) %>%
  filter(!is.na(setting_terrain)) %>% 
  filter(!is.na(trap_work_first)) %>% 
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

porcentagens <- str_c(terrenos$freq_relativa, "%") %>% str_replace("
\\.", ",")

legendas <- str_squish(str_c(terrenos$freq, " (", porcentagens, ")"))

colnames(terrenos)[colnames(terrenos) == "Armadilha funcionou de primeira"] <- "Armadilha_funcionou_de_primeira"

ggplot(terrenos) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = T), y = freq,
    fill = Armadilha_funcionou_de_primeira, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terreno", y = "Frequência") +
  theme_estat()
ggsave("analise3.pdf", width = 158, height = 93, units = "mm")

---------------------------------------------------------------------------------------------------------------

#Análise 4 
#Gráfico de dispersão e quadro de medidas resumo

engagement_imdb <- data.frame(warner$imdb, warner$engagement)

mr_imdb <- engagement_imdb %>%
  summarize("Média" = round(mean((warner.imdb)),2),
"Desvio Padrão" = round(sd((warner.imdb)),2),
"Variância" = round(var((warner.imdb)),2),
"Mínimo" = round(min((warner.imdb)),2),
"1º Quartil" = round(quantile((warner.imdb), probs =
.25),2),
"Mediana" = round(quantile((warner.imdb), probs = .5)
,2),
"3º Quartil" = round(quantile((warner.imdb), probs =
.75),2),
"Máximo" = round(max((warner.imdb)),2))

mr_engagement <- engagement_imdb %>%
  summarize("Média" = round(mean((warner.engagement)),2),
"Desvio Padrão" = round(sd((warner.engagement)),2),
"Variância" = round(var((warner.engagement)),2),
"Mínimo" = round(min((warner.engagement)),2),
"1º Quartil" = round(quantile((warner.engagement), probs =
.25),2),
"Mediana" = round(quantile((warner.engagement), probs = .5)
,2),
"3º Quartil" = round(quantile((warner.engagement), probs =
.75),2),
"Máximo" = round(max((warner.engagement)),2))

mr_engagement_imdb <- rbind(mr_engagement, mr_imdb)

ggplot(warner) +
aes(x = engagement, y = imdb) +
geom_point(colour = "#A11D21", size = 3) +
labs(
x = "Engajamento",
y = "Notas IMDB"
) +
  theme_estat()
ggsave("disp_pf.pdf", width = 158, height = 93, units = "mm")

---------------------------------------------------------------------------------------------------------------------------------------------------------------

# Análise 5 Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro

# caught_fred, caught_daphnie, caught_velma, caught_shaggy, caught_scooby, caught_other

# engagement

Fred <- data.frame(warner$caught_fred, warner$engagement)
Fred <- Fred %>% 
   filter(warner.caught_fred == TRUE) %>% 
  mutate(new_var = "Fred")
Fred<- Fred %>% 
  rename(warner.caught = warner.caught_fred)

mr_Fred <- Fred %>%
summarize("Média" = round(mean((warner.engagement)),2),
"Desvio Padrão" = round(sd((warner.engagement)),2),
"Variância" = round(var((warner.engagement)),2),
"Mínimo" = round(min((warner.engagement)),2),
"1º Quartil" = round(quantile((warner.engagement), probs =
.25),2),
"Mediana" = round(quantile((warner.engagement), probs = .5)
,2),
"3º Quartil" = round(quantile((warner.engagement), probs =
.75),2),
"Máximo" = round(max((warner.engagement)),2))
 
Daphnie <- data.frame(warner$caught_daphnie, warner$engagement)
Daphnie <- Daphnie %>% 
   filter(warner.caught_daphnie == TRUE) %>% 
  mutate(new_var = "Daphnie")
Daphnie<- Daphnie %>% 
  rename(warner.caught = warner.caught_daphnie)

mr_Daphnie <- Daphnie %>%
summarize("Média" = round(mean((warner.engagement)),2),
"Desvio Padrão" = round(sd((warner.engagement)),2),
"Variância" = round(var((warner.engagement)),2),
"Mínimo" = round(min((warner.engagement)),2),
"1º Quartil" = round(quantile((warner.engagement), probs =
.25),2),
"Mediana" = round(quantile((warner.engagement), probs = .5)
,2),
"3º Quartil" = round(quantile((warner.engagement), probs =
.75),2),
"Máximo" = round(max((warner.engagement)),2))

Velma <- data.frame(warner$caught_velma, warner$engagement)
Velma <- Velma %>% 
   filter(warner.caught_velma == TRUE) %>% 
  mutate(new_var = "Velma")
Velma<- Velma %>% 
  rename(warner.caught = warner.caught_velma)

mr_Velma <- Velma %>%
summarize("Média" = round(mean((warner.engagement)),2),
"Desvio Padrão" = round(sd((warner.engagement)),2),
"Variância" = round(var((warner.engagement)),2),
"Mínimo" = round(min((warner.engagement)),2),
"1º Quartil" = round(quantile((warner.engagement), probs =
.25),2),
"Mediana" = round(quantile((warner.engagement), probs = .5)
,2),
"3º Quartil" = round(quantile((warner.engagement), probs =
.75),2),
"Máximo" = round(max((warner.engagement)),2))


Salsicha <- data.frame(warner$caught_shaggy, warner$engagement)
Salsicha <- Salsicha %>% 
   filter(warner.caught_shaggy == TRUE) %>% 
  mutate(new_var = "Salsicha")
Salsicha<- Salsicha %>% 
  rename(warner.caught = warner.caught_shaggy)

mr_Salsicha <- Salsicha %>%
summarize("Média" = round(mean((warner.engagement)),2),
"Desvio Padrão" = round(sd((warner.engagement)),2),
"Variância" = round(var((warner.engagement)),2),
"Mínimo" = round(min((warner.engagement)),2),
"1º Quartil" = round(quantile((warner.engagement), probs =
.25),2),
"Mediana" = round(quantile((warner.engagement), probs = .5)
,2),
"3º Quartil" = round(quantile((warner.engagement), probs =
.75),2),
"Máximo" = round(max((warner.engagement)),2))

Scooby <- data.frame(warner$caught_scooby, warner$engagement)
Scooby <- Scooby %>% 
   filter(warner.caught_scooby == TRUE) %>% 
  mutate(new_var = "Scooby")
Scooby<- Scooby %>% 
  rename(warner.caught = warner.caught_scooby)

mr_Scooby <- Scooby %>%
summarize("Média" = round(mean((warner.engagement)),2),
"Desvio Padrão" = round(sd((warner.engagement)),2),
"Variância" = round(var((warner.engagement)),2),
"Mínimo" = round(min((warner.engagement)),2),
"1º Quartil" = round(quantile((warner.engagement), probs =
.25),2),
"Mediana" = round(quantile((warner.engagement), probs = .5)
,2),
"3º Quartil" = round(quantile((warner.engagement), probs =
.75),2),
"Máximo" = round(max((warner.engagement)),2))

Outro <- data.frame(warner$caught_other, warner$engagement)
Outro <- Outro %>% 
   filter(warner.caught_other == TRUE) %>% 
  mutate(new_var = "Outro")
Outro<- Outro %>% 
  rename(warner.caught = warner.caught_other)

mr_Outro <- Outro %>%
summarize("Média" = round(mean((warner.engagement)),2),
"Desvio Padrão" = round(sd((warner.engagement)),2),
"Variância" = round(var((warner.engagement)),2),
"Mínimo" = round(min((warner.engagement)),2),
"1º Quartil" = round(quantile((warner.engagement), probs =
.25),2),
"Mediana" = round(quantile((warner.engagement), probs = .5)
,2),
"3º Quartil" = round(quantile((warner.engagement), probs =
.75),2),
"Máximo" = round(max((warner.engagement)),2))

Doo <- rbind(Fred, Daphnie, Velma, Salsicha, Scooby, Outro)
Doo <- Doo %>% 
  select(new_var, warner.caught, warner.engagement) %>%
  rename(Personagens = new_var) %>% 
  rename(Capturou = warner.caught) %>% 
  rename(Engajamento = warner.engagement)

quadro_mr <- rbind(mr_Daphnie, mr_Fred, mr_Outro, mr_Salsicha, mr_Scooby, mr_Velma)
 
Doo <- Doo %>%
  mutate(Personagens = fct_reorder(Personagens, Engajamento, .fun = mean, .desc = TRUE))

?rbind

#mutate

#rbind

ggplot(Doo) +
aes(x = Personagens, y = Engajamento) +
geom_boxplot(fill = c("#A11D21"), width = 0.5) +
stat_summary(
fun = "median", geom = "point", shape = 23, size = 3, fill = "white"
) +
labs(x = "Personagens", y = "Engajamento") +
theme_estat()
ggsave("boxplot_a5.pdf", width = 158, height = 93, units = "mm")
19
