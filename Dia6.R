# Primeiros passos
setwd("~/Documents/Bootcamp/Dia6")
getwd()

# Bibliotecas
library(corrplot)
library(psych)
library(dplyr)
library(ggplot2)
library(cowplot)

# Paletas de cores
cores <- colorRampPalette(colors = c("#4d4d4d", "#ffffff", "#5288db"))
cores_cat <- c("gray70", "#5288db")

# Importando os dados

library(readr)
case_desempenho <- read_csv("case_desempenho.csv")

summary(case_desempenho)

data <- case_desempenho[ , -1]

# Explorando os dados

hist(data$ninebox)


df_num <- data %>% select_if(is.numeric)
Mcor <- cor(df_num)
corrplot(Mcor, method = "color", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.7, col = cores(50))


data$genero <- factor(data$genero,
                          levels = c(0, 1),
                          labels = c("Feminino", "Masculino"))

summary(data)


ggplot(data) + 
  aes(idade, fill = genero) + 
  geom_histogram(bins = 15, position = "identity", alpha = 0.5) +
  theme_light() + 
  scale_fill_manual(values = cores_cat)

ggplot(data) + 
  aes(ninebox, fill = genero) + 
  geom_bar() + 
  theme_light() +
  scale_fill_manual(values = cores_cat)

graficoBarras <- function(data, variavel, split) {
  tabela_graf <- function(data, variavel, split) {
    df <- data %>%
      group_by({{variavel}}, {{split}}) %>%
      summarise(qtde = n(), .groups = 'drop') %>%
      group_by({{variavel}}) %>%
      mutate(percentual_grupo = 100 * qtde / sum(qtde)) %>%
      ungroup() %>%
      mutate(percentual_geral = 100 * qtde / sum(qtde))
    return(df)
  }
  
  df <- tabela_graf(data, {{variavel}}, {{split}})
  
  p1 <- ggplot(df, aes(x = {{variavel}}, y = qtde, fill = {{split}})) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = cores_cat) +
    geom_text(aes(label = qtde), position = position_stack(vjust = .5)) +
    labs(title = 'Frequência absoluta') +
    theme_light() +
    theme(panel.grid = element_blank())
  
  
  p2 <- ggplot(df, aes(x = {{variavel}}, y = percentual_geral, fill = {{split}})) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = cores_cat) +
    geom_text(aes(label = round(percentual_geral, 1)), position = position_stack(vjust = .5)) +
    labs(y = 'percentual (%)',
         title = 'Proporção em relação ao total') +
    theme_light() +
    theme(panel.grid = element_blank())
  
  p3 <- ggplot(df, aes(x = {{variavel}}, y = percentual_grupo, fill = {{split}})) +
    geom_col() +
    scale_fill_manual(values = cores_cat) +
    geom_text(aes(label = round(percentual_grupo, 1)), position = position_stack(vjust = .5)) +
    labs(y = 'percentual (%)',
         title = 'Proporção em relação a categoria') +
    theme_light() +
    theme(panel.grid = element_blank()) +
    coord_flip()
  
  plot_grid(p1, p2, p3, nrow = 1, rel_widths = c(3, 3, 4))
}


graficoBarras(data, ninebox, genero)
graficoBarras(data, nivel_cargo, genero)


L_superior <- data[data$ninebox >= 7, ]
L_inferior <- data %>% filter(ninebox <= 3)


data$desempenho_cat <- "baixo"
data$desempenho_cat[data$ninebox == 3 | data$ninebox == 5 | data$ninebox == 7] <- "médio" 
data$desempenho_cat[data$ninebox == 6 | data$ninebox == 8 | data$ninebox == 9] <- "alto"



top_performers <- data[data$ninebox >= 7, ]
bottom_performers <- data %>% filter(ninebox <= 6)

summary(top_performers)
summary(bottom_performers)


data$ninebox_cat <- "inferior"
data$ninebox_cat[data$ninebox == 4 | data$ninebox == 5 | data$ninebox == 6] <- "médio" 
data$ninebox_cat[data$ninebox == 7 | data$ninebox == 8 | data$ninebox == 9] <- "superior"
