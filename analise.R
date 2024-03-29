# Setando o ambiente
setwd("C:/BootCamp People Analytics")

# Libraries
library(readr)
library(corrplot)
library(psych)
library(dplyr)
library(ggplot2)
library(cowplot)

# Importando os dados
data <- read_csv("case_desempenho.csv")

# definindo fator
data$genero <- factor(data$genero,
                      levels = c(0,1),
                      labels = c('Mulher', 'Homem'))
summary(data)

# definindo a paleta de cores
cores <- colorRampPalette(colors = c("#4d4d4d", "#ffffff", "#5288db"))
cores_cat <- c("#FA0505", "#5288db")

# correla��o
df_num <- data %>% 
  select_if(is.numeric) %>%
  select(-1)

# An�lise de correla��o
mcor <- cor(df_num)

# Corr plot
corrplot(mcor, method = "color", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.7, col = cores(50))

# Histograma
hist(data$ninebox)

data$desempenho_cat <- "baixo"
data$desempenho_cat[data$ninebox == 3 | data$ninebox == 5 | data$ninebox == 7] <- "m�dio" 
data$desempenho_cat[data$ninebox == 6 | data$ninebox == 8 | data$ninebox == 9] <- "alto"

# Correla��es positivas(fracas):
# Hora extra x nivel_cargo
# tempo_casa x hora_extra
# tempo_casa x idade
# nine box x nivel cargo

# Correla��es negativas:
# idade x nivel_cargo
# idade x hora_extra
# nine_box x hora_extra
# Idade x nine_box(correla��o mais forte da amostra)
# nine_box x tempo_casa

## Analise Univariada
# Fun��o customizada pra analise univariada(do dia 3)
analiseUnivariada <- function(dataset, variavel, split){
  library(ggplot2)
  library(cowplot)
  g1 <- ggplot(dataset, aes_string(variavel)) +
    geom_histogram(aes_string(fill=split), position = "stack", bins=10,
                   alpha=.5, show.legend = FALSE) +
    scale_fill_manual(values = cores_cat) +
    theme_light() + labs(title = "Histograma empilhado")
  
  g2 <- ggplot(dataset, aes_string(variavel)) +
    geom_histogram(aes_string(fill=split), position = "identity", bins=10,
                   alpha=.5, show.legend = FALSE) +
    scale_fill_manual(values = cores_cat) +
    theme_light() + labs(title = "Histograma sobreposto")
  g3 <- ggplot(dataset, aes_string(variavel)) +
    geom_density(aes_string(colour=split), position = "identity", alpha=.5) +
    scale_color_manual(values = cores_cat) +
    theme_light() + labs(title = "Densidade de probabilidade")
  
  plot_grid(g1,g2,g3, nrow=1)
}

# Genero x Nivel do cargo
analiseUnivariada(data,data$nivel_cargo,data$genero) 
# Mesmo havendo menos mulheres que homens, t�m mais mulheres ocupando cargos mais baixos

# Genero x Hora extra
analiseUnivariada(data, data$hora_extra, data$genero)
# As mulheres est�o colocando em m�dia entre 15 e 20 horas extras por m�s
# Os homens est�o colocando em m�dia entre 20 e 30

# Genero x Idade
analiseUnivariada(data, data$idade, data$genero)
# Mesmo em menor n�mero, h� mais mulheres mais velhas na empresa do que homens
# Ao mesmo tempo, h�o poucas mulheres jovens na empresa

# Genero x 9box
analiseUnivariada(data, data$ninebox, data$genero)
# As mulheres tamb�m costumam ter um desempenho pior no 9box em compara��o aos homens
# Temos evidencias pa suspeitar que h� um problema de sexismo na empresa
# Ao mesmo tempo que a empresa tem menos mulheres, elas ocupam cargos mais baixos, costumam ser mais velhas e um desempenho pior no 9box

# Tempo de casa x Genero
analiseUnivariada(data, data$tempo_casa, data$genero)
# As mulheres tamb�m costumam ficar menos tempo na empresa
# Contudo, a medida que o tempo de casa passa de 10 anos eles tem uma presen�a maior

# Agora os gr�ficos de barras para as vari�veis categ�ricas

ggplot(data) + 
  aes(area, fill = genero) + 
  geom_bar() + 
  theme_light() +
  scale_fill_manual(values = cores_cat)
# A maior parte das mulheres trabalha com opera��es, ou suporte ou na produ��o
# Pouca igualdade de genero em todos os setores, em especial no RH (talvez por isso as notas ruins no 9box pras mulheres)


ggplot(data) +
  aes(x = area, y = ID_gestor, color = genero) +
  geom_point(alpha = 0.7, size = 8) +
  theme_light() +
  scale_color_manual(values = cores_cat)
# SUP, RH e OPE s�o mulheres

# Fn��o customizada grafico de barras
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
    labs(title = 'Frequ�ncia absoluta') +
    theme_light() +
    theme(panel.grid = element_blank())
  
  p2 <- ggplot(df, aes(x = {{variavel}}, y = percentual_geral, fill = {{split}})) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = cores_cat) +
    geom_text(aes(label = round(percentual_geral, 1)), position = position_stack(vjust = .5)) +
    labs(y = 'percentual (%)',
         title = 'Propor��o em rela��o ao total') +
    theme_light() +
    theme(panel.grid = element_blank())
  
  p3 <- ggplot(df, aes(x = {{variavel}}, y = percentual_grupo, fill = {{split}})) +
    geom_col() +
    scale_fill_manual(values = cores_cat) +
    geom_text(aes(label = round(percentual_grupo, 1)), position = position_stack(vjust = .5)) +
    labs(y = 'percentual (%)',
         title = 'Propor��o em rela��o � categoria') +
    theme_light() +
    theme(panel.grid = element_blank()) +
    coord_flip()
  
  plot_grid(p1, p2, p3, nrow = 1, rel_widths = c(3, 3, 4))
}

graficoBarras(data, ninebox, genero)
graficoBarras(data, area, genero)
graficoBarras(data, desempenho_cat, genero)
graficoBarras(data, nivel_cargo, genero)

# an�lise bivariada
ggplot(data) +
  aes(x = ninebox, y = idade, color = genero) +
  geom_point(alpha = 0.7, size = 3) +
  theme_light() +
  scale_color_manual(values = cores_cat)
# Apenas homens foram bem avaliados no 9box
# As mulheres mais velhas tiveram as piores avalia��es
# Os homens mais velhos tiverams as melhores avalia��es

ggplot(data) +
  aes(x = area, y = ninebox, color = genero) +
  geom_point(alpha = 0.7, size = 6) +
  theme_light() +
  scale_color_manual(values = cores_cat)
# As mulheres mais mal avaliadas estavam no suporte, enquanto que o homem mais bem avaliado tamb�m

ggplot(data) +
  aes(x = area, y = idade, color = genero) +
  geom_point(alpha = 0.7, size = 3) +
  theme_light() +
  scale_color_manual(values = cores_cat)


ggplot(data) +
  aes(x = tempo_casa, y = ninebox, color = genero) +
  geom_point(alpha = 0.7, size = 3) +
  theme_light() +
  scale_color_manual(values = cores_cat)
# As pessoas mais antigas tambem s�o as mais mal avaliadas no ninebox

ggplot(data) +
  aes(x = hora_extra, y = ninebox, color = genero) +
  geom_point(alpha = 0.7, size = 3) +
  theme_light() +
  scale_color_manual(values = cores_cat)
# Alguns dos homens melhores avaliados tem poucas horas extras

# Essas evidencias levam a crer que a empresa tem um problema de igualdade de genero na empresa
# Poucas mulheres em rela��o ao numero de homens, a maioria mais velha e em setores espec�ficos
# Al�m disso a maior parte delas recebe menos e est� mal avaliada no 9boox



# A�lise multifacetada
ggplot(data) +
  aes(x = nivel_cargo, y = ninebox, color = genero) +
  geom_point(alpha = 0.7, size = 3) +
  theme_light() +
  scale_color_manual(values = cores_cat) +
  facet_grid(ID_gestor ~ area)

# preciso investigar melhor: vou perguntar na aula
colnames(data)


