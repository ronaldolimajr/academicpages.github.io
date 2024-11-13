# Carregar dados
cars = mtcars

# Espiar os dados
?mtcars
str(cars)

# Pizza por quantidade de cilindros
pie(table(cars$cyl))

# Pizza por quantidade de marchas
pie(table(cars$gear))

# Comparando as pizzas
par(mfrow = c(1, 2))
pie(table(cars$cyl))
pie(table(cars$gear))
par(mfrow = c(1, 1))

# Pizza com ggplot
library(tidyverse)

# Criar tabela de contagens e percentuais
prop = data.frame(prop.table(table(cars$cyl)),
                  prop.table(table(cars$gear)))
prop

# Gráficos
# Cilindros
ggplot(prop, aes(x = "", y = Freq, fill = Var1)) +
  geom_col() +
  coord_polar("y") +
  labs(fill = "Cilindros", y = "")

# Marchas
ggplot(prop, aes(x = "", y = Freq.1, fill = Var1.1)) +
  geom_col() +
  coord_polar("y") +
  labs(fill = "Marchas", y = "")

# ======================================================
#                      ALTERNATIVA
#                 - Gráfico de Barras -
# ======================================================

# Gráfico de barras mínimo
ggplot(prop, aes(x = Var1, y = Freq)) +
  geom_col() 
  #geom_bar(stat = "identity")
  #stat_summary(geom = "bar") 

  
# Melhorando visualização
ggplot(prop, aes(x = Var1, y = Freq)) +
  #geom_col() +
  geom_col(fill = ifelse(prop$Var1 == '8', 'blue4', 'lightgray')) +
  geom_text(aes(label = str_c(round(Freq*100, digits = 0), "%")),
            #fontface = "bold", 
            fontface = ifelse(prop$Var1 == '8', 'bold', 'plain'), 
            #size = 4,
            size = ifelse(prop$Var1 == '8', 4, 3), 
            color = ifelse(prop$Var1 == '8', "white", "black"), 
            position = position_stack(vjust = 0.5)
            ) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
        ) +
  labs(y = "", x = "Cilindros",
       title = "Percentual de carros por quantidade de cilindros",
       subtitle = "Dados de 32 modelos de 1974")


# Por quantidade de marchas
ggplot(prop, aes(x = Var1.1, y = Freq.1)) +
  #geom_col() +
  geom_col(fill = ifelse(prop$Var1.1 == '3', 'blue4', 'lightgray')) +
  geom_text(aes(label = str_c(round(Freq*100, digits = 0), "%")),
            #fontface = "bold", 
            fontface = ifelse(prop$Var1.1 == '3', 'bold', 'plain'), 
            #size = 4,
            size = ifelse(prop$Var1.1 == '3', 4, 3), 
            color = ifelse(prop$Var1.1 == '3', "white", "black"), 
            position = position_stack(vjust = 0.5)
  ) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
  ) +
  labs(y = "", x = "Marchas",
       title = "Percentual de carros por quantidade de marchas",
       subtitle = "Dados de 32 modelos de 1974")


# Juntando os dois em um para comparar
library(patchwork)

P1 = ggplot(prop, aes(x = Var1, y = Freq)) +
  #geom_col() +
  geom_col(fill = ifelse(prop$Var1 == '8', 'blue4', 'lightgray')) +
  geom_text(aes(label = str_c(round(Freq*100, digits = 0), "%")),
            #fontface = "bold", 
            fontface = ifelse(prop$Var1 == '8', 'bold', 'plain'), 
            #size = 4,
            size = ifelse(prop$Var1 == '8', 4, 3), 
            color = ifelse(prop$Var1 == '8', "white", "black"), 
            position = position_stack(vjust = 0.5)
  ) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
  ) +
  labs(y = "", x = "Cilindros",
       title = "Percentual de carros por quantidade de cilindros",
       subtitle = "Dados de 32 modelos de 1974")

P2 = ggplot(prop, aes(x = Var1.1, y = Freq.1)) +
  #geom_col() +
  geom_col(fill = ifelse(prop$Var1.1 == '3', 'blue4', 'lightgray')) +
  geom_text(aes(label = str_c(round(Freq*100, digits = 0), "%")),
            #fontface = "bold", 
            fontface = ifelse(prop$Var1.1 == '3', 'bold', 'plain'), 
            #size = 4,
            size = ifelse(prop$Var1.1 == '3', 4, 3), 
            color = ifelse(prop$Var1.1 == '3', "white", "black"), 
            position = position_stack(vjust = 0.5)
  ) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
  ) +
  labs(y = "", x = "Marchas",
       title = "Percentual de carros por quantidade de marchas",
       subtitle = "Dados de 32 modelos de 1974")

P1 + P2

# Antes e Depois e Depois
par(mfrow = c(1, 2))
pie(table(cars$cyl))
pie(table(cars$gear))
par(mfrow = c(1, 1))

pie1 = ggplot(prop, aes(x = "", y = Freq, fill = Var1)) +
  geom_col() +
  coord_polar("y") +
  labs(fill = "Cilindros", y = "")

pie2 = ggplot(prop, aes(x = "", y = Freq.1, fill = Var1.1)) +
  geom_col() +
  coord_polar("y") +
  labs(fill = "Marchas", y = "")

pie1 + pie2

P1 + P2

pie1 + P1

(pie1 | P1) / (pie2 | P2)
