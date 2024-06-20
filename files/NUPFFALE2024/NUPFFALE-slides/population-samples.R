
# Carregar pacote
library(tidyverse)
library(DT)

# Simular uma população de 100 mil alunos com seus resultados em um teste
set.seed(1)
population = rbeta(100000, 5, 2)

# Distribuição das notas da POPULAÇÃO de alunos
hist(population)

mean(population)
sd(population)

# Vamos extrair 3 amostras dessa população, cada uma com 20 pessoas,
# simulando 3 turmas diferentes de alunos vindos da mesma população
set.seed(4)
sample1 = sample(x = population, size = 20)
sample2 = sample(x = population, size = 20)
sample3 = sample(x = population, size = 20)

# Criar um tibble (data frame) com os dados das 3 turmas simuladas (samples)
sample.data = tibble(class1 = sample1,
                     class2 = sample2,
                     class3 = sample3) %>% 
  gather("class1", "class2", "class3", key = class, value = test)

# Médias e desvios-padrão
sample.data %>% 
  group_by(class) %>% 
  summarize(Test.mean = mean(test),
            Test.SD = sd(test))

# Gerar gráfico
ggplot(sample.data, aes(x = class, y = test)) +
  geom_boxplot() +
  stat_summary(color = "blue") +
  theme_minimal()

# Conduzir uma ANOVA
summary(aov(data = sample.data, test ~ class))

# Comparações pareadas
TukeyHSD(aov(data = sample.data, test ~ class))


# PODER ESTATÍSTICO

## Calcular o tamanho do efeito primeiro
library(lsr)

etaSquared(aov(data = sample.data, test ~ class))

## Agora o poder
library(pwr)

pwr.anova.test(k = 3, n = 20, f = 0.1002268, sig.level = 0.0493)
## --> 9.5% de probabilidade de detectar um efeito caso ele exista

# Qual seria o n por grupo para se obter um poder de 80%?
pwr.anova.test(k = 3, f = 0.1, sig.level = 0.05, power = 0.8)
## --> 322 estudantes por turma!

