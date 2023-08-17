# pacotes -----------------------------------------------------------------

library(tidyverse)
library(factoextra)


# dados -------------------------------------------------------------------

dados_brutos <- readxl::read_excel("dados/Tenis turma 22.xlsx", skip = 1) |>
  # limpar nomes das colunas
  janitor::clean_names() |>
  # selecionar colunas que serao usadas
  select(
    contains("horas")[1]:tenis_de_corrida_que_tem_boa_durabilidade_asics
  )

# limpeza -----------------------------------------------------------------

# expressoes regulares para arrumar as perguntas
rx_perguntas <- c(
  "preferencia",
  "evitar_lesoes",
  "correr",
  "amortecimento",
  "inovador",
  "custo_x_beneficio",
  "durabilidade"
) |> str_c(collapse = "|")

# expressao regular para separar os nomes das colunas
rx_separar <- "_(?=[a-z]+$)"

# base das preferencias.
# estamos juntando a turma em 2 grupos:
# corre pouco (0-5h/semana) e corre muito (6-10h/semana)

preferencias <- dados_brutos |>
  # seleciona as colunas que interessam
  select(horas = 1, contains("preferencia")) |>
  # substituir valores vazios por zero
  mutate(across(.fns = replace_na, replace = 0)) |>
  # cria os grupos
  mutate(horas = cut(
    horas, c(0, 5, 10),
    include.lowest = TRUE,
    labels = c("corre_pouco", "corre_muito")
  )) |>
  # empilha as colunas das marcas
  pivot_longer(-horas) |>
  separate(name, c("pergunta", "marca"), sep = rx_separar) |>
  select(-pergunta) |>
  pivot_wider(
    names_from = horas,
    values_from = value,
    values_fn = mean
  ) |>
  # coloca a coluna nos rownames para usar os componentes principais
  column_to_rownames("marca")

# base das percepcoes, utilizando as colunas de percepcoes

percepcoes <- dados_brutos |>
  # seleciona as colunas que interessam
  select(-1, -contains("preferencia")) |>
  # empilha todas as colunas
  pivot_longer(everything()) |>
  separate(name, c("pergunta", "marca"), sep = rx_separar) |>
  mutate(pergunta = str_extract(pergunta, rx_perguntas)) |>
  # coloca de volta nas colunas agregando pela media
  pivot_wider(
    names_from = pergunta,
    values_from = value,
    values_fn = mean
  ) |>
  # coloca a coluna nos rownames para usar os componentes principais
  column_to_rownames("marca")


readr::write_rds(percepcoes, "dados/percepcoes.rds")

# construindo o mapa perceptual -------------------------------------------

pr_comp_perc <- prcomp(percepcoes, scale = FALSE)

pr_comp_perc |>
  fviz_pca_biplot()+
  labs(
    x = " ", y = " ", title = "Mapa Perceptual",
    caption = 'Fonte: Marketing Analítico - Insper'
  )

ggsave("tenis_perceptual.png", bg = "white")

# construindo mapa de preferencia -----------------------------------------

pr_comp_pref <- prcomp(preferencias, scale = FALSE)

pr_comp_pref |>
  fviz_pca_biplot() +
  labs(
    x = " ", y = " ", title = "Mapa de Preferência",
    caption = 'Fonte: Marketing Analítico - Insper'
  )

ggsave("tenis_preferencia.png", bg = "white")

