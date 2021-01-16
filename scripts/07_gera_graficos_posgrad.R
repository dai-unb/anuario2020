# setup
library(ggtext)
library(ragg)

theme_set(theme_minimal(base_family = "Charter"))
fundo <- cores_anuario["pos"]

# gráfico de barras dos totais por unidade
graf1 <- rio::import("dados_posgraduacao/tabelas_posgrad.xlsx", 
                     sheet = "grafico22",
                     skip = 1)

graf <- graf1 %>% 
  mutate(Unidade = fct_inorder(Unidade),
         Unidade = factor(Unidade, levels = rev(levels(Unidade)))) %>% 
  ggplot(aes(x = Total, y = Unidade)) +
  geom_col(fill = fundo, width = 0.5) +
  geom_text(aes(label = format(Total, big.mark = "."), 
                family = "Charter"), 
            hjust = "left",
            nudge_x = 10
  ) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  labs(y = "Unidade acadêmica",
       title = "Alunos regulares nos cursos de pós-graduação Stricto Sensu\npor unidade acadêmica, UnB, 2º semestre de 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf
ggsave("graficos/posgrad/graf1.pdf", device = cairo_pdf)
pdftools::pdf_convert(pdf = "graficos/posgrad/graf1.pdf", 
                      filenames = "graficos/posgrad/graf1.png", 
                      format = "png", 
                      dpi = 250)

# gráfico de barras dos totais por unidade
graf2 <- rio::import("dados_posgraduacao/tabelas_posgrad.xlsx", 
                     sheet = "grafico24")

graf <- graf2 %>% 
  ggplot(aes(x = Continente, y = n)) +
  geom_col(fill = fundo, width = 0.5) +
  geom_text(aes(label = format(n, big.mark = "."), 
                family = "Charter"), 
            hjust = "center",
            nudge_y = 10
  ) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank()) +
  labs(title = "Alunos estrangeiros regulares nos cursos de pós-graduação Stricto Sensu\npor continente, UnB, 2º semestre de 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf
ggsave("graficos/posgrad/graf2.pdf", device = cairo_pdf)
pdftools::pdf_convert(pdf = "graficos/posgrad/graf2.pdf", 
                      filenames = "graficos/posgrad/graf2.png", 
                      format = "png", 
                      dpi = 250)

# gráfico de barras dos totais por unidade
graf3 <- rio::import("dados_posgraduacao/tabelas_posgrad.xlsx", 
                     sheet = "grafico25")

graf <- graf3 %>% 
  mutate(Unidade = fct_inorder(Unidade),
         Unidade = factor(Unidade, levels = rev(levels(Unidade)))) %>% 
  ggplot(aes(x = n, y = Unidade)) +
  geom_col(fill = fundo, width = 0.5) +
  geom_text(aes(label = format(n, big.mark = "."), 
                family = "Charter"), 
            hjust = "left",
            nudge_x = 1
  ) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  labs(y = "Unidade acadêmica",
       title = "Alunos estrangeiros regulares nos cursos de pós-graduação Stricto Sensu\npor unidade acadêmica, UnB, 2º semestre de 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf
ggsave("graficos/posgrad/graf3.pdf", device = cairo_pdf, width = 7)
pdftools::pdf_convert(pdf = "graficos/posgrad/graf3.pdf", 
                      filenames = "graficos/posgrad/graf3.png", 
                      format = "png", 
                      dpi = 250)

# gráfico de barras dos totais por unidade
graf4 <- rio::import("dados_posgraduacao/tabelas_posgrad.xlsx", 
                     sheet = "grafico26")

graf <- graf4 %>% 
  mutate(País = fct_inorder(País),
         País = factor(País, levels = rev(levels(País)))) %>% 
  ggplot(aes(x = n, y = País)) +
  geom_col(fill = fundo, width = 0.5) +
  geom_text(aes(label = format(n, big.mark = "."), 
                family = "Charter"), 
            hjust = "left",
            nudge_x = 1
  ) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  labs(title = "Países com maior número de alunos regulares nos cursos de\npós-graduação Stricto Sensu, UnB, 2º semestre de 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf
ggsave("graficos/posgrad/graf4.pdf", device = cairo_pdf)
pdftools::pdf_convert(pdf = "graficos/posgrad/graf4.pdf", 
                      filenames = "graficos/posgrad/graf4.png", 
                      format = "png", 
                      dpi = 250)
                      