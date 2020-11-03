
# setup
library(ggtext)
library(ragg)

theme_set(theme_minimal(base_family = "Charter"))
fundo <- cores_anuario[2]

# gráfico de barras dos totais por forma de ingresso
graf1 <- rio::import("dados_graduacao/Tabela2.03.2.xlsx")

graf <- graf1 %>% 
  mutate(name = factor(name, levels = rev(c("Vestibular", "PAS", "ENEM", "Outras vias")))) %>% 
  ggplot(aes(x = value, y = name)) +
  geom_col(fill = fundo, width = 0.5) +
  geom_text(aes(label = format(value, big.mark = "."), 
                fontface = "bold",
                family = "Charter"), 
            hjust = "right", 
            nudge_x = -100,
            color = "white",
  ) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  labs(y = "Forma de ingresso",
       title = "Ingresso de alunos nos cursos de graduação pelo vestibular,\nPAS, ENEM e outras vias, UnB, 2019",
       caption = "Fonte: Censo da Educação Superior 2019")

ggsave("graficos/graduacao/graf1.pdf", device = cairo_pdf)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf1.pdf", 
                      filenames = "graficos/graduacao/graf1.png", 
                      format = "png", 
                      dpi = 500)

# gráfico de barras do total de ing e form por unidade
graf2 <- rio::import("dados_graduacao/Tabela2.04.2.RData")

graf <- graf2 %>% 
  pivot_longer(-Unidade) %>% 
  mutate(Unidade = factor(Unidade),
         Unidade = factor(Unidade, levels = rev(levels(Unidade)))) %>% 
  ggplot(aes(x = value, y = Unidade, fill = name)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c('#0071BB', alpha('#0071BB', 0.5))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Unidade acadêmica",
       title = "<span style='color:#7FB8DD'>Ingressantes</span> e <span style='color:#0071BB'>formados</span> nos cursos de graduação,<br>por unidade acadêmica, UnB, 2019",
       caption = "Fonte: Censo da Educação Superior 2019")

ggsave("graficos/graduacao/graf2.pdf", device = cairo_pdf)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf2.pdf", 
                      filenames = "graficos/graduacao/graf2.png", 
                      format = "png", 
                      dpi = 500)

# gráfico de colunas do percentual de ingresso por sexo e faixa etária
graf3 <- rio::import("dados_graduacao/Tabela2.05.xlsx")

graf <-
  graf3 %>% 
  filter(`Faixa Etária` != "Total") %>% 
  select(`Faixa Etária`, Feminino, Masculino) %>% 
  pivot_longer(-`Faixa Etária`) %>% 
  mutate(`Faixa Etária` = factor(`Faixa Etária`),
         `Faixa Etária` = factor(`Faixa Etária`, levels = rev(levels(`Faixa Etária`))),
         name = factor(name, levels = c("Masculino", "Feminino"))) %>% 
  ggplot(aes(x = value, y = `Faixa Etária`, fill = name)) +
  geom_col(position = position_fill(), width = 0.7) +
  geom_text(aes(label = format(value, big.mark = "."), family = "Charter"), 
            position = position_fill(vjust = 0.5), 
            color = "white") + 
  scale_fill_manual(values = c('#0071BB', alpha('#0071BB', 0.5))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Faixa etária",
       title = "Ingresso de alunos nos cursos de graduação por faixa etária e sexo,<br><span style='color:#7FB8DD'>feminino</span> e <span style='color:#0071BB'>masculino</span>, UnB, 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf

ggsave("graficos/graduacao/graf3.pdf", device = cairo_pdf, width = 7)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf3.pdf", 
                      filenames = "graficos/graduacao/graf3.png", 
                      format = "png", 
                      dpi = 500)

# gráfico de barras do total de matriculados por turno e unidade, no 2º semestre
graf4 <- rio::import("dados_graduacao/Tabela2.06.xlsx")

graf <- 
  graf4 %>% 
  select(Unidade, Integral = `Integral 2`, Noturno = `Noturno 2`, EaD = `EAD 2`) %>% 
  filter(Unidade != "Total") %>% 
  pivot_longer(-Unidade) %>% 
  mutate(Unidade = factor(Unidade),
         Unidade = factor(Unidade, levels = rev(levels(Unidade))),
         name = factor(name, levels = c("EaD", "Noturno", "Integral"))) %>% 
  ggplot(aes(x = value, y = Unidade, fill = name)) +
  geom_col(position = position_fill(), width = 0.7) +
  scale_fill_manual(values = c(alpha('#008D46', 0.8), '#00AAAD', alpha('#0071BB', 0.8))) +
  geom_text(aes(label = format(value, big.mark = ".")), 
            family = "Charter", size = 3,
            color = 'white', 
            position = position_fill(vjust = 0.5)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 12),
        panel.background = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Unidade acadêmica",
       title = "Alunos regulares registrados por turno, <span style='color:#65A5D1'>integral</span></b>, <span style='color:#00AAAD'>noturno</span> e <span style='color:#008D46'>EaD</span>, nos cursos de graduação,<br>por unidade acadêmica, UnB, 2º semestre de 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf
ggsave("graficos/graduacao/graf4.pdf", device = cairo_pdf, height = 10, width = 8)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf4.pdf", 
                      filenames = "graficos/graduacao/graf4.png", 
                      format = "png", 
                      dpi = 500)

# gráfico de barras do total de alunos registrados por sexo e unidade, no 2º semestre
graf5 <- rio::import("dados_graduacao/Tabela2.07.xlsx")

graf <- 
  graf5 %>% 
  select(Unidade, Masculino = `Masculino 2`, Feminino = `Feminino 2`) %>% 
  filter(Unidade != "Total") %>% 
  pivot_longer(-Unidade) %>% 
  mutate(Unidade = factor(Unidade),
         Unidade = factor(Unidade, levels = rev(levels(Unidade))),
         name = factor(name, levels = c("Masculino", "Feminino"))) %>% 
  ggplot(aes(x = value, y = Unidade, fill = name)) +
  geom_col(position = position_fill(), width = 0.7) +
  scale_fill_manual(values = c('#0071BB', alpha('#0071BB', 0.5))) +
  geom_text(aes(label = format(value, big.mark = ".")), 
            family = "Charter", size = 4,
            color = 'white', 
            position = position_fill(vjust = 0.5)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 12),
        panel.background = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Unidade acadêmica",
       title = "Alunos regulares registrados por sexo, <span style='color:#7FB8DD'>feminino</span></b> e <span style='color:#0071BB'>masculino</span>, nos cursos de graduação,<br>por unidade acadêmica, UnB, 2º semestre de 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf
ggsave("graficos/graduacao/graf5.pdf", device = cairo_pdf, height = 10, width = 8)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf5.pdf", 
                      filenames = "graficos/graduacao/graf5.png", 
                      format = "png", 
                      dpi = 500)

# gráfico de colunas do percentual de ingresso por sexo e faixa etária
graf6 <- rio::import("dados_graduacao/Tabela2.05.xlsx")

graf <-
  graf6 %>% 
  filter(`Faixa Etária` != "Total") %>% 
  select(`Faixa Etária`, Feminino, Masculino) %>% 
  pivot_longer(-`Faixa Etária`) %>% 
  mutate(`Faixa Etária` = factor(`Faixa Etária`),
         `Faixa Etária` = factor(`Faixa Etária`, levels = rev(levels(`Faixa Etária`))),
         name = factor(name, levels = c("Masculino", "Feminino"))) %>% 
  ggplot(aes(x = value, y = `Faixa Etária`, fill = name)) +
  geom_col(position = position_fill(), width = 0.7) +
  geom_text(aes(label = format(value, big.mark = "."), family = "Charter"), 
            position = position_fill(vjust = 0.5), 
            color = "white") + 
  scale_fill_manual(values = c('#0071BB', alpha('#0071BB', 0.5))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Faixa etária",
       title = "Alunos regulares registrados nos cursos de graduação por faixa etária e sexo,<br><span style='color:#7FB8DD'>feminino</span> e <span style='color:#0071BB'>masculino</span>, UnB, 2º semestre de 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf

ggsave("graficos/graduacao/graf6.pdf", device = cairo_pdf, height = 8, width = 8)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf6.pdf", 
                      filenames = "graficos/graduacao/graf6.png", 
                      format = "png", 
                      dpi = 500)

# graf de barras do total por raça/cor
graf7 <- rio::import("dados_graduacao/Tabela2.XX.xlsx")

graf <- 
  graf7 %>% 
  filter(`Unidade acadêmica / curso / habilitação` == "Total Geral") %>% 
  pivot_longer(-`Unidade acadêmica / curso / habilitação`) %>% 
  select(-`Unidade acadêmica / curso / habilitação`) %>% 
  mutate(name = factor(name),
         name = factor(name, levels = c("Não dispõe de informação",
                                        "Aluno não quis declarar cor/raça",
                                        "Preta",
                                        "Parda",
                                        "Indígena",
                                        "Branca",
                                        "Amarela"))) %>% 
  ggplot(aes(x = value, y = name)) +
  geom_col(fill = fundo, width = 0.5) +
  geom_text(aes(label = format(value, big.mark = "."), family = "Charter"), 
            hjust = "inward") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_markdown(lineheight = 1.2)) +
  labs(y = "Raça/cor autodeclarada",
       title = "Alunos regulares registrados nos cursos de graduação por<br>raça/cor autodeclarada, UnB, 2º semestre de 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf
ggsave("graficos/graduacao/graf7.pdf", device = cairo_pdf, height = 8, width = 8)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf7.pdf", 
                      filenames = "graficos/graduacao/graf7.png", 
                      format = "png", 
                      dpi = 500)

# graf do total de matriculados por sexo e raça/cor
graf8 <- rio::import("dados_graduacao/Tabela2.XX2.xlsx")

graf <-
  graf8 %>% 
  filter(Raca != "Total") %>% 
  select(Raca, Feminino, Masculino) %>% 
  pivot_longer(-Raca) %>% 
  mutate(Raca = factor(Raca),
         Raca = factor(Raca, levels = rev(levels(Raca))),
         name = factor(name, levels = c("Masculino", "Feminino")),
         Raca = factor(Raca, levels = c("Não dispõe de informação",
                                        "Aluno não quis declarar cor/raça",
                                        "Preta",
                                        "Parda",
                                        "Indígena",
                                        "Branca",
                                        "Amarela"))) %>% 
  ggplot(aes(x = value, y = Raca, fill = name)) +
  geom_col(position = position_fill(), width = 0.7) +
  geom_text(aes(label = format(value, big.mark = "."), family = "Charter"), 
            position = position_fill(vjust = 0.5), 
            color = "white") + 
  scale_fill_manual(values = c('#0071BB', alpha('#0071BB', 0.5))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Raça/cor autodeclarada",
       title = "Alunos regulares registrados nos cursos de graduação por raça/cor autodeclarada<br>e sexo, <span style='color:#7FB8DD'>feminino</span> e <span style='color:#0071BB'>masculino</span>, UnB, 2º semestre de 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf

ggsave("graficos/graduacao/graf8.pdf", device = cairo_pdf, height = 8, width = 10)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf8.pdf", 
                      filenames = "graficos/graduacao/graf8.png", 
                      format = "png", 
                      dpi = 500)

# grad de total por continente
graf9 <- rio::import("dados_graduacao/Tabela2.14.xlsx")

graf <- 
  graf9 %>% 
  select(Continente = `Unidade acadêmica / curso / habilitação`, Total) %>% 
  filter(str_detect(Continente, "Total do")) %>% 
  mutate(Continente = case_when(
    
    str_detect(Continente, "África") ~ "África",
    str_detect(Continente, "América") ~ "América",
    str_detect(Continente, "Ásia") ~ "Ásia",
    str_detect(Continente, "Europa") ~ "Europa"
    
  )) %>% 
  mutate(Continente = factor(Continente),
         Continente = factor(Continente, levels = rev(levels(Continente)))) %>% 
  ggplot(aes(x = Total, y = Continente)) +
  geom_col(fill = fundo, width = 0.5) +
  geom_text(aes(label = format(Total, big.mark = "."), family = "Charter"), 
            hjust = "right", color = "white", nudge_x = -2) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2)) +
  labs(y = "Continente",
       title = "Alunos estrangeiros regulares registrados nos cursos de graduação,<br>por continente, UnB, 2º semestre de 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf
ggsave("graficos/graduacao/graf9.pdf", device = cairo_pdf, width = 7)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf9.pdf", 
                      filenames = "graficos/graduacao/graf9.png", 
                      format = "png", 
                      dpi = 500)

# graf de total por país (maiores apenas)
graf10 <- rio::import("dados_graduacao/Tabela2.14.2.xlsx")

graf <- 
  graf10 %>% 
  # mutate(Continente = factor(Continente),
         # Continente = factor(Continente, levels = rev(levels(Continente)))) %>% 
  ggplot(aes(x = n, y = factor(Pais, levels = Pais[order(n)]))) +
  geom_col(fill = fundo, width = 0.5) +
  geom_text(aes(label = format(n, big.mark = "."), family = "Charter"), 
            hjust = "right", color = "white", nudge_x = -1) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2)) +
  labs(y = "País",
       title = "Alunos estrangeiros regulares registrados nos cursos de graduação,<br>por país (10 maiores), UnB, 2º semestre de 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf
ggsave("graficos/graduacao/graf10.pdf", device = cairo_pdf, width = 8)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf10.pdf", 
                      filenames = "graficos/graduacao/graf10.png", 
                      format = "png", 
                      dpi = 500)

# graf de barras da evolução do total de ingressantes por ano 
graf11_ing <- rio::import("dados_graduacao/Tabela2.21.xlsx") %>% 
  filter(`Unidade Acadêmica / Curso / Habilitação` == "Total Geral") %>% 
  mutate(`Unidade Acadêmica / Curso / Habilitação` = "Ingressantes")
graf11_mat <- rio::import("dados_graduacao/Tabela2.22.xlsx") %>% 
  filter(`Unidade Acadêmica / Curso / Habilitação` == "Total Geral") %>% 
  mutate(`Unidade Acadêmica / Curso / Habilitação` = "Matriculados")
graf11_for <- rio::import("dados_graduacao/Tabela2.23.xlsx") %>% 
  filter(`Unidade Acadêmica / Curso / Habilitação` == "Total Geral") %>% 
  mutate(`Unidade Acadêmica / Curso / Habilitação` = "Formados")

graf11 <- list(graf11_ing, graf11_mat, graf11_for) %>% 
  map_df(bind_rows)

graf <- 
  graf11 %>%
  rename(Categoria = `Unidade Acadêmica / Curso / Habilitação`) %>% 
  pivot_longer(-Categoria) %>% 
  mutate(Categoria = factor(Categoria, levels = c("Formados", "Ingressantes", "Matriculados"))) %>% 
  ggplot(aes(x = name, y = value, group = Categoria, color = Categoria)) +
  geom_line() +
  geom_text(aes(label = format(value, big.mark = ".")),
            vjust = "bottom",
            nudge_y = 500,
            family = "Charter") +
  scale_color_manual(values = c('#008D46', '#00AAAD', "#0071BB")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.line.x = element_line(colour = "#E6E6E6"),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(x = "Ano",
       title = "Evolução do número de alunos <span style='color:#0071BB'>matriculados</span>, <span style='color:#00AAAD'>ingressantes</span> e <span style='color:#008D46'>formados</span>,<br>UnB, 2015 a 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
  
graf
ggsave("graficos/graduacao/graf11.pdf", device = cairo_pdf, width = 8)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf11.pdf", 
                      filenames = "graficos/graduacao/graf11.png", 
                      format = "png", 
                      dpi = 500)
