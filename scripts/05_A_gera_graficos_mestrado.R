

# setup
library(ggtext)
library(ragg)

theme_set(theme_minimal(base_family = "Charter"))
fundo <- cores_anuario[4]

# Ingresso de alunos e número de dissertações homologadas nos cursos de mestrado, por unidade acadêmica, UnB, 2019
graf1 <- rio::import("dados_mestrado/Tabelas_Mestrado_Anuário_2019.xlsx", 
                     sheet = "Tabela 3.12.2") %>% 
  map_df(~replace_na(., 0))

graf <- 
  graf1 %>% 
  pivot_longer(-Unidade) %>% 
  mutate(Unidade = factor(Unidade),
         Unidade = factor(Unidade, levels = rev(levels(Unidade)))) %>% 
  ggplot(aes(x = value, y = Unidade, fill = name)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c(alpha('#8EC63F', 0.5), '#8EC63F')) +
  xlim(c(0,250)) +
  geom_vline(xintercept = 50, color = "#F3F3F4") +
  geom_vline(xintercept = 100, color = "#F3F3F4") +
  geom_vline(xintercept = 150, color = "#F3F3F4") +
  geom_vline(xintercept = 200, color = "#F3F3F4") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Unidade",
       title = "<b><span style='color:#8EC63F'>Ingressantes</span></b> e <b><span style='color:#D2E8B2'>concluintes</span></b> nos cursos de mestrado<br>por unidade acadêmica, UnB, 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf
ggsave("graficos/mestrado/graf1.pdf", device = cairo_pdf)
pdftools::pdf_convert(pdf = "graficos/mestrado/graf1.pdf", 
                      filenames = "graficos/mestrado/graf1.png", 
                      format = "png", 
                      dpi = 250)

# Alunos ingressantes nos cursos de mestrado, por sexo e faixa etária, UnB, 2019
graf2 <- rio::import("dados_Mestrado/Tabelas_Mestrado_Anuário_2019.xlsx", 
                     sheet = "Tabela 3.13")

graf <-
  graf2 %>% 
  filter(`Faixa Etária` != "Total") %>% 
  select(`Faixa Etária`, Feminino = `Fem. Ing`, Masculino = `Masc. Ing`) %>% 
  pivot_longer(-`Faixa Etária`) %>% 
  mutate(`Faixa Etária` = factor(`Faixa Etária`),
         `Faixa Etária` = factor(`Faixa Etária`, levels = rev(levels(`Faixa Etária`))),
         name = factor(name, levels = c("Masculino", "Feminino"))) %>% 
  ggplot(aes(x = value, y = `Faixa Etária`, fill = name)) +
  geom_col(position = position_fill(), width = 0.7) +
  geom_text(aes(label = format(value, big.mark = "."), family = "Charter"), 
            position = position_fill(vjust = 0.5), 
            color = "white") + 
  scale_fill_manual(values = c('#8EC63F', alpha('#8EC63F', 0.4))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Faixa etária",
       title = "Ingresso de alunos nos cursos de mestrado por faixa etária e sexo,<br><b><span style='color:#D2E8B2'>feminino</span></b> e <b><span style='color:#8EC63F'>masculino</span></b>, UnB, 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf

ggsave("graficos/mestrado/graf2.pdf", device = cairo_pdf, width = 7)
pdftools::pdf_convert(pdf = "graficos/mestrado/graf2.pdf", 
                      filenames = "graficos/mestrado/graf2.png", 
                      format = "png", 
                      dpi = 250)

# Alunos concluintes nos cursos de mestrado, por sexo e faixa etária, UnB, 2019
graf3 <- rio::import("dados_Mestrado/Tabelas_Mestrado_Anuário_2019.xlsx", 
                     sheet = "Tabela 3.13")

graf <-
  graf3 %>% 
  filter(`Faixa Etária` != "Total") %>% 
  select(`Faixa Etária`, Feminino = `Fem. For`, Masculino = `Masc. For`) %>% 
  pivot_longer(-`Faixa Etária`) %>% 
  mutate(`Faixa Etária` = factor(`Faixa Etária`),
         `Faixa Etária` = factor(`Faixa Etária`, levels = rev(levels(`Faixa Etária`))),
         name = factor(name, levels = c("Masculino", "Feminino"))) %>% 
  ggplot(aes(x = value, y = `Faixa Etária`, fill = name)) +
  geom_col(position = position_fill(), width = 0.7) +
  geom_text(aes(label = format(value, big.mark = "."), family = "Charter"), 
            position = position_fill(vjust = 0.5), 
            color = "white") + 
  scale_fill_manual(values = c('#8EC63F', alpha('#8EC63F', 0.4))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Faixa etária",
       title = "Formatura de alunos nos cursos de mestrado por faixa etária e sexo,<br><b><span style='color:#D2E8B2'>feminino</span></b> e <b><span style='color:#8EC63F'>masculino</span></b>, UnB, 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf

ggsave("graficos/mestrado/graf3.pdf", device = cairo_pdf, width = 7)
pdftools::pdf_convert(pdf = "graficos/mestrado/graf3.pdf", 
                      filenames = "graficos/mestrado/graf3.png", 
                      format = "png", 
                      dpi = 250)

# Alunos matriculados nos cursos de mestrado, por sexo e faixa etária, UnB, 2019
graf4 <- rio::import("dados_Mestrado/Tabelas_Mestrado_Anuário_2019.xlsx", 
                     sheet = "Tabela 3.15",
                     skip = 1)

graf <-
  graf4 %>% 
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
  scale_fill_manual(values = c('#8EC63F', alpha('#8EC63F', 0.4))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Faixa etária",
       title = "Alunos regulares nos cursos de mestrado por faixa etária e sexo,<br><b><span style='color:#D2E8B2'>feminino</span></b> e <b><span style='color:#8EC63F'>masculino</span></b>, UnB, 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf

ggsave("graficos/mestrado/graf4.pdf", device = cairo_pdf, width = 8)
pdftools::pdf_convert(pdf = "graficos/mestrado/graf4.pdf", 
                      filenames = "graficos/mestrado/graf4.png", 
                      format = "png", 
                      dpi = 250)

# Alunos matriculados nos cursos de mestrado, por sexo e unidade, UnB, 2019
graf5 <- rio::import("dados_Mestrado/Tabelas_Mestrado_Anuário_2019.xlsx", 
                     sheet = "Tabela 3.16",
                     skip = 1)

graf <-
  graf5 %>% 
  filter(`Unidade Acadêmica/Curso` != "Total") %>% 
  left_join(label_unidade, by = c(`Unidade Acadêmica/Curso` = "Curso")) %>% 
  filter(!is.na(Unidade)) %>% 
  select(Unidade, Feminino = `Feminino 2`, Masculino = `Masculino 2`) %>% 
  pivot_longer(-Unidade) %>% 
  mutate(Unidade = factor(Unidade),
         Unidade = factor(Unidade, levels = rev(levels(Unidade))),
         name = factor(name, levels = c("Masculino", "Feminino"))) %>% 
  ggplot(aes(x = value, y = Unidade, fill = name)) +
  geom_col(position = position_fill(), width = 0.8) +
  geom_text(aes(label = format(value, big.mark = "."), family = "Charter"), 
            position = position_fill(vjust = 0.5), 
            color = "white") + 
  scale_fill_manual(values = c('#8EC63F', alpha('#8EC63F', 0.4))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(y = "Unidade",
       title = "Alunos regulares nos cursos de mestrado por unidade e sexo,<br><b><span style='color:#D2E8B2'>feminino</span></b> e <b><span style='color:#8EC63F'>masculino</span></b>, UnB, 2º semestre de 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf

ggsave("graficos/mestrado/graf5.pdf", device = cairo_pdf, width = 8)
pdftools::pdf_convert(pdf = "graficos/mestrado/graf5.pdf", 
                      filenames = "graficos/mestrado/graf5.png", 
                      format = "png", 
                      dpi = 250)

# graf de linhas da evolução do total de ingressantes, matriculados e formados por ano 
graf6_ing <- rio::import("dados_mestrado/Tabelas_Mestrado_Anuário_2019.xlsx", 
                         sheet = "Tabela 3.19",
                         skip = 1) %>% 
  filter(`Unidade Acadêmica/Curso` == "Total Geral") %>% 
  mutate(`Unidade Acadêmica/Curso` = "Ingressantes")
graf6_mat <- rio::import("dados_mestrado/Tabelas_Mestrado_Anuário_2019.xlsx", 
                         sheet = "Tabela 3.20",
                         skip = 1) %>% 
  filter(`Unidade Acadêmica/Curso` == "Total Geral") %>% 
  mutate(`Unidade Acadêmica/Curso` = "Matriculados")
graf6_for <- rio::import("dados_mestrado/Tabelas_Mestrado_Anuário_2019.xlsx", 
                        sheet = "Tabela 3.21",
                        skip = 1) %>% 
  filter(`Unidade Acadêmica/Curso` == "Total Geral") %>% 
  mutate(`Unidade Acadêmica/Curso` = "Formados")

graf6 <- list(graf6_ing, graf6_mat, graf6_for) %>% 
  map_df(bind_rows)

graf <- 
  graf6 %>%
  rename(Categoria = `Unidade Acadêmica/Curso`) %>% 
  pivot_longer(-Categoria) %>% 
  mutate(Categoria = factor(Categoria, levels = c("Formados", "Ingressantes", "Matriculados"))) %>% 
  ggplot(aes(x = name, y = value, group = Categoria, color = Categoria)) +
  geom_line() +
  geom_text(aes(label = format(value, big.mark = ".")),
            vjust = "bottom",
            nudge_y = 60,
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
       title = "Evolução do número de alunos de mestrado <span style='color:#0071BB'>matriculados</span>, <span style='color:#00AAAD'>ingressantes</span> e <span style='color:#008D46'>formados</span>,<br>UnB, 2015 a 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")

graf
ggsave("graficos/mestrado/graf6.pdf", device = cairo_pdf, width = 8)
pdftools::pdf_convert(pdf = "graficos/mestrado/graf6.pdf", 
                      filenames = "graficos/mestrado/graf6.png", 
                      format = "png", 
                      dpi = 250)

# Evolução do número de alunos com dissertações homologadas nos cursos de mestrado, por décadas, 1976 a 2018
graf7 <- tribble(
  
  ~Década, ~Formados,
  "Até 1980", 25,
  "Até 1990", 84,
  "Até 2000", 3001,
  "Até 2010", 10288,
  "Até 2019", 23369
  
)

graf <- 
  graf7 %>% 
  ggplot(aes(x = Formados, y = Década)) +
  geom_col(fill = fundo, width = 0.5) +
  coord_flip() + 
  geom_text(aes(label = format(Formados, big.mark = "."), family = "Charter"),
            vjust = "bottom", nudge_x = 100) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_markdown(lineheight = 1.2)) +
  labs(title = "Evolução do número de alunos com dissertações homologadas nos cursos de mestrado<br>por década, 1976 a 2019",
       caption = "Fonte: Sistemas acadêmicos da UnB, em 14/04/2020")
graf
ggsave("graficos/mestrado/graf7.pdf", device = cairo_pdf, width = 8)
pdftools::pdf_convert(pdf = "graficos/mestrado/graf7.pdf", 
                      filenames = "graficos/mestrado/graf7.png", 
                      format = "png", 
                      dpi = 250)
