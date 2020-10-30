
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
         `Faixa Etária` = factor(`Faixa Etária`, levels = rev(levels(`Faixa Etária`)))) %>% 
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
       title = "Ingresso de alunos nos cursos de graduação por sexo,<br><span style='color:#7FB8DD'>masculino</span> e <span style='color:#0071BB'>feminino</span>, e faixa etária, UnB, 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf

ggsave("graficos/graduacao/graf3.pdf", device = cairo_pdf)
pdftools::pdf_convert(pdf = "graficos/graduacao/graf3.pdf", 
                      filenames = "graficos/graduacao/graf3.png", 
                      format = "png", 
                      dpi = 500)
