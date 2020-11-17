
library(ggtext)
library(ragg)

theme_set(theme_minimal(base_family = "Charter"))
fundo <- cores_anuario[2]

# graf de linhas da evolução da população universitária
graf1 <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=16)


graf <-
  graf1 %>% 
  select(Ano, 
         Grad = `Alunos de Graduação1`, 
         Posgrad = `Alunos de Pós-Graduação1`, 
         Doc = Docentes3, 
         TAE = `Técnico-Administrativos`, 
         Total) %>% 
  pivot_longer(-c(Ano, Total)) %>% 
  mutate(name = factor(name, levels = c("Grad",
                                        "Posgrad",
                                        "Doc",
                                        "TAE"))) %>% 
  ggplot(aes(x = value, y = as.character(Ano), fill = name)) +
  geom_col(position = position_stack(), width = 0.7) +
  coord_flip() +
  geom_text(aes(label = format(value, big.mark = "."), family = "Charter"), 
            size = 5,
            position = position_stack(vjust = 0.5), 
            color = "white") + 
  stat_summary(aes(x = Total, label = format(Total, big.mark = "."), group = Ano), 
               fun = 'mean', 
               geom = 'text', vjust = -1, family = "Charter", size = 5) +
  xlim(c(0,56000)) + 
  scale_fill_manual(values = c('#0071BB', '#008D46', '#FFCB06', '#303192')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_markdown(lineheight = 1.2),
        legend.position = "none") +
  labs(title = "Evolução da população universitária da UnB por categoria,<br><span style='color:#0071BB'>graduação</span>, <span style='color:#008D46'>pós-graduação</span>, <span style='color:#FFCB06'>docentes</span> e <span style='color:#303192'>TAEs</span>, 2015 a 2019",
       caption = "Fonte: Censo da Educação Superior 2019")
graf

ggsave("graficos/dados_gerais/graf1.pdf", device = cairo_pdf, width = 7)
pdftools::pdf_convert(pdf = "graficos/dados_gerais/graf1.pdf", 
                      filenames = "graficos/dados_gerais/graf1.png", 
                      format = "png", 
                      dpi = 250)
