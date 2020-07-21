
library(tidyverse)
options(OutDec = ",")

# carrega os dados necessários

Arquivo42 <- readRDS("dados_identificados/Arquivo42.RDS")
load("dados_identificados/Completo14042020.RData")

# define o ano

anobase <- 2019

# Graduação ---------------------------------------------------------------

Grad_Reg <- Arquivo42 %>% 
  filter(Sem_Ref==2 & Vinculo!="Transferido" & Vinculo != "Falecido") %>% 
  group_by(Unidade) %>% 
  summarise(Graduação=n()) %>% 
  mutate_if(is.factor, as.character)

Grad_Cur <- Arquivo42 %>% 
  group_by(Unidade) %>% 
  summarise(Grad.=n_distinct(Curso)) %>% 
  mutate_if(is.factor, as.character)

Grad_For <- Arquivo42 %>% 
  filter(Vinculo=="Formado") %>% 
  group_by(Unidade) %>% 
  summarise(Graduação=n()) %>% 
  mutate_if(is.factor, as.character)


# Pós-graduação -----------------------------------------------------------

Mest <- Completo %>% 
  filter(`Ano Ingresso Opcao`<=anobase & `Ano Saida Opcao`>=anobase, `Nivel`=="Mestrado")

Mest_Reg <- Mest %>% 
  filter(`Ano Ingresso Opcao`<= anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>%
  group_by(Unidade) %>% 
  summarise(Mestrado=n())

Mest_Cur <- Mest %>% 
  group_by(Unidade) %>% 
  summarise(Mestr.=n_distinct(`Nome Curso`))

Mest_For <- Mest %>% 
  filter(`Ano Saida Opcao` == anobase,
         str_detect(`For. Saida Opcao`, "Formatura")) %>% 
  group_by(Unidade) %>% 
  summarise(Mestrado=n())


Doc <- Completo %>% 
  filter(`Ano Ingresso Opcao`<=anobase & `Ano Saida Opcao`>=anobase,
         `Nivel`=="Doutorado")

Doc_Reg <- Doc %>% 
  filter(`Ano Ingresso Opcao`<=anobase & (`Ano Saida Opcao`>anobase | (`Ano Saida Opcao`==anobase & `Semestre Saida Opcao`==2))) %>%
  group_by(Unidade) %>% 
  summarise(Doutorado=n())

Doc_Cur <- Doc %>% 
  group_by(Unidade) %>% 
  summarise(Dout.=n_distinct(`Nome Curso`))

Doc_For <- Doc %>% 
  filter(`Ano Saida Opcao`==anobase,
         str_detect(`For. Saida Opcao`, "Formatura")) %>% 
  group_by(Unidade) %>% 
  summarise(Doutorado=n())

Pos_Reg <- full_join(Mest_Reg, Doc_Reg) %>% 
  mutate(Pos_Graduação = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>% 
  select(Unidade, Pos_Graduação)

Tabela1.03 <- full_join(Grad_Reg, Pos_Reg, by = "Unidade") %>% 
  full_join(Grad_Cur, by = "Unidade") %>% 
  full_join(Mest_Cur, by = "Unidade") %>% 
  full_join(Doc_Cur, by = "Unidade") %>% 
  full_join(Grad_For, by = "Unidade") %>% 
  full_join(Mest_For, by = "Unidade") %>% 
  full_join(Doc_For, by = "Unidade") %>% 
  janitor::adorn_totals()

rm(Completo, Grad_Reg, Pos_Reg, Grad_Cur, Mest_Cur, Doc_Cur, Grad_For, Mest_For, Doc_For, Arquivo42, Mest, Doc, Doc_Reg, Mest_Reg)

rio::export(Tabela1.03, "dados_gerais/Tabela_1.03.xlsx")