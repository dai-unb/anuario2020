
library(tidyverse)
options(OutDec = ",")

# carrega os dados necessários

Arquivo42 <- readRDS("dados_identificados/Arquivo42.RDS")
load("dados_identificados/Completo14042020.RData")

# define o ano

anobase <- 2019

### define o ano do CENSO

sem1 <- 12019
sem2 <- 22019

# Componentes institucionais ----
comp_inst <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=1)

# Números da UnB ----

vagas_oferecidas <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=2)

inscritos <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=3)

demanda <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=4)


### Ingressantes

Arquivo42$forma_ingresso <- ifelse(Arquivo42$Ing_Enem==1,"Sisu/Enem",
                                     ifelse(Arquivo42$Ing_Pas==1,"PAS",
                                            ifelse(Arquivo42$Ing_Vest==1, "Vestibular",
                                                   ifelse(Arquivo42$Ing_Trans==1|
                                                            Arquivo42$Ing_Rem==1|
                                                            Arquivo42$Ing_PECG==1|
                                                            Arquivo42$Ing_Esp==1,"Por outras vias",0))))
  
  ingressantes <- Arquivo42 %>% 
    filter(Sem_Ingresso==sem1 | Sem_Ingresso==sem2) %>% 
    group_by(forma_ingresso,Sem_Ref) %>% 
    tally() %>% 
    spread(Sem_Ref,n)

  names(ingressantes)[1:3] <- c("INGRESSANTES", "1º Sem.","2º Sem.")   
  
  ingressantes <- ingressantes %>%
   filter(INGRESSANTES!=0)

 # adcionando uma linha de total 
 ingressantes <- adorn_totals(ingressantes, where = "row", fill = "-", na.rm = TRUE, name = "Total")
 
 # exportando a tabela
 rio::export(ingressantes, "dados_gerais/Tabela_ingressantes.xlsx")  

### Cursos 
 
### Diplomados  
 
 # graduação
 
 formado_grad <- Arquivo42
 formado_grad$grad <- "Graduação"
 formado_grad <- formado_grad %>% 
   filter(Vinculo=="Formado") %>% 
   group_by(Sem_Ref, grad) %>% 
   tally() %>% 
   spread(Sem_Ref,n)
 
 names(formado_grad)[1:3] <- c("DIPLOMADOS", "1º Sem.","2º Sem.")   
 
 # pós
 
 Mest <- Completo %>% 
   filter(`Ano Ingresso Opcao`<= anobase,
          `Ano Saida Opcao`>= anobase,
          `Nivel Curso`=="Mestrado",
          `Forma Saida` != "Anulacao de Registro")
 
 Mest$mest <-"Mestrado"
 
 
 formado_mest <- Mest %>% 
   filter(`Ano Saida Opcao`==anobase,
          (`Semestre Saida Opcao`==2|`Semestre Saida Opcao`==1),
          str_detect(`For. Saida Opcao`, "Formatura")) %>%
   group_by(mest,`Semestre Saida Opcao`) %>% 
   tally() %>% 
   spread(`Semestre Saida Opcao`,n)
  
 names(formado_mest)[1:3] <- c("DIPLOMADOS", "1º Sem.","2º Sem.") 

 Doc <- Completo %>% 
   filter(`Ano Ingresso Opcao` <= anobase,
          `Ano Saida Opcao` >= anobase,
          `Nivel` == "Doutorado",
          `Forma Saida` != "Anulacao de Registro")
 
 Doc$doc <-"Doutorado"
 
 formado_doc <- Doc %>% 
   filter(`Ano Saida Opcao`==anobase,
          (`Semestre Saida Opcao`==2|`Semestre Saida Opcao`==1),
          str_detect(`For. Saida Opcao`, "Formatura")) %>%
   group_by(doc,`Semestre Saida Opcao`) %>% 
   tally() %>% 
   spread(`Semestre Saida Opcao`,n)
 
 names(formado_doc)[1:3] <- c("DIPLOMADOS", "1º Sem.","2º Sem.") 
 
 
 formados <- rbind(formado_grad,formado_mest,formado_doc)
 rm(formado_grad,formado_mest,formado_doc)
 
 formados <- adorn_totals(formados, where = "row", fill = "-", na.rm = TRUE, name = "Total")
 
 
 
 escolaridade_docente <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=5)
 escolaridade_tecnico <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=6)
 area_fisica <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=7)
 bce <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=8)
 edu <- rio::import("dados_gerais/dados_gerais_tabelas.xlsx", sheet=9)

 
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