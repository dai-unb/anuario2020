### script para atualizar todas as tabelas
### e para ativar o RENV (gerenciamento de pacotes)
### vai ser construído aos poucos
options(encoding = "UTF-8", OutDec = ",", scipen = 9)

source("scripts/01_renv_setup.R")
source("scripts/02_pega_dados_censo.R")
source("scripts/03_gera_tabela_geral.R")
# source("scripts/04_gera_tabelas_graduacao.R")  - ARRUMAR ANTES
# source("scripts/05_gera_tabelas_mestrado.R")   - ARRUMAR ANTES
# source("scripts/06_gera_tabelas_doutorado.R")  - ARRUMAR ANTES


### compila o livro
### vou deixar comentado para só ser usado quando necessário
# bookdown::render_book("anuario2020.Rmd", encoding = "UTF-8")
# rstudioapi::viewer("_book/index.html")
