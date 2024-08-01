#################################################################
########### Extração de Dados#######################
######## Pacotes necessários
library(tidyverse)
library(fredr)
library(Quandl)
library(BETS)
library(sidrar)
library(rbcb) 
library(ipeadatar)
library(tidyipea)
library(OECD)
library(rio)
library(janitor)
library(tsibble)
library(timetk)
library(tidyr)
library(readxl)
library(stringr)
library(lubridate)
library(labelled)
library(scales)
library(owidR)
library(tstools)   # devtools::install_github("leripio/tstools")
library(WDI)
library(IndexNumber)
library(tidyr)



# ---------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls())

setwd("C:\\Users\\Lucasma\\OneDrive - Inter-American Development Bank Group\\Documents\\IAMC_NEW")

# ---------------------------------------------------------------------------------------------------------------------------------------

codes_ipea <- codes_ipea()

write.csv(codes_ipea, file = "codes_ipea.csv")


# ---------------------------------------------------------------------------------------------------------------------------

# COVID - Our World in Data:


covid <- owid_covid() 

covid_cases_deaths <- covid %>% 
  filter(iso_code %in% c("BRA")) %>% 
  select(location, date, new_cases_smoothed, new_deaths_smoothed)

covid_vaccionation <- covid %>%
  filter(iso_code %in% c("BRA", "ARG", "GBR", "USA")) %>% 
  select(location, date, people_vaccinated_per_hundred) %>% 
  pivot_wider(names_from = location, values_from = people_vaccinated_per_hundred) %>% 
  fill(-1) %>% 
  select(date, Brazil, `United States`, `United Kingdom`, Argentina) %>% 
  filter(date >= "2020-02-26")


covid_chart <- full_join(covid_cases_deaths %>% mutate(espaco = "----"), covid_vaccionation)



write.csv(covid_chart, "COVID - Our World in Data.csv")


rm(covid)

# --------------------------------------------------------------------------------------------------------------------------

covid <- owid_covid()

covid_USA <- covid %>% 
  filter(iso_code == "USA") %>%
  select(date, `Casos - EUA` = new_cases_smoothed, `Mortes - EUA` = new_deaths_smoothed)
  
covid_ZAF <- covid %>% 
  filter(iso_code == "ZAF") %>%
  select(date, `Casos - África do Sul` = new_cases_smoothed, `Mortes - África do Sul` = new_deaths_smoothed)
  
covid_GBR <- covid %>% 
  filter(iso_code == "GBR") %>%
  select(date, `Casos - Reino Unido` = new_cases_smoothed, `Mortes - Reino Unido` = new_deaths_smoothed)
  
covid_DEU <- covid %>% 
  filter(iso_code == "DEU") %>%
  select(date, `Casos - Alemanha` = new_cases_smoothed, `Mortes - Alemanha` = new_deaths_smoothed)
  
covid_NLD <- covid %>% 
  filter(iso_code == "NLD") %>%
  select(date, `Casos - Holanda` = new_cases_smoothed, `Mortes - Holanda` = new_deaths_smoothed)
  
covid_CHN <- covid %>% 
  filter(iso_code == "CHN") %>%
  select(date, `Casos - China` = new_cases_smoothed, `Mortes - China` = new_deaths_smoothed)
  

covid_cases_deaths_mundo <- full_join(covid_USA, covid_ZAF) %>%
  full_join(covid_GBR) %>% 
  full_join(covid_DEU) %>% 
  full_join(covid_NLD) %>% 
  full_join(covid_CHN) %>% 
  select(1, 2, 4, 6, 8, 10, 12, everything())

rm(covid_USA, covid_ZAF, covid_GBR, covid_DEU, covid_NLD, covid_CHN, covid)

write.csv(covid_cases_deaths_mundo, "COVID - Casos e Mortes no Mundo.csv")
# write.csv2(covid_cases_deaths_mundo, "COVID - Casos e Mortes no Mundo.csv")

# -------------------------------------------------------------------------------------------------------------------

# Crescimento do PIB - Diversos Países


# Crescimento do PIB em Diversos Países:

url <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2021/WEOOct2021all.ashx"
download.file(url, destfile = 'WEO.xls', mode = 'wb')
dir()

WEO_growth <- read.delim("WEO.xls", na = c("", "n/a", "--"))

WEO_growth <- WEO_growth %>%
  select(ISO, Country, WEO.Subject.Code, `X1980`:`X2021`) %>%
  pivot_longer(names_to = "ano", names_prefix = "X", values_to = "valor", cols = `X1980`:`X2021`) %>%
  pivot_wider(names_from = WEO.Subject.Code, values_from = valor) %>%
  select(ISO, Country, ano, gdp_growth = NGDP_RPCH) %>%
  filter(ISO %in% c("BRA", "CHL", "USA", "CHN", "GBR")) %>%
  select(-ISO) %>%
  mutate(gdp_growth = as.numeric(gdp_growth)/100) %>%
  mutate(gdp_growth = label_percent(accuracy = 0.01)(gdp_growth)) %>%
  pivot_wider(names_from = Country, values_from = gdp_growth, names_sep = " - ", names_prefix = "gdp_growth - ")


write.csv(WEO_growth, file = "WEO_growth.csv")



# PIB Real em Diversos Países:
WEO_Real_GDP_2010 <- read.delim("WEO.xls", na = c("", "n/a", "--"))

WEO_Real_GDP_2010 <- WEO_Real_GDP_2010 %>%
  select(ISO, Country, WEO.Subject.Code, `X1980`:`X2021`) %>%
  pivot_longer(names_to = "ano", names_prefix = "X", values_to = "valor", cols = `X1980`:`X2021`) %>%
  pivot_wider(names_from = WEO.Subject.Code, values_from = valor) %>%
  select(ISO, Country, ano, real_gdp = NGDP_R) %>%
  filter(ISO %in% c("BRA", "CHL", "USA", "CHN", "GBR")) %>%
  select(-ISO) %>%
  pivot_wider(names_from = Country, values_from = real_gdp, names_sep = " - ", names_prefix = "Real GDP - ") %>%
  mutate_at(-1, str_replace, pattern = ",", replacement = "") %>% 
  mutate_at(-1, as.numeric) %>% 
  mutate(`Real GDP - Chile` = (`Real GDP - Chile`/118521.37)* 100) %>%
  mutate(`Real GDP - Brazil` = (`Real GDP - Brazil`/1127.060)* 100) %>%
  mutate(`Real GDP - China` = (`Real GDP - China`/47294.46)* 100) %>%
  mutate(`Real GDP - United Kingdom` = (`Real GDP - United Kingdom`/1849.250)* 100) %>%
  mutate(`Real GDP - United States` = (`Real GDP - United States`/15649.00)* 100)

write.csv(WEO_Real_GDP_2010, file = "WEO_Real_GDP_2010.csv")




# ---------------------------------------------------------------------------------------------------------------------------------------

# Chart 1 - GDP

# Sidra - Tabela 6613
GDP <- get_sidra(api = "/t/6613/n1/all/v/all/p/all/c11255/90687,90691,90696,90705,90707,93404,93405,93406,93407,93408/d/v9319%202")

# GDP <- get_sidra(api = "/t/6613/n1/all/v/all/p/all/c11255/all/d/v9319%202")

GDP <- GDP %>%
  select(ano_trimestre = `Trimestre (Código)`, Trimestre, setor = `Setores e subsetores`, Valor) %>%
  separate(ano_trimestre, into = c("ano", "trimestre"), sep = 4) %>%
  pivot_wider(names_from = setor, values_from = Valor)

colnames(GDP) <- c("ano", "trimestre", "periodo", "agropecuaria", "industria", "servicos", "VA", "PIB", "C", "G", "I", "X", "M")
  
GDP <- GDP %>%
  mutate(imposto = PIB - VA) %>% 
  select(-ano, -trimestre) %>%
  select(periodo, PIB, agropecuaria, industria, servicos, imposto, C, G, I, X, M)
  

# Sidra - Tabela 5932
PIB <- get_sidra(api = "/t/5932/n1/all/v/6561,6562,6564/p/all/c11255/90707/d/v6561%201,v6562%201,v6564%201")

PIB <- PIB %>% 
  select(periodo = Trimestre, variavel = Variável, Valor) %>% 
  pivot_wider(names_from = variavel, values_from = Valor) %>% 
  select(1, 3, 2, 4)

chart_1 <- full_join(GDP, PIB)
write.csv(chart_1, file = "pib1t.csv")

# ------------------------------------------------------------------------------------------------------------------------------------

# Panel 1 and 2 - Indicators

# Índice de Atividade Econômica do Banco Central (IBC-Br) - com ajuste sazonal
`24364_IBC_Br_dessazonalizado` <- rbcb::get_series(c(`24364_IBC_Br_dessazonalizado` = 24364), start_date = "2019-01-01")
panel_1_2_IBC_BR <- `24364_IBC_Br_dessazonalizado` %>% mutate(date = format(date, format = "%Y - %b"))
rm(`24364_IBC_Br_dessazonalizado`)
write.csv(panel_1_2_IBC_BR, file = "panel_1_2_IBC_br.CSV")


# --------------------------------------------------------------------------------------------------------------

# ICVA - Cielo

url <- "https://api.mziq.com/mzfilemanager/v2/d/4d1ebe73-b068-4443-992a-3d72d573238c/163a9070-918c-2f20-035e-b40da951bc9f?origin=2"
download.file(url, destfile = 'planilha_icva.xlsx', mode = 'wb')

ICVA <- read_xlsx('planilha_icva.xlsx', sheet = 1, skip = 6) %>%
  head(n = 1) %>% 
  select(-Setor, -Localidade, -Visão)

colnames_ICVA <- as.character(1:1000)[1:ncol(ICVA)]
colnames(ICVA) <-  colnames_ICVA

ICVA <- ICVA %>%
  select(`73`:`106`) %>% 
  pivot_longer(cols = `73`:`106`, names_to = "obs")

ano <- c(rep(2019, 12),
         rep(2020, 12),
         rep(2021, 12),
         rep(2022,12))[1:nrow(ICVA)]

mes <- rep(1:12, 100)[1:nrow(ICVA)]

ICVA <- cbind(ano, mes, ICVA) %>% 
  select(-obs) %>% 
  unite(col = "data", ano, mes, sep = "-") %>% 
  mutate(data = ym(data)) %>%
  mutate(data = format(data, format = "%Y - %b")) %>% 
  mutate(`ICVA - Deflacionado - Com Ajuste Calendário` = label_percent(accuracy = 0.1)(value)) %>% 
  select(-value)

rm(ano, mes, url)

write.csv(ICVA, file = "panel_1_2_ICVA.CSV")
# ----------------------------------------------------------------------------------------------------------------

# Dados da OCDE - Weekly Tracker of Brazilian GDP
# OECD::get_datasets()

url <- "https://github.com/NicolasWoloszko/OECD-Weekly-Tracker/raw/main/Data/Weekly_Tracker_Excel.xlsx"
download.file(url, destfile = 'planilha_ocde.xlsx', mode = 'wb')

OCDE <- read_xlsx('planilha_ocde.xlsx', sheet = "Brazil") %>% 
  select(date, `Tracker (yoy)`) 

# %>% mutate(date = format(date, format = "%b-%d-%Y"))

write.csv(OCDE, file = "panel_1_2_OCDE.CSV")
# write.csv2(OCDE, file = "panel_1_2_OCDE.CSV")


# ------------------------------------------------------------------------------------------------------------------

# ICEI - CNI

url <- "https://static.portaldaindustria.com.br/media/filer_public/76/3f/763f6c5a-6c77-401c-8185-22a63163b917/indicedeconfiancadoempresarioindustrial_serierecente_abril2023.xlsx"
download.file(url, destfile = 'planilha_ICEI.xlsx', mode = 'wb')

ICEI <- read_xlsx('planilha_ICEI.xlsx', skip = 7) %>%
  tail(n = -1) %>% 
  head(n = 1) %>% 
  select(-`...1`, -`...2`)

colnames_ICEI <- as.character(1:length(ICEI))

colnames(ICEI) <- colnames_ICEI
 
ICEI <- ICEI %>%
  pivot_longer(cols = everything(), names_to = "obs", values_to = "ICEI")

ano <- c(rep(2010:2025, each = 12))[1:nrow(ICEI)]

mes <- rep(1:12, times = 100)[1:nrow(ICEI)]

ICEI <- cbind(ano, mes, ICEI) %>%
  select(-obs) %>%
  unite(col = "data", ano, mes, sep = "-") %>% 
  mutate(data = ym(data)) %>%
  mutate(data = format(data, format = "%Y - %b")) %>% 
  filter(data >= "2018-01-01") %>%
  mutate(Coluna = "-----------------", aviso = "O link precisa ser atualizado. Ou pelo menos o mês do link")
  
rm(ano, mes, url, colnames_ICEI)

write.csv(ICEI, file = "panel_1_2_ICEI.CSV")
# write.csv2(ICEI, file = "panel_1_2_ICEI.CSV")

# --------------------------------------------------------------------------------------------------------------
# UCI da CNI com dados do IBGE PM-PF - Mesma Defasagem das Pesquisas do IBGE - É a UCI mais robusta. 
# Utilização da Capacidade Instalada - CNI UCI Dessazonalizada:
url <- "https://static.portaldaindustria.com.br/media/filer_public/22/8f/228f187e-1573-4d13-a79e-9b790dd0334a/indicadoresindustriais_serie-recente_fevereiro2023.xlsx"
download.file(url, destfile = 'CNI_UCI.xlsx', mode = 'wb')

UCI <- read_xlsx('CNI_UCI.xlsx', sheet = "Indicadores", skip = 14)
UCI <- UCI[-1,]
UCI <- UCI %>% 
  head(n = -2) %>% 
  select(mes = Meses, UCI_dessazonalizada = `Utilização da Capacidade Instalada...39`) %>% 
  mutate(Coluna = "-----------------", aviso = "O link precisa ser atualizado. Ou pelo menos o mês do link") %>% 
  mutate(UCI_dessazonalizada = round(as.numeric(UCI_dessazonalizada), 1))

write.csv(UCI, file = "panel_1_2_UCI.CSV")
# --------------------------------------------------------------------------------------------------------------
# Pesquisa Industrial Mensal - PIM PF - Índice de base fixa com ajuste sazonal (Base: média de 2012 = 100)
PIM <- get_sidra(api = "/t/8159/n1/all/v/11600/p/all/c544/129314/d/v11600%201") %>% 
  select(mes = Mês, pim_base_fixa_sa = Valor) %>% 
  tail(n = -108)
# Pesquisa Mensal de Serviços - PMS - Índice de volume de serviços
PMS <- get_sidra(api = "/t/8161/n1/all/v/11622/p/all/c11046/56725/d/v11622%201") %>% 
  select(mes = Mês, pms_indice_volume = Valor)
# Pesquisa Mensal de Comércio - PMC - Índice de volume de vendas no comércio varejista - Tabela 8185
PMC <- get_sidra(api = "/t/8185/n1/all/v/11707/p/all/c11046/56734/d/v11707%201") %>%
  select(mes = Mês, pmc_indice_volume = Valor) %>% 
  tail(n = -132)
painel_1_2_PIM_PMC_PMS <- full_join(PIM, PMC) %>% full_join(PMS)

write.csv(painel_1_2_PIM_PMC_PMS, file = "painel_1_2_PIM_PMC_PMS.CSV", row.names = F)
# ---------------------------------------------------------------------------------------------------------------
# Índice ABCR - Tráfego de Veículos:
library(rvest)
abcr_webpage = "https://melhoresrodovias.org.br/indice-abcr/"
abcr_site = read_html(abcr_webpage)
tabela_abcr <- abcr_site %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset(., "\\.xls$")
ultimo.abcr = sort(tabela_abcr, decreasing = TRUE)[1]
temp_file = tempfile(fileext = "abcr.xls")
download.file(ultimo.abcr, temp_file,mode = "wb")
sheet <- "(C) Original"
data.abcr <- read_excel(temp_file, sheet = sheet)
colnames(data.abcr)
colunas.abcr <- c("Brasil","...3","...4")
df.abcr <- data.abcr[,colunas.abcr]
tail(df.abcr)
ABCR = data.abcr[3:292,1:4]
tail(ABCR)
colnames(ABCR) <- c("data","leves","pesados","total")
# as colunas estão em caracteres e precisam ser convertidas em numeric
ABCR <- ABCR %>% 
  mutate(across(c(leves, pesados, total), ~as.numeric(.)))
abcr.ts = xts(ABCR[,c("leves","pesados","total")], order.by = ABCR$data)
write.csv(abcr.ts, file = "ABCE_trafego.CSV")

# ------------------------------------------------------------------------------------------------------------------------------------
# Chart 2 - Expectations
# Expectativas para o crescimento do PIB (referentes a 2021 e 2022):
chart_2 <- rbcb::get_annual_market_expectations(c('PIB Total', 'IPCA', 'Taxa de câmbio', 'Meta para taxa over-selic','IGP-M'),
                                                start_date = '2021-01-01')
chart_2 <- chart_2 %>%
  filter(indic == "PIB Total", reference_date %in% c(2023, 2024, 2025), base == 0) %>% 
  select(date, reference_date, median) %>% 
  pivot_wider(names_from = reference_date, values_from = median, names_prefix = "GDP_growth_expectations_") %>% 
  arrange(date) %>%
  mutate(GDP_growth_expectations_2023 = GDP_growth_expectations_2023/100) %>% 
  mutate(GDP_growth_expectations_2024 = GDP_growth_expectations_2024/100) %>% 
  mutate(GDP_growth_expectations_2025 = GDP_growth_expectations_2025/100) %>% 
  mutate(GDP_growth_expectations_2023 = label_percent(accuracy = 0.01)(GDP_growth_expectations_2023)) %>% 
  mutate(GDP_growth_expectations_2024 = label_percent(accuracy = 0.01)(GDP_growth_expectations_2024)) %>% 
  mutate(GDP_growth_expectations_2025 = label_percent(accuracy = 0.01)(GDP_growth_expectations_2025))
  
  
tail(chart_2)  
  

write.csv(chart_2, file = "C:/Users/Lucasma/OneDrive - Inter-American Development Bank Group/Documents/IAMC_NEW/focus_pib.csv")



# ---------------------------------------------------------------------------------------------------------------------------------------

# Chart 3 - Labor

# Taxa de desocupação - PNADC
taxa_desemprego <- ipeadatar::ipeadata("PNADC12_TDESOC12") %>% select(date, taxa_desemprego = value) %>% 
  arrange(desc(date))

# Saldo de Contratações - CAGED - SEM AJUSTE
caged_novo <- ipeadatar::ipeadata("CAGED12_SALDON12") %>% select(date, saldo_caged_sem_ajuste = value)
caged_antigo <- ipeadatar::ipeadata("CAGED12_SALDO12") %>% select(date, saldo_caged_sem_ajuste = value)
caged <- rbind(caged_antigo, caged_novo) %>% arrange(desc(date))
tail(caged_novo)
chart_3 <- full_join(taxa_desemprego, caged) %>% arrange(date) %>% filter(date >= "2018-01-01") %>% 
  mutate(date = format(date, format = "%b - %Y"))
rm(caged, caged_antigo, caged_novo, taxa_desemprego)
write.csv(chart_3, file = "chart_3_01_12_1.csv")
# -----------------------------------------------------------------------------------------------------------------------
# Chart3_b - Taxa de Participação na Força de Trabalho - IBGE PNADC
taxa_participacao <- ipeadatar::ipeadata(code = "PNADC12_TPART12") %>%
  mutate(value = value/100) %>%
  mutate(value = label_percent(accuracy = 0.1)(value)) %>%
  transmute(date = format(date, format = "%b-%Y"), `Taxa de Participação na Força de Trabalho (%)` = value) %>%
  remove_var_label()
chart_3b <- taxa_participacao
rendimento_medio <- ipeadatar::ipeadata(code = "PNADC12_RRTH12") %>%
  transmute(date = format(date, format = "%b-%Y"), `Rendimento Real Médio de Todos os Trabalhos, Habitualmente Recebido por Mês (R$)` = value) %>%
  remove_var_label()

chart_3b <- full_join(taxa_participacao, rendimento_medio) 

rm(taxa_participacao, rendimento_medio)

write.csv(chart_3b, file = "chart_3b_01_12.csv")
# ------------------------------------------------------------------------------------------------------------------------

# Dívida Líquida do Setor Público (% PIB) - SGS BACEN - Série 4513
`4513_DLSP_%` <- rbcb::get_series(c(`4513_DLSP_%` = 4513), start_date = "2019-01-01")

# Dívida bruta do governo geral (% PIB) - Metodologia utilizada a partir de 2008 - % - SGS BACEN - Série 13762
`13762_DBGG_%` <- rbcb::get_series(c(`13762_DBGG_%` = 13762), start_date = "2019-01-01")

# Dívida bruta do governo geral (% PIB) - Metodologia utilizada até 2007 - % - SGS BACEN - Série 4537
`4537_DBGG_antiga_%` <- rbcb::get_series(c(`4537_DBGG_antiga_%` = 4537), start_date = "2019-01-01")

chart_6_debt <- full_join(`4513_DLSP_%`, `13762_DBGG_%`) %>% full_join(`4537_DBGG_antiga_%`) %>% 
  mutate(date = format(date, format = "%b - %Y"))

rm(`4513_DLSP_%`, `13762_DBGG_%`, `4537_DBGG_antiga_%`)

tail(chart_6_debt)

  
write.csv(`chart_6_debt`, file = "chart_6_debt.csv")
# write.csv2(`chart_6_debt`, file = "chart_6_debt.csv")

# ---------------------------------------------------------------------------------------------------------------------------------------


# # Chart 7 - Emissão de Dívida
 
url <- "https://sisweb.tesouro.gov.br/apex/f?p=2501:9::::9:P9_ID_PUBLICACAO_ANEXO:21362"
download.file(url, destfile = 'resultado.xlsx', mode = 'wb')
 
RTN <- read_excel("Anexo_RMD_Setembro_23.zip", sheet = "1.1", skip = 4, col_names = T)
  

# Olá, Felipe. Tente usar a função unzip.
# 
# unzip("Anexo_RMD_Setembro_23.zip")
# read_excel("Anexo_RMD_Set_21.xlsx")






# ---------------------------------------------------------------------------------------------------------------------------------------


# Chart 9c - Duração (Duration) da Dívida

# SGS BACEN 10617 - Dívida mobiliária federal - Títulos do Tesouro Nacional - Emitidos em oferta pública - Duração média - Total - Meses
chart_9c <- rbcb::get_series(c(`10617_duration` = 10617), start_date = "2018-01-01") %>% 
  mutate(date = format(date, format = "%b - %Y"))

tail(chart_9c)

write.csv(`chart_9c`, file = "chart_9c.csv")
# write.csv2(`chart_9c`, file = "chart_9c.csv")


# ---------------------------------------------------------------------------------------------------------------------------------------

# General government structural balance

# Extraindo a base de dados World Economic Outlook (WEO) - FMI
url <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2022/WEOOct2022all.ashx"
download.file(url, destfile = 'WEO.xls', mode = 'wb')
dir()

WEO <- read.delim("WEO.xls", na = c("", "n/a", "--"))

WEO <- WEO %>%
  select(ISO, Country, WEO.Subject.Code, `X1980`:`X2026`) %>% 
  pivot_longer(names_to = "ano", names_prefix = "X", values_to = "valor", cols = `X1980`:`X2026`) %>% 
  pivot_wider(names_from = WEO.Subject.Code, values_from = valor) %>% 
  select(ISO, Country, ano, structural_balance = GGSB_NPGDP) %>% 
  filter(ISO == "BRA") %>% 
  select(-ISO) %>%
  mutate(structural_balance = as.numeric(structural_balance)/100) %>% 
  mutate(structural_balance = label_percent(accuracy = 0.01)(structural_balance))
  
  
write.csv(WEO, file = "Structural Balance.csv")


# ----------------------------------------------------------------------------------------------------------------

# Yield Curve

# Ver a função get.yield.curve() do pacote "GetTDData"

# Ver a função yields() do pacote "GBcurves"











# ------------------------------------------------------------------------------------------------------------------------



# Chart 15 and 16 - Balança Comercial - BACEN

# SE ESTIVER DANDO ERRRO, SEPARAR A EXTRAÇÃO DO COMANDO MUTATE
library(dplyr)
library(rbcb)
# SGS Bacen 24419 - Transações Correntes acumulado em 12 meses - mensal - US$ (Milhões)
`24419_transacoes_correntes` <- rbcb::get_series(c(`24419_transacoes_correntes_12m_million` = 24419), start_date = "2019-01-01")
`24419_transacoes_correntes` <- `24419_transacoes_correntes` %>% 
  mutate(`24419_transacoes_correntes_12m_billion` = `24419_transacoes_correntes_12m_million`/1000)
# SGS Bacen 23079 - Transações Correntes acumulado em 12 meses em relação ao PIB - mensal - %
`23079_transacoes_correntes` <- rbcb::get_series(c(`23079_transacoes_correntes_12m_pib` = 23079), start_date = "2019-01-01")

# SGS Bacen 23080 - Investimento Direto no País acumulado em 12 meses em relação ao PIB - mensal - %
`23080_IDP_12m_pib` <- rbcb::get_series(c(`23080_IDP_12m_pib` = 23080), start_date = "2019-01-01")
# SGS Bacen 22885 - Investimento Direto no País  - mensal - US$ (Milhões)- net - US$ (million)
`22885_IDP_mensal_million` <- rbcb::get_series(c(`22885_IDP_mensal_million` = 22885), start_date = "2019-01-01")
# Tentar fazer a função que calcula 12 meses acumulado
# SGS Bacen 4192 - PIB acumulado dos últimos 12 meses - Em US$ milhões - US$ (milhões)
`4192_pib_acumulado_12m` <- rbcb::get_series(c(`4192_pib_acumulado_12m` = 4192), start_date = "2019-01-01")
# SGS Bacen 22930 - Investimentos em carteira - ações - no país - mensal - líquido - US$ (milhões)
`22930_investimento_carteira_acoes` <- rbcb::get_series(c(`22930_investimento_carteira_acoes` = 22930), start_date = "2019-01-01")
# SGS Bacen 22936 - Investimentos em carteira - fundos de investimento - passivos - mensal - líquido - US$ (milhões)
`22936_investimento_carteira_fundos` <- rbcb::get_series(c(`22936_investimento_carteira_fundos` = 22936), start_date = "2019-01-01")
# SGS Bacen 22942 - Investimentos em carteira - Títulos de dívida - passivos - negociados no mercado doméstico - mensal - líquido - US$ (milhões)
`22942_investimento_carteira_titulos` <- rbcb::get_series(c(`22942_investimento_carteira_titulos` = 22942), start_date = "2019-01-01")
`chart_15_16` <- full_join(`24419_transacoes_correntes`, `23079_transacoes_correntes`) %>%
  full_join(`23080_IDP_12m_pib`) %>% 
  full_join(`22885_IDP_mensal_million`) %>%
  full_join(`4192_pib_acumulado_12m`) %>%
  full_join(`22930_investimento_carteira_acoes`) %>%
  full_join(`22936_investimento_carteira_fundos`) %>%
  full_join(`22942_investimento_carteira_titulos`) %>% 
  mutate(date = format(date, format = "%b - %Y"))
  

write.csv(`chart_15_16`, file = "chart_15_16_all.csv")
# write.csv2(`chart_15_16`, file = "chart_15_16.csv")

# --------------------------------------------------------------------------------------------------------------------------------------------


# Chart 17 - Balança Comercial - SECEX

## Importar via readxl
library(utils)
url <- "https://balanca.economia.gov.br/balanca/mes/2021/BCB002.xlsx"
destfile <- "BCB002_chart_17.xlsx"
download.file(url, destfile, mode = 'wb')


# --------------------------------------------------------------------------------------------------------------------------------------------

# Common Global Factor e Taxa de Câmbio
endereco <- "C:/Users/jeduardogo/Inter-American Development Bank Group/CSCUSER - CSC Main/High Frequency Indicators/CSC High Frequency Indicators.xlsx"
Common_Global_Factor <- read_xlsx(endereco, sheet = "calc", skip = 3) %>% 
  select(date = `...2`, BRAZILF = BRAZILF...27, BRL_CURNCY) %>% 
  filter(date > "2019-12-31") %>%
  mutate(BRAZILF = round(BRAZILF, 2), BRL_CURNCY = round(BRL_CURNCY, 2))
chart_18 <- Common_Global_Factor
rm(Common_Global_Factor)
write.csv(chart_18, file = "chart_18.csv")
# write.csv2(chart_18, file = "chart_18.csv")


# --------------------------------------------------------------------------------------------------------------------------------------------
# Chart 20 - Inflação 
# SE ESTIVER DANDO ERRRO, SEPARAR A EXTRAÇÃO DO COMANDO MUTATE
# IPCA - PCA - Variação acumulada em 12 meses:
library(dplyr)
library(tidyverse)
library(sidrar)
ipca_variacao <- get_sidra(api = "/t/7060/n1/all/v/63,69,2265/p/all/c315/7169/d/v63%202,v69%202,v2265%202") %>% 
  select(data = Mês, variavel = Variável, valor = Valor) %>%
  pivot_wider(names_from = variavel, values_from = valor) %>% 
  rename(ipca_variacao_mensal = `IPCA - Variação mensal`,
         ipca_variacao_acumulada_ano = `IPCA - Variação acumulada no ano`,
         `ipca_variacao_acumulada_12M` = `IPCA - Variação acumulada em 12 meses`) %>% 
  select(data, `ipca_variacao_acumulada_12M`)


# SGS BACEN 4466 - IPCA - Núcleo médias aparadas com suavização
# SGS BACEN 4466 - IPCA - Smoothed trimmed mean Core IPCA
#`4466_smoothed_trimmed_mean_core_IPCA` <- rbcb::get_series(c(`4466_smoothed_trimmed_mean_core_IPCA` = 4466), start_date = "2014-01-01")

# SGS BACEN 1635 - Índice nacional de preços ao consumidor-Amplo (IPCA) - Alimentação e bebidas - Var. % mensal
`1635_inflacao_food` <- rbcb::get_series(c(`1635_inflacao_food` = 1635), start_date = "2018-01-01")


# SGS BACEN 1636 - Índice nacional de preços ao consumidor-Amplo (IPCA) - Habitação - Var. % mensal
`1636_inflacao_housing` <- rbcb::get_series(c(`1636_inflacao_housing` = 1636), start_date = "2018-01-01")


# SGS BACEN 1637 - Índice nacional de preços ao consumidor-Amplo (IPCA) - Artigos de residência -	Var. % mensal
`1637_inflacao_house_equipment` <- rbcb::get_series(c(`1637_inflacao_house_equipment` = 1637), start_date = "2018-01-01")


# SGS BACEN 1638 - Índice nacional de preços ao consumidor-Amplo (IPCA)- Vestuário -	Var. % mensal
`1638_inflacao_clothing` <- rbcb::get_series(c(`1638_inflacao_clothing` = 1638), start_date = "2018-01-01")


# SGS BACEN 1639 - Índice de nacional de preços ao consumidor-Amplo (IPCA) - Transportes - 	Var. % mensal
`1639_inflacao_transport` <- rbcb::get_series(c(`1639_inflacao_transport` = 1639), start_date = "2018-01-01")


# SGS BACEN 1640 - Índice nacional de preços ao consumidor-Amplo (IPCA) - Comunicação - Var. % mensaL
`1640_inflacao_communication` <- rbcb::get_series(c(`1640_inflacao_communication` = 1640), start_date = "2018-01-01")


# SGS BACEN 1641 - Índice nacional de preços ao consumidor-Amplo (IPCA) - Saúde e cuidados pessoais % - - Var. % mensal
`1641_inflacao_health` <- rbcb::get_series(c(`1641_inflacao_health` = 1641), start_date = "2018-01-01")

# SGS BACEN 1642 - Índice nacional de preços ao consumidor-Amplo (IPCA) - Despesas pessoais	- Var. % mensal
`1642_inflacao_personal_expenses` <- rbcb::get_series(c(`1642_inflacao_personal_expenses` = 1642), start_date = "2018-01-01")

# SGS BACEN 1643 - Índice nacional de preços ao consumidor-Amplo (IPCA) - Educação - Var. % mensal
`1643_inflacao_education` <- rbcb::get_series(c(`1643_inflacao_education` = 1643), start_date = "2018-01-01")

inflacao_variacao_grupos <- full_join(`1635_inflacao_food`, `1636_inflacao_housing`) %>% 
  full_join(`1637_inflacao_house_equipment`) %>%
  full_join(`1638_inflacao_clothing`) %>%
  full_join(`1639_inflacao_transport`) %>%
  full_join(`1640_inflacao_communication`) %>%
  full_join(`1641_inflacao_health`) %>%
  full_join(`1642_inflacao_personal_expenses`) %>%
  full_join(`1643_inflacao_education`)

inflacao_antigos_variacao <- inflacao_variacao_grupos %>% filter(date < "2020-01-01")
inflacao_recentes_variacao <- inflacao_variacao_grupos %>% filter(date >= "2020-01-01")


rm(`1635_inflacao_food`, `1636_inflacao_housing`, `1637_inflacao_house_equipment`, `1638_inflacao_clothing`, `1639_inflacao_transport`,
   `1640_inflacao_communication`, `1641_inflacao_health`, `1642_inflacao_personal_expenses`, `1643_inflacao_education`)


# Extraindo dados do SIDRA:

# IPCA - Peso mensal (%):
ipca_peso_mensal <- sidrar::get_sidra(api = "/t/7060/n1/all/v/66/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v66%204")
ipca_peso_mensal <- ipca_peso_mensal %>% 
  select(data = `Mês`, grupo = `Geral, grupo, subgrupo, item e subitem`, peso = Valor) %>%
  pivot_wider(names_from = grupo, values_from = peso) %>% 
  mutate_at(.vars = c(-1), .funs = function(x) {x/100}) %>% 
  select(data,
         `Peso - Alimentação e bebidas` = `1.Alimentação e bebidas`,
         `Peso - Habitação` = `2.Habitação`,
         `Peso - Artigos de residência` = `3.Artigos de residência`,
         `Peso - Vestuário` = `4.Vestuário`,
         `Peso - Transportes` = `5.Transportes`,
         `Peso - Comunicação` = `9.Comunicação`,
         `Peso - Saúde e cuidados pessoais` = `6.Saúde e cuidados pessoais`,
         `Peso - Despesas pessoais` = `7.Despesas pessoais`,
         `Peso - Educação` = `8.Educação`)
  
  
# Unindo as duas bases de dados
chart_20 <- cbind(inflacao_recentes_variacao, ipca_peso_mensal)
chart_20 <- full_join(chart_20, inflacao_antigos_variacao) %>% arrange(date)
chart_20 <- full_join(ipca_variacao, chart_20) %>% arrange(date) %>% 
  mutate(date = format(date, format = "%b-%Y"))

rm(inflacao_antigos_variacao, inflacao_recentes_variacao, inflacao_variacao_grupos, ipca_peso_mensal, ipca_variacao)
setwd("C://Users//Lucasma//OneDrive - Inter-American Development Bank Group//Desktop//APRESENTACAO IAMC")
write.csv(`chart_20`, file = "chart_20_20.csv")



# ------------------------------------------------------------------------------------------------------------------------------------


# Chart 21 - Expectations

# Expectativas para o crescimento do PIB (referentes a 2023 e 2024):

chart_21 <- rbcb::get_annual_market_expectations(c('PIB Total', 'IPCA', 'Taxa de câmbio', 'Selic','IGP-M'),
                                                start_date = '2020-01-01')
tail(chart_21)

# CUIDADO COM A MUDANÇA DE NOME NA VARIAVAL --> reference_date ou reference_year

# chart_21a <- chart_21 %>%
#   filter(indic == "Meta para taxa over-selic", reference_date %in% c(2021, 2022),
#          base == 0, indic_detail == "Fim do ano") %>% 
#   select(date, reference_date, median) %>% 
#   pivot_wider(names_from = reference_date, values_from = median, names_prefix = "SELIC_expectations_") %>% 
#   arrange(date) %>% 
#   mutate(SELIC_expectations_2021 = SELIC_expectations_2021/100, SELIC_expectations_2022 = SELIC_expectations_2022/100)
chart_21 <- chart_21 %>%
  filter(indic == "Selic", reference_date %in% c(2023, 2024),
         base == 0) %>% 
  select(date, reference_date, median) %>% 
  pivot_wider(names_from = reference_date, values_from = median, names_prefix = "SELIC_expectations_") %>% 
  arrange(date) %>% 
  mutate(SELIC_expectations_2023 = SELIC_expectations_2023/100, SELIC_expectations_2024 = SELIC_expectations_2024/100) %>% 
  mutate(SELIC_expectations_2023 = label_percent(accuracy = 0.01)(SELIC_expectations_2023)) %>% 
  mutate(SELIC_expectations_2024 = label_percent(accuracy = 0.01)(SELIC_expectations_2024))


write.csv(chart_21, file = "chart_21_21.csv")

# ggplot(chart_21 %>% filter(date >= "2020-03-01"), aes(x = date)) +
#   geom_line(aes(y = SELIC_expectations_2021), colour = "blue")


# esse aqui ta ok atualiza sozinho
chart_21_selic <- rbcb::get_series(code = 432, start_date = "2020-01-01") %>%
  transmute(date, SELIC = label_percent(accuracy = 0.01)(`432`/100))

write.csv(chart_21_selic, file = "chart_21_selic.csv")
# write.csv2(chart_21_selic, file = "chart_21_selic.csv")


# --------------------------------------------------------------------------------------------------------------------------------------------


# Chart 23 - Crédito

# SE ESTIVER DANDO ERRRO, SEPARAR A EXTRAÇÃO DO COMANDO MUTATE

# SGS BACEN 13522 - Índice nacional de preços ao consumidor - amplo (IPCA) - em 12 meses
`13522_ipca_12m` <- rbcb::get_series(c(`13522_ipca_12m` = 13522), start_date = "2018-01-01")

# SGS BACEN 20539 - Saldo da carteira de crédito - Total
`20539_credito_total` <- rbcb::get_series(c(`20539_credito_totall` = 20539), start_date = "2018-01-01")

# SGS BACEN 20542 - Saldo da carteira de crédito com recursos livres - Total
`20542_credito_recursos_livresl` <- rbcb::get_series(c(`20542_credito_recursos_livresl` = 20542), start_date = "2018-01-01")

# SGS BACEN 20593 - Saldo da carteira de crédito com recursos direcionados - Total
`20593_credito_recursos_direcionados` <- rbcb::get_series(c(`20593_credito_recursos_direcionados` = 20593), start_date = "2018-01-01")

chart_23 <- full_join(`13522_ipca_12m`, `20539_credito_total`, ) %>%
  full_join(`20542_credito_recursos_livresl`) %>% full_join(`20593_credito_recursos_direcionados`) %>% 
  mutate(date = format(date, format = "%b - %Y"))

rm(`13522_ipca_12m`, `20539_credito_total`, `20542_credito_recursos_livresl`, `20593_credito_recursos_direcionados`)

write.csv(`chart_23`, file = "chart_23_23.csv")
# write.csv2(`chart_23`, file = "chart_23.csv")



# --------------------------------------------------------------------------------------------------------------------------------------------


# Chart 23 B - Crédito

# SE ESTIVER DANDO ERRRO, SEPARAR A EXTRAÇÃO DO COMANDO MUTATE

# SGS BACEN 2007 - Saldos das operações de crédito das instituições financeiras sob controle público - Total
`2007_credito_publico` <- rbcb::get_series(c(`2007_credito_publico` = 2007), start_date = "2018-01-01")

# SGS BACEN 2043 - Saldos das operações de crédito das instituições financeiras sob controle privado - Total
`2043_credito_privado` <- rbcb::get_series(c(`2043_credito_privadol` = 2043), start_date = "2018-01-01")


chart_23_b <- full_join(`2007_credito_publico`, `2043_credito_privado`) %>% 
  mutate(date = format(date, format = "%b-%Y"))

rm(`2007_credito_publico`, `2043_credito_privado`)

write.csv(`chart_23_b`, file = "chart_23_b.csv")
# write.csv2(`chart_23_b`, file = "chart_23_b.csv")
# --------------------------------------------------------------------------------------------------------------------------------------------
# Chart 24 - Crédito
# SE ESTIVER DANDO ERRRO, SEPARAR A EXTRAÇÃO DO COMANDO MUTATE
# SGS BACEN 27701 - Saldo das operações de crédito por porte da empresa - Micro, Pequena e Média (MPMe)
`27701_credito_pequena_empresa` <- rbcb::get_series(c(`27701_credito_pequena_empresa` = 27701), start_date = "2019-01-01")
# SGS BACEN 27702 - Saldo das operações de crédito por porte da empresa - Grande
`27702_credito_grande_empresa` <- rbcb::get_series(c(`27702_credito_grande_empresa` = 27702), start_date = "2019-01-01")
chart_24 <- full_join(`27701_credito_pequena_empresa`, `27702_credito_grande_empresa`) %>% 
  mutate(date = format(date, format = "%b - %Y"))
rm(`27701_credito_pequena_empresa`, `27702_credito_grande_empresa`)
write.csv(`chart_24`, file = "chart_24.csv")
# --------------------------------------------------------------------------------------------------------------------------------------------
# Chart 25 - Loans/Empréstimos

# SE ESTIVER DANDO ERRRO, SEPARAR A EXTRAÇÃO DO COMANDO MUTATE

# SGS BACEN 13667 - Inadimplência da carteira de crédito das instituições financeiras sob controle público - Total
`13667_non_performing_loan_public` <- rbcb::get_series(c(`13667_non_performing_loan_public` = 13667), start_date = "2017-01-01")

# SGS BACEN 13673 - Inadimplência da carteira de crédito das instituições financeiras sob controle privado nacional - Total
`13673_non_performing_loan_private` <- rbcb::get_series(c(`13673_non_performing_loan_private` = 13673), start_date = "2017-01-01")

# SGS BACEN 13679 - Inadimplência da carteira de crédito das instituições financeiras sob controle estrangeiro - Total
`13679_non_performing_loan_foreign` <- rbcb::get_series(c(`13679_non_performing_loan_foreign` = 13679), start_date = "2017-01-01")

# SGS BACEN 13685 - Inadimplência da carteira de crédito das instituições financeiras sob controle privado - Total
`13685` <- rbcb::get_series(c(`13685` = 13685), start_date = "2017-01-01")

# SGS BACEN 21085 - Inadimplência da carteira de crédito com recursos livres - Total
`21085` <- rbcb::get_series(c(`21085` = 21085), start_date = "2017-01-01")

# SGS BACEN 21086 - Inadimplência da carteira de crédito com recursos livres - Pessoas jurídicas - Total
`21086` <- rbcb::get_series(c(`21086` = 21086), start_date = "2017-01-01")

# SGS BACEN 21112 - Inadimplência da carteira de crédito com recursos livres - Pessoas físicas - Total
`21112` <- rbcb::get_series(c(`21112` = 21112), start_date = "2017-01-01")


library(dplyr)
chart_25 <- full_join(`13667_non_performing_loan_public`, `13673_non_performing_loan_private`) %>%
  full_join(`13679_non_performing_loan_foreign`) %>%
  full_join(`13685`) %>% 
  full_join(`21085`) %>% 
  full_join(`21086`) %>% 
  full_join(`21112`) %>% 
  mutate(date = format(date, format = "%b - %Y"))

  

rm(`13667_non_performing_loan_public`, `13673_non_performing_loan_private`, `13679_non_performing_loan_foreign`,
   `13685`, `21085`, `21086`, `21112`)

write.csv(`chart_25`, file = "chart_25_25.csv")
# write.csv2(`chart_25`, file = "chart_25.csv")



# -----------------------------------------------------------------------------

 # Dívida Externa com Organismos Multilaterais como proporção da dívida Externa do Setor Público.
 
divida_mult <- rbcb::get_series(c(`Dívida Externa com Organismos Multilaterais` = 21530), start_date = "2000-01-01")
divida_externa_publica <- rbcb::get_series(c(`Dívida Externa - Pública` = 21523), start_date = "2007-07-01")
divida_externa <- full_join(divida_mult, divida_externa_publica) %>%
  mutate(multilateral_prop = `Dívida Externa com Organismos Multilaterais`/`Dívida Externa - Pública`)
write.csv(divida_externa, file = "divida externa.csv")
