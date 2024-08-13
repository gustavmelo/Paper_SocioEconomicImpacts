
# Instalar e carregar os pacotes necessários

{
  if (!require("MatchIt")) install.packages("MatchIt", dependencies=TRUE)
  if (!require("here")) install.packages("here", dependencies=TRUE)
  if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
  if (!require("openxlsx")) install.packages("openxlsx", dependencies=TRUE)
  if (!require("lubridate")) install.packages("lubridate", dependencies=TRUE)
  if (!require("tibble")) install.packages("tibble", dependencies=TRUE)
  if (!require("tableone")) install.packages("tableone", dependencies=TRUE)
  if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
  if (!require("cobalt")) install.packages("cobalt", dependencies=TRUE)
  if (!require("tidyr")) install.packages("tidyr", dependencies=TRUE)
  if (!require("fixest")) install.packages("fixest", dependencies=TRUE)
  if (!require("Rcpp")) install.packages("Rcpp", dependencies=TRUE)
  if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
  if (!require("tseries")) install.packages("tseries", dependencies=TRUE)
  if (!require("lmtest")) install.packages("lmtest", dependencies=TRUE)
  if (!require("sandwich")) install.packages("sandwich", dependencies=TRUE)
  if (!require("purrr")) install.packages("purrr", dependencies=TRUE)
  if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
}

# Ler a base de dados
file_path <- here() # Base deve estar no mesmo diretorio do script
df <- read_excel('base_final.xlsx')

# Definir as colunas que não devem ser consideradas na contabilização de dados faltantes (colunas das usinas)
ignore_columns <- 15
total_columns <- ncol(df)
columns_to_check <- total_columns - ignore_columns

initial_rows <- nrow(df)

# Eliminar linhas com dados faltantes (exceto nas últimas 15 colunas)
df_cleaned <- df %>%
  filter(rowSums(is.na(select(., 1:columns_to_check))) == 0)

final_rows <- nrow(df_cleaned)

rows_removed <- initial_rows - final_rows

# Selecionar as colunas relativas às usinas
usinas_columns <- c("Eolica", "FV", "PCH", "CGH", "UHE")

# Contando quantos municípios possuem ao menos uma usina
df_cleaned %>%
  filter(Eolica == 1 | FV == 1 | PCH == 1 | CGH == 1 | UHE == 1)

# Colunas de data de inicio da operacao das usinas
date_columns <- c("INIC_OPER_EOL", "INIC_OPER_FV", "INIC_OPER_PCH", "INIC_OPER_CGH", "INIC_OPER_UHE")

# Funcao para converter as datas utilizando parse_date_time
convert_dates <- function(column) {
  parse_date_time(df_cleaned[[column]], orders = c("dmy", "dmY", "d/m/Y", "d-m-Y", "d.m.Y","Y-m-d"))
}

df_cleaned[date_columns] <- lapply(date_columns, convert_dates)

# Definir a data de corte (eliminar quem tiver usina antes)
cutoff_date <- as.Date("2011-01-01")

df_filtered <- df_cleaned[apply(df_cleaned[date_columns], 1, function(x) all(is.na(x) | x >= cutoff_date)), ]

#Definindo os grupos controle e tratamento

# Definir a data de corte (municipios apenas com usinas apos essa data tambem podem fazer parte do grupo controle)
cutoff_date <- as.Date("2020-12-31")

# Função para verificar se todas as datas são vazias ou todas são após a data de corte
is_control <- function(..., date_columns, cutoff_date) {
  dates <- c(...)
  dates <- dates[!is.na(dates)]
  all(is.na(dates)) || all(dates > cutoff_date)
}

# Aplicar a função para criar o data frame "control"
control <- df_filtered %>%
  filter(pmap_lgl(select(., all_of(date_columns)), is_control, date_columns = date_columns, cutoff_date = cutoff_date))

# Verificar o menor valor de todas as colunas de data no data frame "control"
min_date_control <- apply(control[date_columns], 1, function(row) min(row, na.rm = TRUE))

# Verificar o menor valor global
min(min_date_control, na.rm = TRUE) #Deve ser maior que 2020-12-31

# Criar o data frame "treatment" com o restante das observações
treatment <- df_filtered %>%
  anti_join(control, by = colnames(df_filtered))

# Adicionar a coluna treat ao data frame treatment
treatment <- treatment %>%
  mutate(treat = 1)

# Adicionar a coluna treat ao data frame control
control <- control %>%
  mutate(treat = 0)

# Tipos de usina do tratamento

# Verificando o maximo de fontes em cada municipio
teste <- treatment %>%
  mutate(N_Sources = rowSums(select(., all_of(usinas_columns)), na.rm = TRUE))
max(teste$N_Sources, na.rm = TRUE)

df_eolica_only <- treatment %>%
  filter((Eolica == 1 & INIC_OPER_EOL <= cutoff_date) & 
           (FV == 0 | INIC_OPER_FV > cutoff_date) & 
           (PCH == 0 | INIC_OPER_PCH > cutoff_date) & 
           (CGH == 0 | INIC_OPER_CGH > cutoff_date) & 
           (UHE == 0 | INIC_OPER_UHE > cutoff_date))

treatment <- treatment %>%
  mutate(treat_eol = ifelse(
    (Eolica == 1 & INIC_OPER_EOL <= cutoff_date) &
      (FV == 0 | INIC_OPER_FV > cutoff_date) &
      (PCH == 0 | INIC_OPER_PCH > cutoff_date) &
      (CGH == 0 | INIC_OPER_CGH > cutoff_date) &
      (UHE == 0 | INIC_OPER_UHE > cutoff_date),
    1,
    0
  ))

df_fv_only <- treatment %>%
  filter((Eolica == 0 | INIC_OPER_EOL > cutoff_date) & 
           (FV == 1 & INIC_OPER_FV <= cutoff_date) & 
           (PCH == 0 | INIC_OPER_PCH > cutoff_date) & 
           (CGH == 0 | INIC_OPER_CGH > cutoff_date) & 
           (UHE == 0 | INIC_OPER_UHE > cutoff_date))

treatment <- treatment %>%
  mutate(treat_fv = ifelse(
    (Eolica == 0 | INIC_OPER_EOL > cutoff_date) &
      (FV == 1 & INIC_OPER_FV <= cutoff_date) &
      (PCH == 0 | INIC_OPER_PCH > cutoff_date) &
      (CGH == 0 | INIC_OPER_CGH > cutoff_date) &
      (UHE == 0 | INIC_OPER_UHE > cutoff_date),
    1,
    0
  ))

df_hyd_only <- treatment %>%
  filter((Eolica == 0 | INIC_OPER_EOL > cutoff_date) & 
           (FV == 0 | INIC_OPER_FV > cutoff_date) & 
           (PCH == 1 | CGH == 1 | UHE == 1))

treatment <- treatment %>%
  mutate(treat_hyd = ifelse(
    (Eolica == 0 | INIC_OPER_EOL > cutoff_date) &
      (FV == 0 | INIC_OPER_FV > cutoff_date) &
      (PCH == 1 | CGH == 1 | UHE == 1),
    1,
    0
  ))

df_eol_fv <- treatment %>%
  filter((Eolica == 1 & INIC_OPER_EOL <= cutoff_date) & 
           (FV == 1 & INIC_OPER_FV <= cutoff_date) & 
           (PCH == 0 | INIC_OPER_PCH > cutoff_date) & 
           (CGH == 0 | INIC_OPER_CGH > cutoff_date) & 
           (UHE == 0 | INIC_OPER_UHE > cutoff_date))

treatment <- treatment %>%
  mutate(treat_eol_fv = ifelse(
    (Eolica == 1 & INIC_OPER_EOL <= cutoff_date) &
      (FV == 1 & INIC_OPER_FV <= cutoff_date) &
      (PCH == 0 | INIC_OPER_PCH > cutoff_date) &
      (CGH == 0 | INIC_OPER_CGH > cutoff_date) &
      (UHE == 0 | INIC_OPER_UHE > cutoff_date),
    1,
    0
  ))


# Criar uma lista de data frames para salvar no arquivo Excel
list_of_dfs <- list("Eolica_Only" = df_eolica_only, 
                    "FV_Only" = df_fv_only, 
                    "Hyd_Only" = df_hyd_only, 
                    "Eol_FV" = df_eol_fv)

# Salvar os data frames no arquivo Excel
#write.xlsx(list_of_dfs, "treatment.xlsx")

control <- control %>%
  mutate(
    treat_eol = 0,
    treat_fv = 0,
    treat_hyd = 0,
    treat_eol_fv = 0
  )

### Analises ###

# Periodo pre-tratamento (2010)

# Variáveis e suas unidades

variables <- c("Latitude", "Longitude", "Altitude", "Area_km2", "Populacao_2010", "Populacao_rural_2010_perc", 
               "Taxa_de_analfabetismo_2010", "PIB_ind_2010_perc", "Prod_ind_2010_milhoes", "Bolsa_familia_2010_milhoes", "PIB_per_capta_2010", "Pessoal_ocupado_2010")

units <- c("Graus decimais", "Graus decimais", "m", "km²", "Number", "%", "%", "%","Reais","Reais","Reais","Number")

treatment$PIB_per_capta_2010<-as.numeric(treatment$PIB_per_capta_2010)
control$PIB_per_capta_2010<-as.numeric(control$PIB_per_capta_2010)
treatment$Pessoal_ocupado_2010<-as.numeric(treatment$Pessoal_ocupado_2010)
control$Pessoal_ocupado_2010<-as.numeric(control$Pessoal_ocupado_2010)

# Função para realizar o teste t, teste KS e coletar estatísticas
analyze_variable <- function(var, control, treatment) {
  control_data <- control[[var]]
  treatment_data <- treatment[[var]]
  
  control_mean <- mean(control_data, na.rm = TRUE)
  control_sd <- sd(control_data, na.rm = TRUE)
  
  treatment_mean <- mean(treatment_data, na.rm = TRUE)
  treatment_sd <- sd(treatment_data, na.rm = TRUE)
  
  t_test <- t.test(control_data, treatment_data, var.equal = FALSE)
  t_p_value <- t_test$p.value
  
  ks_test <- ks.test(control_data, treatment_data)
  ks_p_value <- ks_test$p.value
  
  return(c(control_mean, control_sd, treatment_mean, treatment_sd, t_p_value, ks_p_value))
}

# Criar um dataframe para armazenar os resultados
results <- data.frame(Variable = variables, Unit = units, 
                      Control_Mean = numeric(length(variables)), Control_SD = numeric(length(variables)),
                      Treatment_Mean = numeric(length(variables)), Treatment_SD = numeric(length(variables)), 
                      T_P_Value = numeric(length(variables)), KS_P_Value = numeric(length(variables)), 
                      stringsAsFactors = FALSE)

# Aplicar a função para cada variável
for (i in 1:length(variables)) {
  results[i, 3:8] <- analyze_variable(variables[i], control, treatment)
}

print(results)
