
########## PSM ################################################################################################

# Combinar os data frames de controle e tratamento em um só
df_combined <- rbind(treatment, control)

# Realizar o matching usando a regressão logística e nearest neighbor

psm_model <- matchit(treat ~ Latitude + Longitude + Altitude + Area_km2 + Populacao_2010 + Populacao_rural_2010_perc +
                       Taxa_de_analfabetismo_2010 + PIB_ind_2010_perc + Bolsa_familia_2010_milhoes, 
                     data = df_combined, method = "nearest", distance = "logit")

# Obter os dados pareados
matched_data <- match.data(psm_model)

balance_data <- bal.tab(psm_model, un = TRUE)
balance_data$Balance

# Plotar o bias antes e depois do matching

bias <- data.frame(
  Variable = c("Propensity score", "Latitude", "Longitude", "Altitude", "Area", "Population", "Rural pop rate", "Illiteracy rate", "Industrial rate", "Bolsa spending"),
  Unmatched = balance_data$Balance$Diff.Un,
  Matched = balance_data$Balance$Diff.Adj
) 

bias <- bias %>% arrange(desc(Unmatched))

bias_long <- pivot_longer(bias, cols = c(Unmatched, Matched), names_to = "Type", values_to = "Value")

bias_long <- bias_long %>%
  mutate(Variable = factor(Variable, levels = rev(unique(bias$Variable))))

bias_long$Type <- factor(bias_long$Type, levels = c("Unmatched", "Matched"))

# Criar o gráfico
plot_match <- ggplot(bias_long, aes(x = Value, y = Variable, color = Type, shape = Type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Unmatched" = "red3", "Matched" = "navy")) +
  scale_shape_manual(values = c("Unmatched" = 16, "Matched" = 16)) + # 16 = bolinha, 8 = asterisco
  xlab("Standardized Percentage Bias") +
  ylab("") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, color = "black")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, by = 0.25)) +
  geom_hline(yintercept = 9.5, linetype = "dashed") +
  theme(axis.ticks = element_line(color = "black"), axis.ticks.length = unit(0.10, "cm")) +
  coord_fixed(ratio = 0.13)

## Analise pos-matching 

# Covariates

variables <- c("Latitude", "Longitude", "Altitude", "Area_km2", "Populacao_2010", "Populacao_rural_2010_perc", 
               "Taxa_de_analfabetismo_2010", "PIB_ind_2010_perc", "Prod_ind_2010_milhoes", "Bolsa_familia_2010_milhoes")

units <- c("Graus decimais", "Graus decimais", "m", "km²", "Number", "%", "%", "%","Reais","Reais")

# Criar um dataframe para armazenar os resultados
results_matched_cov <- data.frame(Variable = variables, Unit = units, 
                      Control_Mean = numeric(length(variables)), Control_SD = numeric(length(variables)),
                      Treatment_Mean = numeric(length(variables)), Treatment_SD = numeric(length(variables)), 
                      T_P_Value = numeric(length(variables)), KS_P_Value = numeric(length(variables)), 
                      stringsAsFactors = FALSE)

control_matched  <- matched_data %>%
  filter(treat == 0)

treatment_matched  <- matched_data %>%
  filter(treat == 1)

# Aplicar a função para cada variável
for (i in 1:length(variables)) {
  results_matched_cov[i, 3:8] <- analyze_variable(variables[i], control_matched, treatment_matched)
}

print(results_matched_cov)

# Outcome variables

inflation<-0.67

matched_data$Pessoal_ocupado_2010<-as.numeric(matched_data$Pessoal_ocupado_2010)
matched_data$Pessoal_ocupado_2020<-as.numeric(matched_data$Pessoal_ocupado_2020)
matched_data$PIB_per_capta_2010<-as.numeric(matched_data$PIB_per_capta_2010)
matched_data$PIB_per_capta_2010<-matched_data$PIB_per_capta_2010*(1+inflation)
matched_data$Prod_ind_2010_milhoes<-matched_data$Prod_ind_2010_milhoes*(1+inflation)
matched_data$Bolsa_familia_2010_milhoes<-matched_data$Bolsa_familia_2010_milhoes*(1+inflation)

control_matched  <- matched_data %>%
  filter(treat == 0)

treatment_matched  <- matched_data %>%
  filter(treat == 1)

# Criar um dataframe para armazenar os resultados

variables <- c("PIB_per_capta_2010", "Pessoal_ocupado_2010", "PIB_per_capta_2020", "Pessoal_ocupado_2020")

units <- c("Reais","Number","Reais","Number")

results_matched_out <- data.frame(Variable = variables, Unit = units, 
                      Control_Mean = numeric(length(variables)), Control_SD = numeric(length(variables)),
                      Treatment_Mean = numeric(length(variables)), Treatment_SD = numeric(length(variables)), 
                      T_P_Value = numeric(length(variables)), KS_P_Value = numeric(length(variables)), 
                      stringsAsFactors = FALSE)

# Aplicar a função para cada variável
for (i in 1:length(variables)) {
  results_matched_out[i, 3:8] <- analyze_variable(variables[i], control_matched, treatment_matched)
}

print(results_matched_out)

########## DID ################################################################################################

selected_columns <- c("Populacao_2010", "Populacao_2022", "Prod_ind_2010_milhoes", 
                      "Bolsa_familia_2010_milhoes", "Prod_ind_2020_milhoes", 
                      "Bolsa_familia_2020_milhoes", "Pessoal_ocupado_2010", 
                      "Pessoal_ocupado_2020", "PIB_per_capta_2010", "PIB_per_capta_2020", 
                      "Eolica","FV","PCH","CGH","UHE","treat",
                      "treat_eol","treat_fv","treat_hyd","treat_eol_fv","subclass")

matched_aux <- matched_data[, selected_columns]

#### PIB per capta #####################################

selected_columns_final <- c("treat", "treat_eol","treat_fv","treat_hyd","treat_eol_fv","time", 
                            "PIB per capta","Prod ind", "Bolsa familia")  

### RES em geral #################################

RES<-matched_aux

RES <- RES %>%
  rename(
    `PIB per capta 2010` = PIB_per_capta_2010,
    `PIB per capta 2020` = PIB_per_capta_2020,
    `Prod ind 2010` = Prod_ind_2010_milhoes,
    `Prod ind 2020` = Prod_ind_2020_milhoes,
    `Bolsa familia 2010` = Bolsa_familia_2010_milhoes,
    `Bolsa familia 2020` = Bolsa_familia_2020_milhoes
  )

RES_long <- RES %>%
  pivot_longer(
    cols = starts_with("PIB") | starts_with("Bolsa") | starts_with("Prod"),
    names_to = c(".value", "year"),
    names_pattern = "(.*) (\\d{4})"
    )

# Criar a variável de tempo
RES_long$time <- ifelse(RES_long$year == "2010", 0, 1)
RES_long <- RES_long %>%
  mutate(time = as.numeric(time))

# Criar um novo data frame com as colunas selecionadas
RES_final <- RES_long[, selected_columns_final]

RES_final <- RES_final %>%
  rename(
    PIB_per_capta = `PIB per capta`,
    Prod_ind = `Prod ind`,
    Bolsa_familia = `Bolsa familia`,
  )

RES_final <- RES_final %>%
  mutate(id = rep(1:(n() / 2), each = 2))

# Estimar o modelo DiD 
model_RES <- feols(PIB_per_capta ~ treat * time + Bolsa_familia + Prod_ind | id, data = RES_final)
summary(model_RES)

# Erro padrão robusto
summary(model_RES, se = "hetero")

#write.xlsx(as.data.frame(coeftable(model_RES)), "PIB_RES.xlsx")

### Eolica ###################################################################

eolica <- matched_aux[, selected_columns]
eolica <- subset(eolica, !(Eolica == 0 & treat == 1))

subclasses_treat_1 <- eolica %>%
  filter(treat_eol == 1) %>%
  select(subclass) %>%
  distinct()

eolica_filtered <- eolica %>%
  filter(treat_eol == 1 | (treat == 0 & subclass %in% subclasses_treat_1$subclass))

eolica_filtered <- eolica_filtered %>%
  rename(
    `PIB per capta 2010` = PIB_per_capta_2010,
    `PIB per capta 2020` = PIB_per_capta_2020,
    `Prod ind 2010` = Prod_ind_2010_milhoes,
    `Prod ind 2020` = Prod_ind_2020_milhoes,
    `Bolsa familia 2010` = Bolsa_familia_2010_milhoes,
    `Bolsa familia 2020` = Bolsa_familia_2020_milhoes
  )

eolica_long <- eolica_filtered %>%
  pivot_longer(
    cols = starts_with("PIB") | starts_with("Bolsa") | starts_with("Prod"),
    names_to = c(".value", "year"),
    names_pattern = "(.*) (\\d{4})"
  )

# Criar a variável de tempo
eolica_long$time <- ifelse(eolica_long$year == "2010", 0, 1)
eolica_long <- eolica_long %>%
  mutate(time = as.numeric(time))

# Criar um novo data frame com as colunas selecionadas
eolica <- eolica_long[, selected_columns_final]

eolica <- eolica %>%
  rename(
    PIB_per_capta = `PIB per capta`,
    Prod_ind = `Prod ind`,
    Bolsa_familia = `Bolsa familia`,
  )

eolica <- eolica %>%
  mutate(id = rep(1:(n() / 2), each = 2))

# Estimar o modelo DiD 
model_eol <- feols(PIB_per_capta ~ treat_eol * time + Bolsa_familia + Prod_ind | id, data = eolica)
summary(model_eol)

# Erro padrão robusto
summary(model_eol, se = "hetero")

#write.xlsx(as.data.frame(coeftable(model_eol)), "PIB_EOL.xlsx"

### FV ###################################################################

fv <- matched_aux[, selected_columns]
fv <- subset(fv, !(FV == 0 & treat == 1))

subclasses_treat_1 <- fv %>%
  filter(treat_fv == 1) %>%
  select(subclass) %>%
  distinct()

fv_filtered <- fv %>%
  filter(treat_fv == 1 | (treat == 0 & subclass %in% subclasses_treat_1$subclass))

fv_filtered <- fv_filtered %>%
  rename(
    `PIB per capta 2010` = PIB_per_capta_2010,
    `PIB per capta 2020` = PIB_per_capta_2020,
    `Prod ind 2010` = Prod_ind_2010_milhoes,
    `Prod ind 2020` = Prod_ind_2020_milhoes,
    `Bolsa familia 2010` = Bolsa_familia_2010_milhoes,
    `Bolsa familia 2020` = Bolsa_familia_2020_milhoes
  )

fv_long <- fv_filtered %>%
  pivot_longer(
    cols = starts_with("PIB") | starts_with("Bolsa") | starts_with("Prod"),
    names_to = c(".value", "year"),
    names_pattern = "(.*) (\\d{4})"
  )

# Criar a variável de tempo
fv_long$time <- ifelse(fv_long$year == "2010", 0, 1)
fv_long <- fv_long %>%
  mutate(time = as.numeric(time))

# Criar um novo data frame com as colunas selecionadas
fv_final <- fv_long[, selected_columns_final]

fv_final <- fv_final %>%
  rename(
    PIB_per_capta = `PIB per capta`,
    Prod_ind = `Prod ind`,
    Bolsa_familia = `Bolsa familia`,
  )

fv_final <- fv_final %>%
  mutate(id = rep(1:(n() / 2), each = 2))

# Estimar o modelo DiD 
model_fv <- feols(PIB_per_capta ~ treat_fv * time + Bolsa_familia + Prod_ind | id, data = fv_final)
summary(model_fv)

# Erro padrão robusto
summary(model_fv, se = "hetero")

#write.xlsx(as.data.frame(coeftable(model_fv)), "PIB_FV.xlsx")

### Hydro ###################################################################

hydro <- matched_aux[, selected_columns]
hydro <- subset(hydro, !(PCH == 0 & CGH == 0 & UHE == 0 & treat == 1))

subclasses_treat_1 <- hydro %>%
  filter(treat_hyd == 1) %>%
  select(subclass) %>%
  distinct()

hydro_filtered <- hydro %>%
  filter(treat_hyd == 1 | (treat == 0 & subclass %in% subclasses_treat_1$subclass))

hydro_filtered <- hydro_filtered %>%
  rename(
    `PIB per capta 2010` = PIB_per_capta_2010,
    `PIB per capta 2020` = PIB_per_capta_2020,
    `Prod ind 2010` = Prod_ind_2010_milhoes,
    `Prod ind 2020` = Prod_ind_2020_milhoes,
    `Bolsa familia 2010` = Bolsa_familia_2010_milhoes,
    `Bolsa familia 2020` = Bolsa_familia_2020_milhoes
  )

hydro_long <- hydro_filtered %>%
  pivot_longer(
    cols = starts_with("PIB") | starts_with("Bolsa") | starts_with("Prod"),
    names_to = c(".value", "year"),
    names_pattern = "(.*) (\\d{4})"
  )

# Criar a variável de tempo
hydro_long$time <- ifelse(hydro_long$year == "2010", 0, 1)
hydro_long <- hydro_long %>%
  mutate(time = as.numeric(time))

# Criar um novo data frame com as colunas selecionadas
hydro_final <- hydro_long[, selected_columns_final]

hydro_final <- hydro_final %>%
  rename(
    PIB_per_capta = `PIB per capta`,
    Prod_ind = `Prod ind`,
    Bolsa_familia = `Bolsa familia`,
  )

hydro_final <- hydro_final %>%
  mutate(id = rep(1:(n() / 2), each = 2))

# Estimar o modelo DiD 
model_hydro <- feols(PIB_per_capta ~ treat_hyd * time + Bolsa_familia + Prod_ind | id, data = hydro_final)
summary(model_hydro)

# Erro padrão robusto
summary(model_hydro, se = "hetero")

#write.xlsx(as.data.frame(coeftable(model_hydro)), "PIB_HYD.xlsx")


#### Pessoal ocupado ####################################

selected_columns_final <- c("treat","treat_eol","treat_fv","treat_hyd","treat_eol_fv", "time", 
                            "Pessoal ocupado","Prod ind", "Bolsa familia") 

### RES em geral #############################

RES <- matched_aux[, selected_columns]

RES <- RES %>%
  rename(
    `Pessoal ocupado 2010` = Pessoal_ocupado_2010,
    `Pessoal ocupado 2020` = Pessoal_ocupado_2020,
    `Prod ind 2010` = Prod_ind_2010_milhoes,
    `Prod ind 2020` = Prod_ind_2020_milhoes,
    `Bolsa familia 2010` = Bolsa_familia_2010_milhoes,
    `Bolsa familia 2020` = Bolsa_familia_2020_milhoes
  )

RES_long <- RES %>%
  pivot_longer(
    cols = starts_with("Pessoal") | starts_with("Bolsa") | starts_with("Prod") ,
    names_to = c(".value", "year"),
    names_pattern = "(.*) (\\d{4})"
  )

# Criar a variável de tempo
RES_long$time <- ifelse(RES_long$year == "2010", 0, 1)
RES_long <- RES_long %>%
  mutate(time = as.numeric(time))

# Criar um novo data frame com as colunas selecionadas
RES_final <- RES_long[, selected_columns_final]

RES_final <- RES_final %>%
  rename(
    Pessoal_ocupado = `Pessoal ocupado`,
    Prod_ind = `Prod ind`,
    Bolsa_familia = `Bolsa familia`
  )

RES_final <- RES_final %>%
  mutate(id = rep(1:(n() / 2), each = 2))

# Estimar o modelo DiD 
model_RES_2 <- feols(Pessoal_ocupado ~ treat * time + Bolsa_familia | id, data = RES_final)
summary(model_RES_2)

# Erro padrão robusto
summary(model_RES_2, se = "hetero")

#write.xlsx(as.data.frame(coeftable(model_RES_2)), "POCUP_RES.xlsx")

### Eolica ###################################################################

eolica <- matched_aux[, selected_columns]
eolica <- subset(eolica, !(Eolica == 0 & treat == 1))

subclasses_treat_1 <- eolica %>%
  filter(treat_eol == 1) %>%
  select(subclass) %>%
  distinct()

eolica_filtered <- eolica %>%
  filter(treat_eol == 1 | (treat_eol == 0 & subclass %in% subclasses_treat_1$subclass))

eolica_filtered <- eolica_filtered %>%
  rename(
    `Pessoal ocupado 2010` = Pessoal_ocupado_2010,
    `Pessoal ocupado 2020` = Pessoal_ocupado_2020,
    `Prod ind 2010` = Prod_ind_2010_milhoes,
    `Prod ind 2020` = Prod_ind_2020_milhoes,
    `Bolsa familia 2010` = Bolsa_familia_2010_milhoes,
    `Bolsa familia 2020` = Bolsa_familia_2020_milhoes
  )

eolica_long <- eolica_filtered %>%
  pivot_longer(
    cols = starts_with("Pessoal") | starts_with("Bolsa") | starts_with("Prod") ,
    names_to = c(".value", "year"),
    names_pattern = "(.*) (\\d{4})"
  )

# Criar a variável de tempo
eolica_long$time <- ifelse(eolica_long$year == "2010", 0, 1)
eolica_long <- eolica_long %>%
  mutate(time = as.numeric(time))

# Criar um novo data frame com as colunas selecionadas
eolica <- eolica_long[, selected_columns_final]

eolica <- eolica %>%
  rename(
    Pessoal_ocupado = `Pessoal ocupado`,
    Prod_ind = `Prod ind`,
    Bolsa_familia = `Bolsa familia`
  )

eolica <- eolica %>%
  mutate(id = rep(1:(n() / 2), each = 2))

# Estimar o modelo DiD 
model_eol_2 <- feols(Pessoal_ocupado ~ treat_eol * time + Bolsa_familia + Prod_ind | id, data = eolica)
summary(model_eol_2)

# Erro padrão robusto
summary(model_eol_2, se = "hetero")

#write.xlsx(as.data.frame(coeftable(model_eol_2)), "POCUP_EOL.xlsx")

### Hydro ###################################################################

hydro <- matched_aux[, selected_columns]
hydro <- subset(hydro, !(PCH == 0 & CGH == 0 & UHE == 0 & treat == 1))

subclasses_treat_1 <- hydro %>%
  filter(treat_hyd == 1) %>%
  select(subclass) %>%
  distinct()

hydro_filtered <- hydro %>%
  filter(treat_hyd == 1 | (treat_hyd == 0 & subclass %in% subclasses_treat_1$subclass))

hydro_filtered <- hydro_filtered %>%
  rename(
    `Pessoal ocupado 2010` = Pessoal_ocupado_2010,
    `Pessoal ocupado 2020` = Pessoal_ocupado_2020,
    `Prod ind 2010` = Prod_ind_2010_milhoes,
    `Prod ind 2020` = Prod_ind_2020_milhoes,
    `Bolsa familia 2010` = Bolsa_familia_2010_milhoes,
    `Bolsa familia 2020` = Bolsa_familia_2020_milhoes
  )

hydro_long <- hydro_filtered %>%
  pivot_longer(
    cols = starts_with("Pessoal") | starts_with("Bolsa") | starts_with("Prod") ,
    names_to = c(".value", "year"),
    names_pattern = "(.*) (\\d{4})"
  )

# Criar a variável de tempo
hydro_long$time <- ifelse(hydro_long$year == "2010", 0, 1)
hydro_long <- hydro_long %>%
  mutate(time = as.numeric(time))

# Criar um novo data frame com as colunas selecionadas
hydro_final <- hydro_long[, selected_columns_final]

hydro_final <- hydro_final %>%
  rename(
    Pessoal_ocupado = `Pessoal ocupado`,
    Prod_ind = `Prod ind`,
    Bolsa_familia = `Bolsa familia`
  )

hydro_final <- hydro_final %>%
  mutate(id = rep(1:(n() / 2), each = 2))

# Estimar o modelo DiD 
model_hydro_2 <- feols(Pessoal_ocupado ~ treat_hyd * time + Bolsa_familia | id, data = hydro_final)
summary(model_hydro_2)

# Erro padrão robusto
summary(model_hydro_2, se = "hetero")

#write.xlsx(as.data.frame(coeftable(model_hydro_2)), "POCUP_HYD.xlsx")

