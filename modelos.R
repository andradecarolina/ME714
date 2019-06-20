load("dados.Rda")
library(gamlss)

# modelo binomial negativa ------------------------------------------------

#zero inflated
dados_zinb <- gamlss(qtde ~ sexo*comida*estacao, data = dados, family = "ZINBI")
dados_c_zinb <- gamlss(qtde ~ sexo*comida*estacao, data = dados_c, family = "ZINBI")
dados_col_zinb <- gamlss(qtde ~ sexo*comida, data = dados_col, family = "ZINBI")
dados_c_col_zinb <- gamlss(qtde ~ sexo*comida, data = dados_c_col, family = "ZINBI")

#normal
dados_nb <- gamlss(qtde ~ sexo*comida*estacao, data = dados, family = "NBI")
dados_c_nb <- gamlss(qtde ~ sexo*comida*estacao, data = dados_c, family = "NBI")
dados_col_nb <- gamlss(qtde ~ sexo*comida, data = dados_col, family = "NBI")
dados_c_col_nb <- gamlss(qtde ~ sexo*comida, data = dados_c_col, family = "NBI")

# modelos poisson ---------------------------------------------------------

#zero inflated
dados_zip <- gamlss(qtde ~ sexo*comida*estacao, data = dados, family = "ZIP")
dados_c_zip <- gamlss(qtde ~ sexo*comida*estacao, data = dados_c, family = "ZIP")
dados_col_zip <- gamlss(qtde ~ sexo*comida, data = dados_col, family = "ZIP")
dados_c_col_zip <- gamlss(qtde ~ sexo*comida, data = dados_c_col, family = "ZIP")

#normal
dados_p <- gamlss(qtde ~ sexo*comida*estacao, data = dados, family = "PO")
dados_c_p <- gamlss(qtde ~ sexo*comida*estacao, data = dados_c, family = "PO")
dados_col_p <- gamlss(qtde ~ sexo*comida, data = dados_col, family = "PO")
dados_c_col_p <- gamlss(qtde ~ sexo*comida, data = dados_c_col, family = "PO")

# salvar modelos ----------------------------------------------------------
save(dados_nb, dados_c_nb, dados_c_col_nb, dados_col_nb, file='modelos_nb.Rda')
save(dados_zinb, dados_c_zinb, dados_c_col_zinb, dados_col_zinb, file='modelos_zinb.Rda')
save(dados_p, dados_c_p, dados_c_col_p, dados_col_p, file='modelos_p.Rda')
save(dados_zip, dados_c_zip, dados_c_col_zip, dados_col_zip, file='modelos_zip.Rda')

# analisando modelos ------------------------------------------------------
# plots modelos zinb
plot(dados_c_col_zinb, summary=TRUE)
plot(dados_c_zinb, summary=TRUE)
plot(dados_col_zinb, summary=TRUE)
plot(dados_zinb, summary=TRUE)

# plots modelos nb
plot(dados_c_col_nb, summary=TRUE)
plot(dados_c_nb, summary=TRUE)
plot(dados_col_nb, summary=TRUE)
plot(dados_nb, summary=TRUE)

# analisando modelos poisson ----------------------------------------------
plot(dados_c_col_zip, summary=TRUE)
plot(dados_c_zip, summary=TRUE)
plot(dados_col_zip, summary=TRUE)
plot(dados_zip, summary=TRUE)

plot(dados_c_col_p, summary=TRUE)
plot(dados_c_p, summary=TRUE)
plot(dados_col_p, summary=TRUE)
plot(dados_p, summary=TRUE)


# exportando coeficientes -------------------------------------------------

broom::tidy(dados_col_zinb) %>% xtable::xtable()


# modelo poisson (pacote glm) ---------------------------------------------
# Modelo de poisson multivariado

ajuste <- glm(qtde ~ sexo*comida, data = dados_col, poisson(link = "log"))
ajuste

anova(ajuste)

qqplot(ajuste$residuals, dados_col$qtde, title("Gráfico de Resíduos"))

plot(ajuste$residuals, title("Ajuste dos Resíduos"))

#Shapiro

shapiro.test(dados_col$qtde)

shapiro.test(ajuste$residuals)

plot(fitted(ajuste), residuals(ajuste))