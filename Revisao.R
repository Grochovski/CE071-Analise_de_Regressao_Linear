### Exemplo de motivação do capítulo 2 do livro 

require(faraway)
data("gala")
help("gala")

names("gala")
pairs(gala, pch = 20, cex = 2)

ajuste <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(ajuste)

X <- model.matrix(ajuste)
y <- gala$Species

beta_est <- solve(crossprod(X,X))%*%crossprod(X,y)
beta_est

sigma2_est <- sum(ajuste$residuals^2)/ajuste$df.residual
sigma2_est ### Estimativa da variância
sigma_est <- sqrt(sigma2_est)
sigma_est ### Desvio padrão estimado

mat_covars <- solve(crossprod(X,X))*sigma2_est
mat_covars ### Matriz de covariâncias estimadas para os beta chapéus.

erros_padroes <- sqrt(diag(mat_covars))
erros_padroes

summary(ajuste)

### IC para beta_Eelvation
beta_elev <- beta_est[3]
beta_elev_ep <- erros_padroes[3]
beta_elev + c(-1,1) * qt(0.975, df = ajuste$df.residual) * beta_elev_ep
confint(ajuste)

### Vamos testar se beta_Elevation=0.
est_teste <- beta_elev/beta_elev_ep
val_critico <- qt(0.025, df = ajuste$df.residual)

abs(est_teste) > abs(val_critico)
### Rejeita-se a hipótese nula.


### Teste F da significância do ajuste
sqres <- sum(ajuste$residuals^2)

ajuste_0 <- update(ajuste, ~1)
summary(ajuste_0)
sqres0 <- sum(ajuste_0$residuals^2)

F_Calc <- ((sqres0 - sqres)/(ajuste_0$df.residual - ajuste$df.residual))/(sqres/ajuste$df.residual)
F_Calc

F_crit <- qf(0.95, df1 = ajuste_0$df.residual - ajuste$df.residual, df2 = ajuste$df.residual)
F_crit

F_Calc > F_crit

pf(F_Calc, df1 = ajuste_0$df.residual - ajuste$df.residual, df2 = ajuste$df.residual, lower.tail = FALSE)

anova(ajuste_0, ajuste)


### Teste F para a hipótese H0: beta_Area = beta_Nearest = beta_Scruz = 0

ajuste_v2 <- update(ajuste, ~ Elevation + Adjacent)
summary(ajuste_v2)

sqres2 <- sum(ajuste_v2$residuals^2)

F_Calc <- ((sqres2 - sqres)/(ajuste_v2$df.residual - ajuste$df.residual))/(sqres/ajuste$df.residual)
F_Calc

F_crit <- qf(0.95, df1 = ajuste_v2$df.residual - ajuste$df.residual, df2 = ajuste$df.residual)
F_crit

F_Calc > F_crit

pf(F_Calc, df1 = ajuste_v2$df.residual - ajuste$df.residual, df2 = ajuste$df.residual, lower.tail = FALSE)

anova(ajuste_v2, ajuste)

### Predição - vamos considerar uma ilha com elevação e área da ilha adjacente médias

ilha_pred <- c(1, mean(gala$Elevation), mean(gala$Adjacent))
betas_chap <- coef(ajuste_v2)
crossprod(betas_chap, ilha_pred)

new_data <- data.frame(Elevation = mean(gala$Elevation), 
                       Adjacent = mean(gala$Adjacent))


predict(ajuste_v2, newdata = new_data, interval = 'confidence')
predict(ajuste_v2, newdata = new_data, interval = 'prediction')

### Estimação da média.


### Predição

