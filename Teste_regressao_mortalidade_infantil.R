require(leaps)
require(car)
require(faraway)

### Selecionando as covariaveis para analise.
dados <- read.csv2(file = "teste.csv",header = T)

ajuste <- lm(mortalidade ~ V11+V3+V16+V18+V23, data = dados) ### Ajuste com todas as covariaveis
summary(ajuste)
par(mfrow=c(2,2))
plot(ajuste)
par(mfrow=c(1,1))

ncvTest(ajuste)
shapiro.test(rstudent(ajuste))

round(cor(dados[,c('mortalidade','V3','V16','V18','V23','V11')]),digits = 2)
vif(ajuste)

boxCox(ajuste, lambda = seq(0.1,0.3,0.1))

dados$mortalidade_trans <- log(dados$mortalidade)
ajuste2 <- lm(mortalidade_trans ~ V11+V3+V16+V18+V23, data = dados)
summary(ajuste2)

ncvTest(ajuste2)
shapiro.test(rstudent(ajuste2))

