# Analizando algunos procesos en series de tiempo

rm(list=ls()) # Limpiamos el entorno
set.seed(9) # Por si quieres replicar mi codigo 

# y_{t} = e_{t} where e_{t} Normal(0,1)

n_data <- 350
noise <- rnorm(n = n_data, mean=0, sd=1)
n <- 1:n_data # tiempo de las observaciones t=1,2,3,...,n
white_noise <- data.frame(n,noise)
plot(white_noise$n, white_noise$noise, type = "l",main="Proceso de ruido blanco",xlab="Tiempo",ylab = "N(0,1)",col="darkred")


# y_{t} = 0.5 + 0.02*t + e_{t} where e_{t} Normal(0,1)

white_noise$determinic <- 0.5 + 0.02 * white_noise$n + white_noise$noise
plot(white_noise$n, white_noise$determinic , type = "l",main="Proceso de tendencia deterministica",xlab="Tiempo",col="darkblue")


#y_{t} = y_{t-1} + e_{t} where e_{t} Normal(0,1)

white_noise$stochastic[1] <- 0
for(i in c(2:n_data)){
  white_noise$stochastic[i] <- rnorm(n = 1, mean = 0, sd = 1) + white_noise$stochastic[i-1]
}
plot(white_noise$n, white_noise$stochastic , type = "l", main="Caminata aleatoria sin deriva",xlab="Tiempo",col="purple")

library(ggplot2) # mejorando visualizacion

# Resumiendo
ggplot(data = white_noise) +
  geom_line(aes(x = n, y = noise, color = "Ruido blanco")) +
  geom_line(aes(x = n, y = determinic, color = "Deterministico")) +
  geom_line(aes(x = n, y = stochastic, color = "Estocastico")) +
  ggtitle("Diferentes procesos de series de tiempo") +
  labs(x = "Tiempo", y = "Valor en el tiempo") +
  labs(colour = "Procesos de series") +
  theme_minimal() +
  theme_bw() +
  theme(panel.grid =element_blank())

library(pracma) # Quitar tendencia a una serie

##### Ruido blanco #####
par(mfrow=c(2,1))
plot(white_noise$n, white_noise$noise, type = "l",main="Proceso de ruido blanco",xlab="Tiempo",ylab = "N(0,1)",col="darkred")

white_noise$noise_d <- detrend(white_noise$noise)
plot(white_noise$n, white_noise$noise_d, type = "l",main="Proceso de ruido blanco sin tendencia",xlab="Tiempo",col="darkred")

##### Tendencia #####
par(mfrow=c(2,1))
plot(white_noise$n, white_noise$determinic , type = "l",main="Proceso de tendencia deterministica",xlab="Tiempo",col="darkblue")

white_noise$determinic_d <- detrend(white_noise$determinic)
plot(white_noise$n, white_noise$determinic_d, type = "l",main="Proceso de tendencia deterministica sin tendencia",xlab="Tiempo",col="darkblue")

##### plot 3 #####
par(mfrow=c(2,1))
plot(white_noise$n, white_noise$stochastic , type = "l", main="Caminata aleatoria sin deriva",xlab="Tiempo",col="purple")

white_noise$stochastic_d <- detrend(white_noise$stochastic)
plot(white_noise$n, white_noise$stochastic_d, type = "l",main="Caminata aleatoria sin deriva sin tendencia",xlab="Tiempo",col="purple")

# Resumiendo
ggplot(data = white_noise) +
  geom_line(aes(x = n, y = noise_d, color = "Ruido blanco sin tendencia")) +
  geom_line(aes(x = n, y = determinic_d, color = "Deterministico sin tendencia")) +
  geom_line(aes(x = n, y = stochastic_d, color = "Estocastico sin tendencia")) +
  ggtitle("Quitando tendencia de series de tiempo") +
  labs(x = "Tiempo", y = "Valor en el tiempo") +
  labs(colour = "Procesos de series") +
  theme_minimal() +
  theme_bw() +
  theme(panel.grid =element_blank())



## 

library(readxl)

df_countries <- read_excel("C:\\Users\\Alejandro Zavala\\Zavala_Programas\\Repositorios_Git\\Econometria\\Apoyo_insumos\\data_countries.xlsx")
head(df_countries)

ggplot(data = df_countries) +
  geom_line(aes(x = Year, y = Japan, color = "Japón")) +
  geom_line(aes(x = Year, y = US, color = "EUA")) +
  ggtitle("PIB a lo largo del tiempo") +
  labs(x = "Tiempo", y = "Valor en el tiempo") +
  labs(colour = "PIB") +
  theme_minimal() +
  theme_bw() +
  theme(panel.grid =element_blank())


# Aplicando logaritmo

df_countries$Japan_ln <- log(df_countries$Japan)
df_countries$US_ln <- log(df_countries$US)

head(df_countries)

ggplot(data = df_countries) +
  geom_line(aes(x = Year, y = Japan_ln, color = "Japón")) +
  geom_line(aes(x = Year, y = US_ln, color = "EUA")) +
  #geom_hline(yintercept=0) +
  ggtitle("ln(PIB) a lo largo del tiempo") +
  labs(x = "Tiempo", y = "Valor en el tiempo") +
  labs(colour = "ln(PIB)") +
  theme_minimal() +
  theme_bw() +
  theme(panel.grid =element_blank())

# Quitando tendencia a ambas series

df_countries$Japan_ln_d <- detrend(df_countries$Japan_ln)
df_countries$US_ln_d <- detrend(df_countries$US_ln)

head(df_countries,n=10)

ggplot(data = df_countries) +
  geom_line(aes(x = Year, y = Japan_ln_d, color = "Japón")) +
  geom_line(aes(x = Year, y = US_ln_d, color = "EUA")) +
  #geom_hline(yintercept=0) +
  ggtitle("Sin tendencia para el ln(PIB) a lo largo del tiempo") +
  labs(x = "Tiempo", y = "Valor en el tiempo") +
  labs(colour = "Sin tendencia para el ln(PIB)") +
  theme_minimal() +
  theme_bw() +
  theme(panel.grid =element_blank())


# Viendo ACF & PACF
library(forecast) # Agregaciones a ggplot2

ggAcf(df_countries$US_ln_d,type = "correlation",plot = TRUE,main="ACF Sin tendencia para el ln(PIB) de EUA",color="purple",lwd=2)
ggAcf(df_countries$Japan_ln_d,type = "correlation",plot = TRUE,main="ACF Sin tendencia para el ln(PIB) de Japón",color="purple",lwd=2)


ggAcf(df_countries$US_ln_d,type = "partial",plot = TRUE,main="PACF Sin tendencia para el ln(PIB) de EUA",color="darkgreen",lwd=2)
ggAcf(df_countries$Japan_ln_d,type = "partial",plot = TRUE,main="PACF Sin tendencia para el ln(PIB) de Japón",color="darkgreen",lwd=2)


# Ajustando a diferentes modelos

# MA(1),AR(1),AR(2), ARMA(1,1)

models_t <- list(c(0,0,1),c(1,0,0),c(2,0,0),c(1,0,1))

for(every_model in 1:length(models_t))
{
  print(paste("Modelo ARIMA (",models_t[[every_model]][1],models_t[[every_model]][2],models_t[[every_model]][3],")"))
  
  Japon_m <- arima(df_countries$Japan_ln_d, order = models_t[[every_model]])
  Japon_m_res <- residuals(Japon_m)
  
  US_m <- arima(df_countries$US_ln_d, order = models_t[[every_model]])
  US_m_res <- residuals(US_m)
  
  par(mfrow=c(2,1))
  acf(Japon_m_res,main=paste("Residuales de Japon con modelo ARIMA (",models_t[[every_model]][1],models_t[[every_model]][2],models_t[[every_model]][3],")"),col="purple",lwd=3)
  acf(US_m_res,main=paste("Residuales de EUA con modelo ARIMA (",models_t[[every_model]][1],models_t[[every_model]][2],models_t[[every_model]][3],")"),col="purple",lwd=3)
}