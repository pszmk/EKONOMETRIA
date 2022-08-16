#===============================================================================#
# Raport zaliczeniowy - Metody ekonometryczne - 2021/2022 semestr letni

# Autorzy: Przemys³aw Pietrzak 109064 i Jakub Ko³pa 109434
#===============================================================================#

# Czyszczenie i obrabianie danych

rm(list=ls())
# Ustawianie folderu, w którym znajduje siê projekt jako working directory
setwd('C:/Users/Kuba/OneDrive - SGH/Pulpit/Metody ekonometryczne/Projekt')

# Wczytanie potrzebnych pakietów
require('ggplot2')
require('tseries')
require('lmtest')
require('tidyverse')
require('reshape2')
require('ggcorrplot')
require('mfp')
require('car')
require('sandwich')
require('stargazer')
require('tidyr')

# Wczytywanie danych z pliku excelowego do zmiennej df
df = read.csv('C:/Users/Kuba/OneDrive - SGH/Pulpit/Metody ekonometryczne/Projekt/owid-covid-data.csv', sep=',')

# Tworzenie wektora z wszystkimi nazwami zmiennych
variables = c('iso_code', 'date', 'total_deaths', 'total_cases', 'total_tests', 'positive_rate', 'total_vaccinations',
              'stringency_index', 'population_density', 'population', 'median_age', 'aged_65_older',
              'aged_70_older', 'gdp_per_capita', 'extreme_poverty', 'cardiovasc_death_rate', 'diabetes_prevalence',
              'female_smokers', 'male_smokers', 'hospital_beds_per_thousand', 'life_expectancy', 'human_development_index'
)

# Tworzenie wektora z nazwami zmiennych, które w nastêpnych etapach bêd¹ zamieniane na 
# œredni¹ z danego okresu
tmp_variables = c('iso_code', 'date', 'total_deaths', 'total_cases', 'total_tests', 'total_vaccinations', 'stringency_index', 'population')

# Tworzenie wektora z pozosta³ymi zmiennymi, których nie ma w tmp_variables
other_variables = c( 'positive_rate', 'population_density', 'median_age', 'aged_65_older',
                     'aged_70_older', 'gdp_per_capita', 'extreme_poverty', 'cardiovasc_death_rate', 'diabetes_prevalence',
                     'female_smokers', 'male_smokers', 'hospital_beds_per_thousand', 'life_expectancy', 'human_development_index'
)

# Tworzenie wektora z nazwami zmiennych, które bêd¹ dzielone przez wartoœæ zmiennej population
# w celu lepszego porównywania ich wartoœci miêdzy krajami
to_be_divided = c('total_deaths', 'total_cases', 'total_tests', 'total_vaccinations')

df.00 = df[, variables]

# Funkcja wypisuj¹ca numery wierszy, z okreœlon¹ dat¹
get_index = function(date=NA, data=NA){
  output = c()
  output = rownames(data[which(data$date==date),])
  return(output)
}

# Wybór pocz¹tku i d³ugoœci badanego okresu
beg_date = '2021-10-01'
len_period = 90

# Uzyskiwanie indeksów wierszy z dat¹ 2021-10-01
indexes = get_index(beg_date, df.00)

# Tworzenie pustego data frame
df.01 = data.frame('', matrix(ncol = length(variables)-1))
colnames(df.01) = variables

for(one_index in indexes){
  one_index = as.numeric(one_index)
  
  # Tworzenie pustego data frame
  tmp = data.frame('', matrix(ncol = length(variables)-1))
  colnames(tmp) = variables
  
  # Dzielenie wartoœci zmiennej population przez milion
  pop = as.numeric(df.00[one_index,]$population)/1000000
  
  # Dodanie zmiennej iso_code do tmp
  tmp$iso_code = df.00[one_index, 'iso_code']
  
  # Dla zmiennych zawartych w tmp_variables zamiana ich wartoœci na œredni¹ z badanego okresu
  for(one_var in tmp_variables[3:(length(tmp_variables))]){
    tmp[one_var] = mean(as.numeric(df.00[one_index:(one_index + len_period), one_var]))
  }
  
  # Dodanie zmiennych zawartych w other_variables do tmp
  for(one_var in other_variables){
    tmp[one_var] = as.numeric(df.00[one_index, one_var])
  }
  
  # Zamiana wartoœci zmiennych zawartych w to_be_divided na wartoœæ przedzielon¹ przez zmienn¹ population
  for(one_var in to_be_divided){
    tmp[one_var] = as.numeric(df.00[one_index, one_var])/pop
  }
  
  # Dodanie zmiennej date do tmp
  tmp$date = df.00[one_index, 'date']
  
  # Dodanie wartoœci z tmp do df.01
  df.01 = rbind(df.01, tmp)
}

# Usuwanie wartoœci NA z zmiennych 
# population, total_deaths, gdp_per_capita i life_expectancy ze zbioru danych
df.02 = df.01
df.02 = df.02[which(is.na(df.02$population) == 'FALSE'),]
df.02 = df.02[which(is.na(df.02$total_deaths) == 'FALSE'),]
df.02 = df.02[which(is.na(df.02$gdp_per_capita) == 'FALSE'),]
df.02 = df.02[which(is.na(df.02$life_expectancy) == 'FALSE'),]

# Zamiana wartoœci NA na 0 dla zmiennych total_tests i total_vaccinations
for(one_var in c('total_tests', 'total_vaccinations')){
  df.02[which(is.na(df.03[one_var])==TRUE), one_var] = 0
}

# Tworzenie pliku z "wyczyszczonymi" danymi
write.csv(df.02, paste('dane_przekrojowe_', beg_date, '.csv', sep=''))

# Wczytanie danych z utworzonego pliku
df = read.csv('C:/Users/Kuba/OneDrive - SGH/Pulpit/Metody ekonometryczne/Projekt/dane_przekrojowe_2021-10-01_3_mies.csv', sep=',')

# Tworzenie data frame'a do wykresów (transformacja do dwóch kolumn, aby u¿yæ facet_wrap)
df_plot_2 = gather(df[,4:ncol(df)], na.rm = TRUE)

# Tworzenie histogramów zmiennych przed ostatecznymi zmianami
ggplot(data=df_plot_2, mapping=aes(x=value, fill = key))+
  geom_histogram()+
  facet_wrap(~ key, scales = "free")+
  theme(legend.position="none")+
  theme(text = element_text(size = 10))

# Ostateczne usuwanie danych, aby modelowanie mia³o sens
df = df[which(df[,'iso_code']!='OWID_WRL'),]
df = df[which(df[,'iso_code']!='PER'),]
df = df[which(df[,'human_development_index']>0.70),]
df = df[which(df[,'population_density'] < 1000),]
df = df[which(df[,'total_deaths']>0),]
df = df[which(df[,'total_vaccinations']>10000),]

# Dodanie do data frame'a logarytmów zmiennych
df_copy = df
for(cn in colnames(df)[4:ncol(df)]){
  df_copy[paste(cn,'_l', sep='')] = log(1+df[,cn])
}
df = df_copy

# Tworzenie data frame'a df_plot_h do utworzenia regresji
df_plot = melt(data=df[,4:ncol(df)], id=c('total_deaths'), na.rm = TRUE)
df_plot_l = gather(df[,4:ncol(df)], na.rm = TRUE)
df_plot_h = df_plot[which(df_plot$variable!='total_deaths_l'),]

# Tworzenie wykresów regresji liniowej zmiennych i ich logarytmów wzglêdem total_deaths
ggplot(df_plot_h, mapping=aes(x=value,y=total_deaths,color=variable))+
  geom_point(shape = 21,fill="white",alpha=0.5, size =1, stroke =1)+
  geom_smooth(method = lm, se=F)+
  facet_wrap(~variable, scales ="free")+
  theme(legend.position="none")+
  theme(text = element_text(size = 10))

# Tworzenie histogramów zmiennych i ich logarytmów
ggplot(data=df_plot_l, mapping=aes(x=value, fill = key))+
  geom_histogram()+
  facet_wrap(~ key, scales = "free")+
  theme(legend.position="none")+
  theme(text = element_text(size = 10))

# Tworzenie wykresów pude³kowych zmiennych i ich logarytmów
ggplot(data=df_plot_l, mapping=aes(x=value, fill = key))+
  geom_boxplot()+
  facet_wrap(~ key, scales = "free")+
  theme(legend.position="none")+
  theme(text = element_text(size = 10))

# Tworzenie histogramu gêstoœci zmiennej total_deaths 
ggplot(df, aes(x=total_deaths)) +
  geom_histogram(aes(y=..density..), color="darkblue", fill="lightblue", bins=10)+
  geom_density(alpha=.2, fill="#FF6666")

# Przygotowanie danych do utworzenia korelogramu
df_cor = df[,4:ncol(df)]
df_cor = df_cor[,c('total_deaths', 'total_cases', 'total_tests', 'positive_rate', 'total_vaccinations', 'stringency_index', 'population_density', 'median_age', 'aged_65_older', 'aged_70_older', 'gdp_per_capita', 'extreme_poverty', 'life_expectancy', 'human_development_index')]
summary(df_cor)
df_cor = df_cor[which(is.na(df_cor$positive_rate)==FALSE),]
df_cor = df_cor[which(is.na(df_cor$stringency_index)==FALSE),]
df_cor = df_cor[which(is.na(df_cor$median_age)==FALSE),]
df_cor = df_cor[which(is.na(df_cor$aged_65_older)==FALSE),]
df_cor = df_cor[which(is.na(df_cor$aged_70_older)==FALSE),]
df_cor = df_cor[which(is.na(df_cor$extreme_poverty)==FALSE),]
corr = cor(df_cor)

# Tworzenie korelogramu
ggcorrplot(corr,
           type = "lower", 
           lab = TRUE,
           lab_size = 4,
           method="square", 
           ggtheme = theme_bw())

# Tworzenie funkcji, która wyœwietla estymacjê modelu, histogram reszt z modelu,
# test Jarque-Bera, test Breuscha-Pagana, test RESET, CIW, i modele z odpornymi b³êdami standandardowymi HC0 i HC1
model_quick_check = function(mod){
  print('--- MODEL SUMMARY ---')
  print(summary(mod))
  print('--- MODEL RESIDUALS ---')
  x=data.frame(mod$residuals)
  colnames(x) = 'resid'
  ggplot(x, aes(x=resid)) + 
    geom_histogram(aes(y=..density..), color="darkblue", fill="lightblue")+
    geom_density(alpha=.2, fill="#FF6666") 
  hist(x$resid, breaks=20)
  
  print(nrow(df))
  print(length(x$resid))
  print('--- STANDARD TESTS ---')
  print(jarque.bera.test(mod$residuals))
  print(reset(mod))
  print(bptest(mod))
  print(vif(mod))
  print(coeftest(mod, vcov = vcovHC(mod, type = "HC0")))
  print(coeftest(mod, vcov = vcovHC(mod, type = "HC1")))
}

# Testowanie modeli
model.1 = lm(formula = total_deaths~total_cases+total_vaccinations+positive_rate, data = df)
model_quick_check(model.1)
model.2 = lm(formula = total_deaths~total_cases+total_vaccinations+extreme_poverty, data = df)
model_quick_check(model.2) # extreme poverty na sens
model.3 = lm(formula = total_deaths~total_cases+total_vaccinations+population_density+extreme_poverty, data = df)
model_quick_check(model.3)
model.4 = lm(formula = total_deaths~total_cases+total_vaccinations+I(total_vaccinations*total_vaccinations), data = df)
model_quick_check(model.4)
model.5 = lm(formula = total_deaths~total_cases+I(total_vaccinations*total_vaccinations), data = df)
model_quick_check(model.5)
model.6 = lm(formula = total_deaths~total_cases+total_vaccinations+gdp_per_capita, data = df)
model_quick_check(model.6) # tutaj widac ponikat, ze vaccinations ujmowalo troche gdp_per_capita
model.7 = lm(formula = total_deaths~total_cases+I(total_vaccinations*total_vaccinations)+gdp_per_capita, data = df)
model_quick_check(model.7)
model.10 = lm(formula = total_deaths~total_cases+I(total_vaccinations*total_vaccinations)+gdp_per_capita+aged_65_older, data = df)
model_quick_check(model.10)
model.11 = lm(formula = total_deaths~total_cases+I(total_vaccinations*total_vaccinations)+gdp_per_capita+aged_70_older, data = df)
model_quick_check(model.11)
model.15 = lm(formula = total_deaths~total_cases+I(total_vaccinations*total_vaccinations)+gdp_per_capita+aged_70_older, data = df)
model_quick_check(model.15)
model.16 = lm(formula = total_deaths~total_cases+total_vaccinations+gdp_per_capita+aged_70_older, data = df)
model_quick_check(model.16)
model.10.1 = lm(formula = total_deaths~total_cases+total_vaccinations+gdp_per_capita+aged_65_older, data = df)
model_quick_check(model.10.1)

# Wypisanie tabelki z wyestymowanymi modelami do pliku html
stargazer(model.1,model.2,model.3,model.4,model.5,model.6,model.7,model.10, model.10.1, model.11, model.15, model.16, type='text', out = 'C:/Users/Kuba/OneDrive - SGH/Pulpit/Metody ekonometryczne/Projekt/modeleee.html')

# wykresy kwadratów reszt wzglêdem zmiennych objasniaj¹cych
index = names(model.10.1$residuals^2)
df2 = df[index,]
df2$ehat2 = model.10.1$residuals^2
plot(df2$total_cases,df2$ehat2)
plot(df2$total_vaccinations,df2$ehat2)
plot(df2$gdp_per_capita,df2$ehat2)
plot(df2$aged_65_older,df2$ehat2)
plot(fitted(model.10.1),df2$ehat2)
df2_plot = melt(data=df2[,c('ehat2','total_cases','total_vaccinations','gdp_per_capita','aged_65_older')], id=c('ehat2'), na.rm = TRUE)
ggplot(df2_plot, mapping=aes(x=value,y=ehat2,color=variable))+
  geom_point(shape = 21,fill="white",alpha=0.5, size =1, stroke =1)+
  facet_wrap(~variable, scales ="free")+
  theme(legend.position="none")+
  theme(text = element_text(size = 10))

# Test Goldfelda-Quandta
ind = order(df$total_cases)
N=nrow(df)
N1 = round(N/2)
N2 = N-N1
data1 = df[ind[1:N1],]
data2 = df[ind[(N1+1):N],]
model2=lm(total_deaths~total_cases+total_vaccinations+gdp_per_capita+aged_65_older,data=data1)
model3=lm(total_deaths~total_cases+total_vaccinations+gdp_per_capita+aged_65_older,data=data2)
summary(model2)
summary(model3)

SSE1 = sum(model2$residuals^2)
SSE2 = sum(model3$residuals^2)
F=(SSE2/(N2-7))/(SSE1/(N1-7)) # statystyka F
pf(F,N2-7,N1-7,lower.tail = FALSE) # p-value dla stytystyki F

# 1.3 Test White'a
attach(df2)
white.model = lm(ehat2~total_cases+total_vaccinations+gdp_per_capita+aged_65_older+
                   I(total_cases^2)+I(total_vaccinations^2)+I(gdp_per_capita^2)+I(aged_65_older^2)+
                   I(total_cases*total_vaccinations)+I(total_cases*gdp_per_capita)+I(total_cases*aged_65_older)+
                   I(total_vaccinations*gdp_per_capita)+I(total_vaccinations*aged_65_older)+I(gdp_per_capita*aged_65_older))
LM=N*summary(white.model)$r.squared # statystyka testu mnoznika Lagrange
LM
pchisq(LM,14, lower.tail=FALSE)

# Wa¿ona MNK
df2$w1 = 1/df2$total_cases
df2$w3 = 1/df2$gdp_per_capita
w1m1 = lm(total_deaths~total_cases+total_vaccinations+gdp_per_capita+aged_65_older, data=df2, weights = df2$w1)
summary(w1m1)
bptest(w1m1)
w3m3 = lm(total_deaths~total_cases+total_vaccinations+gdp_per_capita+aged_65_older, data=df2, weights = df2$w3)
summary(w3m3)
bptest(w3m3)
auxilary.lm		=	lm(log(ehat2)~total_cases+total_vaccinations+gdp_per_capita+aged_65_older, data=df2)
df2$w5	=	1/exp(auxilary.lm$fitted.values)
w5m5 = lm(total_deaths~total_cases+total_vaccinations+gdp_per_capita+aged_65_older, data=df2, weights = df2$w5)
summary(w5m5)
bptest(w5m5)
stargazer(w1m1,w3m3,w5m5, type="text", out = 'C:/Users/Kuba/OneDrive - SGH/Pulpit/Metody ekonometryczne/Projekt/wazonemnk2.html')

# Skorelowanie reszt modelu ze zmienna podejrzan¹ o endogenicznoœæ
cor(df[names(model.10.1$residuals),'gdp_per_capita'], model.10.1$residuals)

# Skorelowanie reszt modelu ze zmiennymi instrumentalnymi

i = names(model.10.1$residuals)
str_ind = df[i, 'stringency_index']
n_index = c(1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
            21, 22, 23, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 40,
            42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62)

resid = model.10.1$residuals
length(resid)
length(str_ind)
names(resid) = names(str_ind)
str_ind = str_ind[n_index]
resid = resid[n_index]
length(str_ind)
length(resid)


# Korelacja reszt ze zmienn¹ podejrzan¹ o endogenicznoœæ gdp_per_capita

cor(df[names(model.10.1$residuals),'gdp_per_capita'], model.10.1$residuals)

# Korelacja reszt ze zmienn¹ stringency_index

r_se = cor(str_ind, resid)

# Korelacja reszt ze zmienn¹ human_development_index

r_he = cor(df[names(model.10.1$residuals),'human_development_index'], model.10.1$residuals)

# Korelacja zmiennej podejrzanej o endogenicznoœæ ze zmienn¹ instrumentaln¹ human_development_index

r_hg = cor(df[names(model.10.1$residuals),'human_development_index'], df[names(model.10.1$residuals),'gdp_per_capita'])

# Korelacja zmiennej podejrzanej o ednogenicznoœæ ze zmienn¹ instrumentaln¹ stringency_index

n_df = df[names(model.10.1$residuals),c('stringency_index', 'gdp_per_capita')]
n_df = n_df[which(is.na(n_df['stringency_index'])==FALSE),]
r_sg = cor(n_df['stringency_index'], n_df['gdp_per_capita'])

r_he/r_hg
r_se/r_sg

library(ivreg) 

# Dwustopniowa metoda najmiejszych kwadratów
tsls = ivreg(formula = total_deaths~total_cases+total_vaccinations+aged_65_older+gdp_per_capita|
               total_cases+total_vaccinations+aged_65_older+human_development_index+stringency_index, data=df)

stargazer(model.10.1, tsls, type='text')
summary(tsls, diagnostics = TRUE)

library(ivpack)
robust.se(tsls)

bptest(tsls)
vif(tsls)

# Rozwi¹zanie problemu niesferycznoœci macierzy wariancji-kowariancji sk³adnika losowego
tsls2 = ivreg(formula = total_deaths~total_cases+total_vaccinations+aged_65_older+gdp_per_capita|
               total_cases+total_vaccinations+aged_65_older+human_development_index+stringency_index, data=df, weights = 1/df$total_cases)
summary(tsls2, diagnostics = TRUE)





