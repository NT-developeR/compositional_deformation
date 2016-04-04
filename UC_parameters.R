rm(list= ls())
uc_data <- read.delim("D:/R projects/crystallograpgy/crystal_composition.txt", header = TRUE, stringsAsFactors = FALSE)
uc_data <- uc_data[order(uc_data$состав....L.Thr),]
#добавляем колонку с составом L-aThr и считаем x1a1+x2a2
composition_LaThr =  1-uc_data$состав....L.Thr
param_composition_LaThr <- composition_LaThr*mean(uc_data[which(composition_LaThr == 1), "parameter.a"], na.rm = TRUE)
param_composition_LThr <- uc_data$состав....L.Thr*mean(uc_data[which(composition_LaThr == 0), "parameter.a"], na.rm = TRUE)

# plot(uc_data$состав....L.Thr, param_composition_LaThr, type = "b", col = "green", lwd = 2)
# lines(uc_data$состав....L.Thr, param_composition_LThr, type = "b", col = "red", lwd = 2)
sum_compsition_pure <- param_composition_LThr+param_composition_LaThr

uc_data <- cbind(uc_data, composition_LaThr, param_composition_LThr, param_composition_LaThr)


plot(uc_data$состав....L.Thr, uc_data$parameter.a)

#1.  Строим гистограммы погрешностей - может лучше показать в процентах?

plot(density(uc_data$error.c), col = "green", xlab = "погрешность определения параметра ромбической ячейки, A", lwd = 2)
lines(density(uc_data$error.b), col = "blue", lwd = 2)
lines(density(uc_data$error.a), col = "red", lwd = 2)

#2. Находим максимальный разброс между значенями для каждой из осей
delta_basis <- max(max(uc_data$parameter.a)-min(uc_data$parameter.a),
                   max(uc_data$parameter.b)-min(uc_data$parameter.b),
                   max(uc_data$parameter.c)-min(uc_data$parameter.c)
                   )
delta_a <- max(uc_data$parameter.a)-min(uc_data$parameter.a)
#3 строим шкалированные значения параметров
  scale_a <- NULL
  scale_b <- NULL
  scale_c <- NULL
  scale_aa <- NULL
  for (i in 1:length(uc_data$parameter.a)){
   scale_a[i] <- (abs( min(uc_data$parameter.a)-uc_data[i, "parameter.a"]))/delta_basis
   scale_b[i] <- (abs(min(uc_data$parameter.b)- uc_data[i, "parameter.b"]))/delta_basis
   scale_c[i] <- (abs(min(uc_data$parameter.c)- uc_data[i, "parameter.c"]))/delta_basis
  }
  
  
#4 Рисуем рисунок шкалированные значения - объем ячейки
opar <- par(mfrow= c(2,1))

plot(uc_data$состав....L.Thr, scale_b, col = "blue", cex=2, pch = 16, xlab = " ", ylab = "относительное изменение ПЭЯ")
points(uc_data$состав....L.Thr, scale_a, col = "red", cex= 2,pch = 16)
points(uc_data$состав....L.Thr, scale_c, col= "green", cex= 2, pch = 16)
plot(uc_data$состав....L.Thr, uc_data$volume, xlab = "состав кристалла, доли L-Thr", ylab = "относительное изменение ПЭЯ", pch = 17)
abline(lm)

plot(uc_data$состав....L.Thr, scale_aa, pch = 16, col = "orange" )
par(opar)
#доделать рисунок


#regression analisys of orthorhombic parameters variation with composition: direct approach

regr_a <- lm(uc_data$parameter.a ~ sum_compsition_pure + uc_data$состав....L.Thr*composition_LaThr)
summary(regr_a)
regr_b <- lm(uc_data$parameter.b ~ sum_compsition_pure + uc_data$состав....L.Thr*composition_LaThr)
summary(regr_b)
regr_c <- lm(uc_data$parameter.c ~ sum_compsition_pure + uc_data$состав....L.Thr*composition_LaThr)
summary(regr_c)




#regression analisys of orthorhombic parameters variation with composition: quadratic_equation
regr_a_qdr <- lm(uc_data$parameter.a ~ uc_data$composition_LaThr - uc_data$composition_LaThr^2)

summary(regr_a_qdr)
plot(uc_data$composition_LaThr, uc_data$parameter.a)
abline( lm(uc_data$parameter.a ~ uc_data$composition_LaThr - uc_data$composition_LaThr^2))
#regression analisys of "scaled" orthorhombic parameters variation with composition

#yflj gtht
regr_a_sc <- lm(scale_a ~ sum_compsition_pure + uc_data$состав....L.Thr*composition_LaThr)
summary(regr_a_sc)
regr_b_sc <- lm(scale_b ~ sum_compsition_pure + uc_data$состав....L.Thr*composition_LaThr)
summary(regr_b_sc)
regr_c_sc <- lm(scale_c ~ sum_compsition_pure + uc_data$состав....L.Thr*composition_LaThr)
summary(regr_c_sc)
