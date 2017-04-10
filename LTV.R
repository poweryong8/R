f1<- function(x) 0.8863*x^-0.508 # now
f2<- function(x) 0.4718*x^-0.359 #retention rate +0.05
f3<- function(x) 0.6502*x^-0.405 #retention rate +0.1
f4<- function(x) 0.6608*x^-0.378 #retention rate +0.12
f5<- function(x) 0.6411*x^-0.437
f6<- function(x) 0.9119*x^-1.056
integrate(f1, 1, 90)
integrate(f1, 1, 180)
integrate(f1, 1, 360)
integrate(f2, 1, 90)
integrate(f2, 1, 180)
integrate(f2, 1, 360)
integrate(f6, 1, 360)
f1(360)
