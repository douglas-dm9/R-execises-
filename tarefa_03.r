# Exercício 1

fresh <- c(10.2,10.5,10.3,10.8,9.8,10.6,10.7,10.2,10,10.6)
stored <- c(9.8,9.6,10.1,10.2,10.1,9.7,9.5,9.6,9.8,9.9)


t.test(fresh,stored, var.equal=T, alternative = 'two.sided',conf.level = 0.95)


# Exercício 2
y1 = 8.27
y2 = 6.78
s1=2.956
s2= 2.565
n1=10
n2 = 9

t.test.from.summary.data <- function(mean1, sd1, n1, mean2, sd2, n2, ...) {
    data1 <- scale(1:n1)*sd1 + mean1
    data2 <- scale(1:n2)*sd2 + mean2
    t.test(conf.level = 0.95,data1, data2, ...)
}

t.test.from.summary.data(y1,s1,n1,y2,s2,n2)



# Exercício 3
dt_sheep <- c(18, 43, 28, 50, 16, 32, 13, 35, 38, 33, 6, 7)
ut_sheep <- c(40,54 ,26, 63, 21 ,37, 39, 23, 48, 58, 28, 39)
t.test(ut_sheep, dt_sheep,var.equal=T, alternative = 'two.sided',conf.level = 0.95)

# Exercício 4

y1 = 25.2
y2 = 33.9
s1=8.6
s2= 17.4
n1=33
n2 = 12
t.test.from.summary.data(y1,s1,n1,y2,s2,n2)