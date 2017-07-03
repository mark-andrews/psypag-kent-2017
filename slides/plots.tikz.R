library(tikzDevice)
library(ggplot2)
library(latex2exp)

n <- 250
m <- 139

beamer.parms = list(paperwidth   = 364.19536/72,  # converts `pt` to `in`
                    paperheight  = 273.14662/72,
                    textwidth    = 307.28987/72,
                    textheight   = 269.14662/72)

##########################################################################
tikz(file='tikZfigs/binomial.sampling.distribution.tex',
     standAlone=F,
     width = beamer.parms$paperwidth * 0.8, 
     height = beamer.parms$paperheight * 0.65)


theta.null <- 0.5

x <- seq(0, n)
y = dbinom(x, size = n, prob = theta.null)
expected.value <- n*theta.null
z <- abs(expected.value-x) >= abs(expected.value-m)

Df <- data.frame(x = x,
                 y = y,
                 z = z)

ggplot(Df,
       mapping=aes(x = x, y = y, fill=z)) + 
  geom_col(width = 0.65) +
  xlim(sapply(c(1e-5, 1-1e-5), function(x) qbinom(x, size=n, prob=theta.null))) + 
  theme_classic() + 
  guides(fill=FALSE) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  xlab('Observed number of Heads') +
  ylab('Probability')

dev.off()

############################################################################
tikz(file='tikZfigs/binomial.likelihood.2.tex',
     standAlone=F,
     width = beamer.parms$paperwidth * 0.8, 
     height = beamer.parms$paperheight * 0.6)

standardized.likelihood <- function(x, N, n){
  dbeta(x, n+1, N-n+1)/dbeta(n/N, n+1, N-n-1)
}

find.roots <- function(n, N, z=1/4) {
  
  f <- function(x, N, n, z) {
    standardized.likelihood(x, N, n) - z
  }
  
  left.root <- uniroot(f, 
                       c(0, n/N), 
                       tol = 0.0001, 
                       N = N, 
                       n=n,
                       z=z)
  
  right.root <- uniroot(f, 
                        c(n/N, 1), 
                        tol = 0.0001, 
                        N = N, 
                        n=n,
                        z=z)
  
  list(x = left.root$root,
       y = standardized.likelihood(left.root$root, N, n),
       xend = right.root$root,
       yend = standardized.likelihood(right.root$root, N, n))
}

theta <- seq(0, 1, by = 0.001)
ll <- standardized.likelihood(theta, n, m)

Df <- data.frame(theta,
                 ll = standardized.likelihood(theta, n, m))

likelihood.interval <- 1/4

roots <- find.roots(m, n, likelihood.interval)

ggplot(Df,
       mapping=aes(x = theta, y = ll)) + 
  geom_line() +
  theme_classic() + 
  ylab('$P(\\theta)$') +
  xlab('$\\theta$') +
  geom_segment(aes(x = roots$x,
                   y = roots$y,
                   xend = roots$xend,
                   yend = roots$yend),
               col='red') +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=rel(0.5)),
        axis.title=element_text(size=rel(0.7)))

dev.off()
############################################################################