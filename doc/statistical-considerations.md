# Statistical considerations in "Developing systems for monitoring navel orangeworm..." ms

## Experiment 1: PPO and kairomone Blend with and without pheromone, 2017

### Complications with almonds in mating disruption
 - Using GLIMMIX for GLMM
    - deviance/df ratio indicated that the model with Poisson distribution 
	was a poor fit
	- failed to converge when attempted with negative binomial
 - Nonparametric alternatives
    - Kruskal-Wallis inappropriate if frequency distributions vary between
	treatments (Mangiafico 2015, https://rcompanion.org/rcompanion
    - The Welch ANOVA might be more appropriate under this circumstance 
	(Zar 1999, McDonald 2014 http://www.biostathandbook.com/onewayanova.html, 
	Mangiafico 2015)
    - The Games-Howell post-hoc test is recommended for use with the Welch 
	ANOVA (Mangiafico 2015), and can be implemented with the R package 
	userfriendlyscience (https://rpubs.com/aaronsc32/games-howell-test, 
	Peters 2018 https://www.doi.org/10.17605/osf.io/txequ	