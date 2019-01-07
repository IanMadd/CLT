This is a Shiny App that simulates the central limit theorem. You can try it out at [shinyapps.io](https://ianmadd.shinyapps.io/CentralLimitTheorem/).

The App simulates normal, binomial, log normal, exponential, uniform, poisson, beta, student-t, and chi-squared population distributions, plots those distributions, plots population samples and sample mean distributions. The user can change the sample size to show how sample size affects the sample mean distribution.

This borrows code and ideas from:
* https://qualityandinnovation.com/2015/03/30/sampling-distributions-and-central-limit-theorem-in-r/ 

* https://github.com/ShinyEd/ShinyEd/tree/master/CLT_mean


This deliberately doesn't allow the user to select the number of sample means 
for the simulation.

Quoting from Quality And Innovation:

"You aren’t allowed to change the number of replications in this simulation because of the nature of the sampling distribution: it’s a theoretical model that describes the distribution of statistics from an infinite number of samples. As a result, if you increase the number of replications, you’ll see the mean of the sampling distribution bounce around until it converges on the mean of the population. This is just an artifact of the simulation process: it’s not a characteristic of the sampling distribution, because to be a sampling distribution, you’ve got to have an infinite number of samples."

Ten thousand samples is close enough to an infinite number that the distribution will cluster around the population mean, but not so many that it will overwhelm a computer doing the simulation.

Allowing users to select the number of sample means in a simulation will give them the false impression that the distribution of sample means will become more normal and the distribution will approach the population mean, as the number of sample means increases.
 
For additional reading see here: 
http://www.amstat.org/publications/jse/v22n3/watkins.pdf
