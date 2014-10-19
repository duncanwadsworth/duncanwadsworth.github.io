# What is Ebola's Mortality Rate?
Duncan Wadsworth  
Oct 17, 2014  

---

> ### Disclaimer
> I **am not** an epidemiologist, public health expert, or infectious diseases specialist. But I have been frustrated by the innumeracy displayed by journalists reporting on the current Ebola outbreak. In particular, no one seemed to report confidence intervals on the mortality rate of Ebola. Since I **am** a statistician I chose to satisfy my own curiousity and look into it myself. Numerous problems arise in a data analysis like this and I address a few of them at the bottom of this page. 

---

# Short Answer: 2/3

No matter how you calculate it (and I explore this exhaustively below), with 95% confidence, the mortality rate is 66% $\pm$ 2%. Depending on where the data is obtained, **and not including the current outbreak**, since 1976 there have been about 25 Ebola outbreaks resulting in about 1600 deaths from about 2400 cases. With this much data there is little uncertainty in these calculations. However, very good arguments could be made about biased sampling which would most likely push the mortality estimates lower. 

---

# Long Anwer: 2/3, conditionally

Outline:

1. We'll scrape the data from the World Health Organization's website 
2. Since data scraped from HTML pages are usually pretty mangled, clean and reformat it
3. Calculate the overall mortality rate and several types of confidence intervals
4. Calculate a Bayesian posterior distribution, a mortality rate point estimate from that distribution, and it's corresponding credible interval
5. Compare the approaches graphically
6. Look as some slightly more complicated models

All the code to produce the analyses, plots, and tables is include in case anyone would like to follow along at home.

#### Enter into the Hadleyverse


```r
#install.packages(devtools)
#library(devtools)
#install_github("hadley/rvest")
suppressPackageStartupMessages(library(rvest))
library(ggplot2)
library(plyr)
library(xtable)
```

## Scrape WHO fact sheet #103

Using `rvest`'s crazy simple web scraping abilities, get data from WHO's website.  To do this we obtain an `html` object then extract the table we want from that object.  Since there's only one table on [this page](http://www.who.int/mediacentre/factsheets/fs103/en/) index that one.


```r
aa = html("http://www.who.int/mediacentre/factsheets/fs103/en/")
bb = html_table(aa, header = T, fill = T)[[1]]
```

The data.frame needs a little bit of cleanup. Strip the month information away on the date and remove all but the first year of multiyear outbreaks.


```r
cc = na.omit(bb[, setdiff(names(bb), c("", "Case fatality"))])
## avert thine eyes
cc$Year = unlist(lapply(lapply(strsplit(cc$Year, split = ""), `[`, 1:4), paste, collapse = ""))
## other calculations, type conversions, and name changes
cc$MLE = cc$Deaths/cc$Cases
cc$Year = as.integer(cc$Year)
names(cc)[names(cc) == "Ebolavirus species"] = "Species"
ebo01 = cc
```

## The Data

A table of the cleaned and formatted data and a quick summary.  The dataset contains 2387 total cases and 1590 total deaths.


```r
print(xtable(ebo01), type = 'html', include.rownames = F)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Sat Oct 18 21:17:16 2014 -->
<TABLE border=1>
<TR> <TH> Year </TH> <TH> Country </TH> <TH> Species </TH> <TH> Cases </TH> <TH> Deaths </TH> <TH> MLE </TH>  </TR>
  <TR> <TD align="right"> 2012 </TD> <TD> Democratic Republic of Congo </TD> <TD> Bundibugyo </TD> <TD align="right">  57 </TD> <TD align="right">  29 </TD> <TD align="right"> 0.51 </TD> </TR>
  <TR> <TD align="right"> 2012 </TD> <TD> Uganda </TD> <TD> Sudan </TD> <TD align="right">   7 </TD> <TD align="right">   4 </TD> <TD align="right"> 0.57 </TD> </TR>
  <TR> <TD align="right"> 2012 </TD> <TD> Uganda </TD> <TD> Sudan </TD> <TD align="right">  24 </TD> <TD align="right">  17 </TD> <TD align="right"> 0.71 </TD> </TR>
  <TR> <TD align="right"> 2011 </TD> <TD> Uganda </TD> <TD> Sudan </TD> <TD align="right">   1 </TD> <TD align="right">   1 </TD> <TD align="right"> 1.00 </TD> </TR>
  <TR> <TD align="right"> 2008 </TD> <TD> Democratic Republic of Congo </TD> <TD> Zaire </TD> <TD align="right">  32 </TD> <TD align="right">  14 </TD> <TD align="right"> 0.44 </TD> </TR>
  <TR> <TD align="right"> 2007 </TD> <TD> Uganda </TD> <TD> Bundibugyo </TD> <TD align="right"> 149 </TD> <TD align="right">  37 </TD> <TD align="right"> 0.25 </TD> </TR>
  <TR> <TD align="right"> 2007 </TD> <TD> Democratic Republic of Congo </TD> <TD> Zaire </TD> <TD align="right"> 264 </TD> <TD align="right"> 187 </TD> <TD align="right"> 0.71 </TD> </TR>
  <TR> <TD align="right"> 2005 </TD> <TD> Congo </TD> <TD> Zaire </TD> <TD align="right">  12 </TD> <TD align="right">  10 </TD> <TD align="right"> 0.83 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> Sudan </TD> <TD> Sudan </TD> <TD align="right">  17 </TD> <TD align="right">   7 </TD> <TD align="right"> 0.41 </TD> </TR>
  <TR> <TD align="right"> 2003 </TD> <TD> Congo </TD> <TD> Zaire </TD> <TD align="right">  35 </TD> <TD align="right">  29 </TD> <TD align="right"> 0.83 </TD> </TR>
  <TR> <TD align="right"> 2003 </TD> <TD> Congo </TD> <TD> Zaire </TD> <TD align="right"> 143 </TD> <TD align="right"> 128 </TD> <TD align="right"> 0.90 </TD> </TR>
  <TR> <TD align="right"> 2001 </TD> <TD> Congo </TD> <TD> Zaire </TD> <TD align="right">  59 </TD> <TD align="right">  44 </TD> <TD align="right"> 0.75 </TD> </TR>
  <TR> <TD align="right"> 2001 </TD> <TD> Gabon </TD> <TD> Zaire </TD> <TD align="right">  65 </TD> <TD align="right">  53 </TD> <TD align="right"> 0.82 </TD> </TR>
  <TR> <TD align="right"> 2000 </TD> <TD> Uganda </TD> <TD> Sudan </TD> <TD align="right"> 425 </TD> <TD align="right"> 224 </TD> <TD align="right"> 0.53 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> South Africa (ex-Gabon) </TD> <TD> Zaire </TD> <TD align="right">   1 </TD> <TD align="right">   1 </TD> <TD align="right"> 1.00 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> Gabon </TD> <TD> Zaire </TD> <TD align="right">  60 </TD> <TD align="right">  45 </TD> <TD align="right"> 0.75 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> Gabon </TD> <TD> Zaire </TD> <TD align="right">  31 </TD> <TD align="right">  21 </TD> <TD align="right"> 0.68 </TD> </TR>
  <TR> <TD align="right"> 1995 </TD> <TD> Democratic Republic of Congo </TD> <TD> Zaire </TD> <TD align="right"> 315 </TD> <TD align="right"> 254 </TD> <TD align="right"> 0.81 </TD> </TR>
  <TR> <TD align="right"> 1994 </TD> <TD> Cote d'Ivoire </TD> <TD> TaÃ¯ Forest </TD> <TD align="right">   1 </TD> <TD align="right">   0 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 1994 </TD> <TD> Gabon </TD> <TD> Zaire </TD> <TD align="right">  52 </TD> <TD align="right">  31 </TD> <TD align="right"> 0.60 </TD> </TR>
  <TR> <TD align="right"> 1979 </TD> <TD> Sudan </TD> <TD> Sudan </TD> <TD align="right">  34 </TD> <TD align="right">  22 </TD> <TD align="right"> 0.65 </TD> </TR>
  <TR> <TD align="right"> 1977 </TD> <TD> Democratic Republic of Congo </TD> <TD> Zaire </TD> <TD align="right">   1 </TD> <TD align="right">   1 </TD> <TD align="right"> 1.00 </TD> </TR>
  <TR> <TD align="right"> 1976 </TD> <TD> Sudan </TD> <TD> Sudan </TD> <TD align="right"> 284 </TD> <TD align="right"> 151 </TD> <TD align="right"> 0.53 </TD> </TR>
  <TR> <TD align="right"> 1976 </TD> <TD> Democratic Republic of Congo </TD> <TD> Zaire </TD> <TD align="right"> 318 </TD> <TD align="right"> 280 </TD> <TD align="right"> 0.88 </TD> </TR>
   </TABLE>

```r
print(xtable(summary(data.frame(Year = factor(ebo01$Year), Country = factor(ebo01$Country), Species = factor(ebo01$Species), Cases = ebo01$Cases, Deaths = ebo01$Deaths))), type = 'html', include.rownames = F)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Sat Oct 18 21:17:16 2014 -->
<TABLE border=1>
<TR> <TH>      Year </TH> <TH>                         Country </TH> <TH>        Species </TH> <TH>     Cases </TH> <TH>     Deaths </TH>  </TR>
  <TR> <TD> 1996   : 3   </TD> <TD> Congo                       :4   </TD> <TD> Bundibugyo : 2   </TD> <TD> Min.   :  1.0   </TD> <TD> Min.   :  0.00   </TD> </TR>
  <TR> <TD> 2012   : 3   </TD> <TD> Cote d'Ivoire               :1   </TD> <TD> Sudan      : 7   </TD> <TD> 1st Qu.: 15.8   </TD> <TD> 1st Qu.:  9.25   </TD> </TR>
  <TR> <TD> 1976   : 2   </TD> <TD> Democratic Republic of Congo:6   </TD> <TD> TaÃ¯ Forest: 1   </TD> <TD> Median : 43.5   </TD> <TD> Median : 29.00   </TD> </TR>
  <TR> <TD> 1994   : 2   </TD> <TD> Gabon                       :4   </TD> <TD> Zaire      :14   </TD> <TD> Mean   : 99.5   </TD> <TD> Mean   : 66.25   </TD> </TR>
  <TR> <TD> 2001   : 2   </TD> <TD> South Africa (ex-Gabon)     :1   </TD> <TD>  </TD> <TD> 3rd Qu.:144.5   </TD> <TD> 3rd Qu.: 71.75   </TD> </TR>
  <TR> <TD> 2003   : 2   </TD> <TD> Sudan                       :3   </TD> <TD>  </TD> <TD> Max.   :425.0   </TD> <TD> Max.   :280.00   </TD> </TR>
  <TR> <TD> (Other):10   </TD> <TD> Uganda                      :5   </TD> <TD>  </TD> <TD>  </TD> <TD>  </TD> </TR>
   </TABLE>

## Frequentist Point Estimates and Confidence Intervals

The overall mortality rate (estimated via maximum likelihood) and several (1 - $\alpha$ = 0.95) confidence intervals. The  different confidence interval approaches are pulled straight from the Wikipedia page for the [Binomial distribution](http://en.wikipedia.org/wiki/Binomial_distribution).


```r
nn = sum(ebo01$Cases) ## total number of cases
xx = sum(ebo01$Deaths) ## total number of deaths
(phat = xx/nn) ## maximum likelihood estimate of mortality rate
```

```
## [1] 0.6661
```

That is remarkably close to 2/3.


```r
alpha = 0.05 ## alpha level
zz = qnorm(1 - alpha/2) ## half alpha z-score quantile 
## Wald method
wald = zz * sqrt((phat * (1 - phat))/nn)
## Wald method with continuity correction
waldcc = wald + 0.5 / nn 
## Agresti-Coull method
agcophat = (sum(ebo01$Deaths) + 0.5 * zz^2)/(nn + zz^2)
agco = zz * sqrt((agcophat * (1 - agcophat))/(nn + zz^2))
## ArcSine method
aspos = sin(asin(sqrt(phat)) + zz/(2 * sqrt(nn)))^2 - phat
asneg = phat - sin(asin(sqrt(phat)) - zz/(2 * sqrt(nn)) )^2
## Wilson (score) method
wphat = (phat + (1/(2 * nn)) * zz^2)/(1 + (1/nn) * zz^2) 
wpos = ((1/(2 * nn)) * zz * sqrt(4 * nn * phat * (1 - phat) + zz^2))/(1 + (1/nn) * zz^2)
wneg = ((1/(2 * nn)) * zz * sqrt(4 * nn * phat * (1 - phat) + zz^2))/(1 + (1/nn) * zz^2)
```

That's a lot of intervals.  After getting the Bayesian estimate we'll take a closer look at those results.

## Bayesian Estimation

We clearly have Binomial sampling so the most obvious thing to do is use the Beta-Binomial model.  The Beta-Binomial is often the first example of Bayesian inference in any given textbook.  I like how Christensen et al., (2010)[^Christensen2010] call the Beta an example of a "Data augmentation prior" since the prior parameters simply add to the likelihood in the posterior.  The function `bbpost()` is defined at the end of the page. I'll omit the explanation and algebra since there are more than enough resources out there. 




```r
priora = 1
priorb = 1
pp = bbpost(xx, nn, priora, priorb)
pp$postmean
```

```
## [1] 0.666
```

Which is also remarkably close to 2/3.  The following plot shows the point estimates and their intervals graphically.  The horizontal dashed line is placed at 2/3.


```r
eboest01 = data.frame(Method = c("Wald","Wald + CC","Agresti-Coull","Arcsine","Wilson", "Bayesian"), phat = c(phat, phat, agcophat, phat, wphat, pp$postmean), minus = c(wald, waldcc, agco, asneg, wneg, pp$postminus), plus = c(wald, waldcc, agco, aspos, wpos, pp$postplus))
ee = ggplot(eboest01, aes(Method, phat, ymin = phat - minus, ymax = phat + plus))
ee + geom_pointrange() + coord_flip() + ylim(c(0,1)) + geom_hline(yintercept = 2/3, linetype = 2) + ylab("Mortality rate") + xlab("") + ggtitle("Estimated mortality rate\n with 95% confidence intervals according to several methods")
```

![plot of chunk unnamed-chunk-8](./ebola_files/figure-html/unnamed-chunk-8.png) 

Just to verify that the numbers behind this plot are actually changing check the results in tabular form.


```r
print(xtable(eboest01, digits = 6), type = 'html', include.rownames = F)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Sat Oct 18 21:17:17 2014 -->
<TABLE border=1>
<TR> <TH> Method </TH> <TH> phat </TH> <TH> minus </TH> <TH> plus </TH>  </TR>
  <TR> <TD> Wald </TD> <TD align="right"> 0.666108 </TD> <TD align="right"> 0.018919 </TD> <TD align="right"> 0.018919 </TD> </TR>
  <TR> <TD> Wald + CC </TD> <TD align="right"> 0.666108 </TD> <TD align="right"> 0.019128 </TD> <TD align="right"> 0.019128 </TD> </TR>
  <TR> <TD> Agresti-Coull </TD> <TD align="right"> 0.665841 </TD> <TD align="right"> 0.018908 </TD> <TD align="right"> 0.018908 </TD> </TR>
  <TR> <TD> Arcsine </TD> <TD align="right"> 0.666108 </TD> <TD align="right"> 0.019048 </TD> <TD align="right"> 0.018780 </TD> </TR>
  <TR> <TD> Wilson </TD> <TD align="right"> 0.665841 </TD> <TD align="right"> 0.018906 </TD> <TD align="right"> 0.018906 </TD> </TR>
  <TR> <TD> Bayesian </TD> <TD align="right"> 0.665969 </TD> <TD align="right"> 0.019039 </TD> <TD align="right"> 0.018775 </TD> </TR>
   </TABLE>
  
  
Adding the Bayesian approach to the Frequentist ones gives no additional insight. Essentially all we get is a little shrinkage toward the prior, which, with this much data, contributes very, very little. (Unless we were to use a stupidly strong prior.)

## Slightly more complicated models

OK, so this is basically a tedious exercise in estimation and inference methods and the results (in terms of take-away message) are all the same. This shouldn't be a surprise though since there is ample data and repeated heterogeneous measurements. But there are additional variables on hand that have not been taken into account like species, country, and year. So they should be used to investigate a little further.

First, remove the single case outbreaks.  These will muck up the inference since they are perfectly fatal or non-fatal and contribute very little to the population-based mortality rate which is the real object of interest here. Then take a look at the outbreak-wise mortality rates graphically. The dashed vertical lines represents the overall mortality rate.


```r
ebo02 = subset(ebo01, Cases > 1)
ebo03 = ddply(ebo02, .(Cases, Deaths), mutate, postmean = bbpost(Deaths, Cases, 1, 1)$postmean, postleft = bbpost(Deaths, Cases, 1, 1)$postminus, postright = bbpost(Deaths, Cases, 1, 1)$postplus)
ebo04 = data.frame(arrange(ebo03, desc(postmean)), OutbreakID = factor(1:dim(ebo03)[1]))
dd = ggplot(ebo04, aes(x = OutbreakID, postmean, ymin = postmean - postleft, ymax = postmean + postright, Species, Country))
dd + geom_pointrange(size = 1.5) + coord_flip() + ylim(c(0,1)) + theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Mortality rate") + xlab("") + ggtitle("Estimated mortality rate for each recorded outbreak\n with 95% credibility intervals") + geom_hline(yintercept = bbpost(sum(ebo04$Deaths), sum(ebo04$Cases), 1, 1)$postmean, linetype = 2)
```

![plot of chunk unnamed-chunk-10](./ebola_files/figure-html/unnamed-chunk-10.png) 

Now take a look at the outbreak-wise mortality rates by species of the Ebola virus and outbreak country of origin.


```r
dd + geom_pointrange(aes(colour = Species), size = 1.5) + coord_flip() + ylim(c(0,1)) + theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Mortality rate") + xlab("") + ggtitle("Estimated mortality rate for each recorded outbreak\n with 95% credibility intervals") + geom_hline(yintercept = bbpost(sum(ebo04$Deaths), sum(ebo04$Cases), 1, 1)$postmean, linetype = 2)
```

![plot of chunk unnamed-chunk-11](./ebola_files/figure-html/unnamed-chunk-111.png) 

```r
dd + geom_pointrange(aes(colour = Country), size = 1.5) + coord_flip() + ylim(c(0,1)) + theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Mortality rate") + xlab("") + ggtitle("Estimated mortality rate for each recorded outbreak\n with 95% credibility intervals") + geom_hline(yintercept = bbpost(sum(ebo04$Deaths), sum(ebo04$Cases), 1, 1)$postmean, linetype = 2)
```

![plot of chunk unnamed-chunk-11](./ebola_files/figure-html/unnamed-chunk-112.png) 

In order to look at the mortality rate through time the data must be aggregated into yearly summaries.


```r
ebo05 = ddply(ebo04, .(Year), summarize, Deaths = sum(Deaths), Cases = sum(Cases))
ebo06 = ddply(ebo05, .(Cases, Deaths), mutate, postmean = bbpost(Deaths, Cases, 1, 1)$postmean, postleft = bbpost(Deaths, Cases, 1, 1)$postminus, postright = bbpost(Deaths, Cases, 1, 1)$postplus)
ebo07 = arrange(ebo06, Year)
gg = ggplot(ebo07, aes(x = Year, postmean, ymin = postmean - postleft, ymax = postmean + postright))
gg + geom_pointrange(size = 1.5) + ylim(c(0,1)) + ylab("Mortality rate") + xlab("") + ggtitle("Estimated yearly mortality rate\n with 95% credibility intervals") + geom_hline(yintercept = bbpost(sum(ebo07$Deaths), sum(ebo07$Cases), 1, 1)$postmean, linetype = 2)
```

![plot of chunk unnamed-chunk-12](./ebola_files/figure-html/unnamed-chunk-12.png) 

The plot with color by country tells us little without additional information and the temporal plot doesn't seem to display any strong trends.  But the most interesting is the one that shows outbreak mortality by Ebolavirus species.  It seems that some species are more fatal than others.  This will be checked in two ways: using three seperate models (one for each species) and a Bayesian hierarchical model. To do this the data must be aggregated by species.


```r
ebo08 = ddply(ebo04, .(Species), summarize, Deaths = sum(Deaths), Cases = sum(Cases))
ebo09 = ddply(ebo08, .(Cases, Deaths), mutate, postmean = bbpost(Deaths, Cases, 1, 1)$postmean, postleft = bbpost(Deaths, Cases, 1, 1)$postminus, postright = bbpost(Deaths, Cases, 1, 1)$postplus)
hh = ggplot(ebo09, aes(x = Species, postmean, ymin = postmean - postleft, ymax = postmean + postright))
hh + geom_pointrange(aes(color = Species), size = 1.5) + coord_flip() + ylim(c(0,1)) + theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Mortality rate") + xlab("") + ggtitle("Estimated mortality rate for each recorded outbreak\n with 95% credibility intervals") + geom_hline(yintercept = bbpost(sum(ebo09$Deaths), sum(ebo09$Cases), 1, 1)$postmean, linetype = 2)
```

![plot of chunk unnamed-chunk-13](./ebola_files/figure-html/unnamed-chunk-13.png) 

Since this figure is really just a summary of nine numbers, it's probably better to just present it as a table.


```r
print(xtable(ebo09, digits = 6), type = 'html', include.rownames = F)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Sat Oct 18 21:17:19 2014 -->
<TABLE border=1>
<TR> <TH> Species </TH> <TH> Deaths </TH> <TH> Cases </TH> <TH> postmean </TH> <TH> postleft </TH> <TH> postright </TH>  </TR>
  <TR> <TD> Bundibugyo </TD> <TD align="right">      66 </TD> <TD align="right">     206 </TD> <TD align="right"> 0.322115 </TD> <TD align="right"> 0.061652 </TD> <TD align="right"> 0.064892 </TD> </TR>
  <TR> <TD> Sudan </TD> <TD align="right">     425 </TD> <TD align="right">     791 </TD> <TD align="right"> 0.537201 </TD> <TD align="right"> 0.034761 </TD> <TD align="right"> 0.034584 </TD> </TR>
  <TR> <TD> Zaire </TD> <TD align="right">    1096 </TD> <TD align="right">    1386 </TD> <TD align="right"> 0.790346 </TD> <TD align="right"> 0.021797 </TD> <TD align="right"> 0.021005 </TD> </TR>
   </TABLE>



## Reference and Function Definition

[^Christensen2010]:Christensen, R., Johnson, W., Branscum, A., & Hanson, T. E. (2010). Bayesian Ideas and Data Analysis: An Introduction for Scientists and Statisticians. CRC Press.


```r
bbpost = function(xx, nn, priora, priorb, alpha = 0.05){
  
  ## calculates the posterior distribution of the Beta-Binomial model
  
  ## Inputs:
  ##  xx: the number of "successes"
  ##  nn: the number of trials
  ##  priora: Beta parameter a, must be greater than 0 
  ##  priorb: Beta parameter b, must be greater than 0 
  ##  alpha: posterior credible interval threshold, must be in (0,1)
  
  ## Output is a list with members named:
  ##  postmean: the posterior mean
  ##  postminus: the left credible interval
  ##  postplus: the right credible interval
  
  posta = priora + xx ## posterior beta parameter a
  postb = priorb + nn - xx ## posterior beta parameter b
  postmean = posta/(posta + postb)
  postminus = postmean - qbeta(alpha/2, posta, postb)
  postplus = qbeta(1 - alpha/2, posta, postb) - postmean

  return(list(postmean = postmean, postminus = postminus, postplus = postplus))
  
}
```
