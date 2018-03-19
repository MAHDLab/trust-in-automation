# trust-in-automation
Analyzing experimental evidence on respondents' trust in automation to make political judgements

This is the wowrking code for analyzing our experiments on people's trust in automation as they complete our forecasting (IFP) challenges. Importantly, this represents a work in progress, so update the code in the `R` script file, `exp.R` accoringly.

Let me know if you have issues with the code/set up. See below for instructions on loading the data into your own environment.

```{R }
## Load the data from Ryan's (TIA) DB

experiment1 <- read.csv("C:/Users/Ryan/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 1/results01112018.csv", as.is = TRUE)
experiment2 <- read.csv("C:/Users/Ryan/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 2/results01112018.csv", as.is = TRUE)
experiment3 <- read.csv("C:/Users/Ryan/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 3/results01112018.csv", as.is = TRUE)
experiment4 <- read.csv("C:/Users/Ryan/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 4/results03052018.csv", as.is = TRUE)
```
