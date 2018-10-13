# Yellow Dog Word Predictor

A word suggestion system based on an n-gram model. Created as the Data Science 
specialization (Johns Hopkins University, Coursera) final project.

Developed by Ivan Lysiuchenko in September-October, 2018.

## Overview

This is a collection of R scripts which can be used to train an n-gram model,
obtaining a tool able to predict the next word based on the user input.

Also you can find some functions allowing to evaluate the n-gram model 
performance on a testing set: accuracy and perplexity.

Once an n-gram model is built, you can run the prediction algorithm as part
of a web (Shiny) application.

Designed to work with English input.

## Details

For more information on building the model, its validation and deployment,
see docs/using.html on your local copy or online:
[https://ivanlis.github.io/yellow_dog_predictor/using.html](https://ivanlis.github.io/yellow_dog_predictor/using.html)

Try the Shiny app at [https://ivanlis.shinyapps.io/yellow_dog/](https://ivanlis.shinyapps.io/yellow_dog/)

## License

Released under the terms of the 3-clause BSD license. See COPYING.txt.

## References

1. D. Jurafsky, J. Martin. Speech and language processing. Draft of August 7, 2018. Chapter 3.
2. S. Katz. Estimation of probabilities from sparse data for the language model component of
a speech recognizer. IEEE Transactions on Acoustics, Speech and Signal Processing, 1987. 
3. K. Welbers, W. van Atteveldt, K. Benoit. Text Analysis in R. Communication Methods and
Measures, 2017.
4. D. Attali. Shiny tips & tricks for improving your apps and solving common problems.
[https://github.com/daattali/advanced-shiny](https://github.com/daattali/advanced-shiny) 
