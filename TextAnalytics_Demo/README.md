
### Objectives

This workshop covers how to perform common text analysis and natural language processing tasks using R.  Contrary to a belief popular among some data scientists, when used properly, R is a fast and powerful tool for managing even very large text analysis tasks.  

The course comprises six mini "modules", each consisting of guided instruction in the form of presentation of methods followed by student implementation of those methods prior to advancing to the next module.   Only after we have completed the practical exercises will we advance to the next stage.  

We will cover how to format and input source texts, how to structure their metadata, and how to prepare them for analysis.  This includes common tasks such as tokenisation, including constructing ngrams and "skip-grams", removing stopwords, stemming words, and other forms of feature selection.  We show how to: get summary statistics from text, search for and analyse keywords and phrases, analyse text for lexical diversity and readability,  detect collocations, apply dictionaries, and measure term and document associations using distance measures.  Our analysis covers basic text-related data processing in the R base language, but most relies on the “quanteda” (https://github.com/kbenoit/quanteda) package for the quantitative analysis of textual data.  We also cover how to pass the structured objects from quanteda into other text analytic packages for doing topic modelling, latent semantic analysis, regression models, and other forms of machine learning.



### Prerequisites

While it is designed for those who have used R in some form previously, expertise in R is not required, and even those with no previous knowledge of R are welcome.

### Reading materials

Designed to be done before the course or after, to augment what is presented during the course.  These are just suggestions -- no reading before the course is required.

*  [Sanchez, G. (2013) Handling and Processing Strings in R Trowchez Editions. Berkeley, 2013.](http://www.gastonsanchez.com/Handling and Processing Strings in R.pdf)  
*  [**stringi** package page](http://www.rexamine.com/resources/stringi/), which also includes a good discussion of the [ICU library](http://site.icu-project.org)  
*  Some guides to regular expressions: (Zytrax.com's User Guide)[http://www.zytrax.com/tech/web/regex.htm]
 or the comprehensive resources from http://www.regular-expressions.info  
*  See the [`quanteda` tag on Stack Overflow](http://stackoverflow.com/questions/tagged/quanteda), where you can pose questions and see some brilliant answers by our development team.


### Schedule

The course will taught interactively, as a series of "mini-modules" consisting of presentations of different aspects of quantitative text analysis using R, followed by practical exercises. 


### Module 0: Installation and setup of R and relevant packages

*  [CRAN](https://cran.r-project.org) for downloading and installing R
*  [GitHub page for the **quanteda** package](https://github.com/kbenoit/quanteda)
*  Additional packages to install:  STM, topicmodels, glmnet
*  **Exercise:**  Try running this RMarkdown file: [test_setup.Rmd](0_setup/test_setup.Rmd).  If it builds without error and looks like [this](http://htmlpreview.github.com/?https://github.com/kbenoit/ITAUR/blob/master/0_setup/test_setup.html), then you have successfully configured your system.

### Module 1: Overview and demonstration of text analysis using R

*  Demonstration of text analysis using R: [demo.R](1_demo/demo.R)
*  [Overview, motivation, and philosophy of the **quanteda** package](1_demo/motivation.pdf) (pdf slides)
*  **Exercise:** [Step through execution of this example code `workflow.Rmd`.](1_demo/workflow.Rmd)

### Module 2: Basic text data types and functions for text

*  [Basic text manipulation using R](http://htmlpreview.github.io/?https://github.com/kbenoit/ITAUR/blob/master/2_text_manipulation/text_manipulation.html)
*  **Exercise:** Step through execution of the [.Rmd file](2_text_manipulation/text_manipulation.Rmd).


### Module 3: Getting textual data into R

*  [Getting textual data into R](http://htmlpreview.github.com/?https://github.com/kbenoit/ITAUR/blob/master/3_file_import/file_import.html)
*  **Exercise:** Step through execution of the [.Rmd file](3_file_import/file_import.Rmd).
*  Sample data files: [SOTU_metadata.csv](https://github.com/kbenoit/ITAUR/blob/master/data/SOTU_metadata.csv), [inaugTexts.csv](https://github.com/kbenoit/ITAUR/blob/master/data/inaugTexts.csv), [tweetSample.RData](https://github.com/kbenoit/ITAUR/blob/master/data/tweetSample.RData)


### Module 4: Processing and preparing texts for analysis

*  [Text processing in R](http://www.kenbenoit.net/files/preparingtexts.html)
*  **Exercise:** Step through execution of the [.Rmd file](4_preparing_texts/preparingtexts.Rmd).


### Module 5: Descriptive analysis

*  [Descriptive analysis of texts](http://htmlpreview.github.com/?https://github.com/kbenoit/ITAUR/blob/master/5_descriptive/descriptive.html)
*  **Exercise:** Step through execution of the [.Rmd file](5_descriptive/descriptive.Rmd).


### Module 6: Advanced analysis and working with other text packages

*  [Advanced analysis and working with other packages](http://htmlpreview.github.com/?https://github.com/kbenoit/ITAUR/blob/master/6_advanced/advanced.html)
*  **Exercise:** Step through execution of the [.Rmd file](6_advanced/advanced.Rmd)
*  **Twitter analysis example**, and the instructions for setting up your own Twitter app, in [Twitter.Rmd](6_advanced/Twitter.Rmd). 


### Module 7: Tell us about your problems

*  This session is intended for students to describe their own challenges and for the instructors to describe how to solve them.  If you have some data you'd like us to work on live, as part of our interactive answers to your problems, you are encouraged to put them somewhere that can be accessed online, so that we will be able to access them in the class.




