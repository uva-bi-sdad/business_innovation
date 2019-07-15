Labeling Techniques
================
DSPG Business Innovation Team
7/15/2019

Here, we present three proposed methods for the labeling the articles. As opposed to classifying the articles, labeling is actually a necessary first step to obtain "true" articles that can be reliably assessed to be indicating innovation or not. We need these "true" articles to serve as the outcome or output for the article data with which we build models for predicting innovation Y/N. Put simply, many machine learning techniques rely on a outcome in order to build the model and in order to have an outcome, we first need to label the articles.

Starting with over two million articles from 2010-2018, it is infeasible to label every or even a few thousand articles by hand (i.e. with human eyes and brains). So we have devised a few methods to obtain labels for these articles, some "by hand" and others "by computer". Here we present the methods, with a brief explication and evaluation of the advantages and disadvantages within and between methods.

1. Manual Labeling
------------------

### a. Internal Labeling

One possible way to label the articles would be extract the title, body, company, etc. and pass these documents around our office in a word .docx format, and manually label the articles as describing innovation Y/N as well as (potentially) the degree of innovation. This actually has a large advantage in the accuracy of our labels, as our office is familiar with the OSLO definition of innovation, as well as the concept of classification and the innovation with respect to the scope of our specific project. Thus, we would expect this to be the most accurate way to label the articles. However, this is clearly taxing on everyone in terms of time and limits severely the total number of articles we may actually label and use to build our initial models.

### b. Mechanical Turk

As an alternative to internally labeling the articles, we have designed a survey containing the articles and instructions for answering questions which will give us labels, key words, company indentification, etc. This survey can then be outsourced to a service, through Amazon, called Mechanical Turk. This is a platform which posts tasks, in our case the task of answering our survey, and offers them up to participants, affectionately called "Turkers", who self-select and complete tasks for a specific amount of money per task. This is a convenient work around to recieve labeled articles, without having to do it ourselves by hand. However, there are three main challenges associated with these method:

-   Designing an effective and good survey is not a simple task, requiring lots of planning, testing, reliability analysis, etc. This is both time consuming and may not lead to particularly accurate results/labels if the survey is not properly constructed.

-   Given the first issue, the cost of outsourcing the labeling process increases very quickly with this service. As a first run, we can probably afford to get 1000 articles labeled by this method. More than internal labeling but still less than we would prefer.

-   Given the first two issues, we must generate a sample of articles to actually be sent off and labeled. As we are beggining with 2 million articles and potentially filtering it down to about 150,000 or fewer "valid" artciles per year, taking a random sample of 1000 which is representative of the target population is also no simple task. Statistical rigor and stratified sampling most occur such that we can ensure our future models, based on these labels, are generalizable and useful to our specific aim.

2. Automated Labeling
---------------------

### a. Machine Learning to Label Articles

#### One-Shot Neural Nets

Advantages:

-   Allows for labeling of all valid articles

-   Can be more accurate than human labeling if accurately constructed

-   May be necessary to build ourselves, novel application and could be a significant step forward

-   Flexible and adaptable to changes (more than a single sample labeling exercise, can be repeated and )

Disadvantages:

-   Method proposed is relatively new and not extensively verified as a "best practice" labeling methodology

-   Without manual labeling verification, impossible to validate "true" accuracy of the labeling model

-   As an unsupervised method, also little to no information about the actually inner-workings of the model

### b. Outsourcing Automated Labels

##### Proprietary Software

-   Same advantages and disadvantages as above, with the exceptionally large additional disadvantage of us not knowing exactly how the software is doing the labeling. This may be an impassible obstacle, which may necessitate the use of the above method for machine labeling.
