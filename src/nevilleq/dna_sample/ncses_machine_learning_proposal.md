Machine Learning Techniques
================
DSPG Business Innovation Team
7/15/2019

Here, we propose three models for classifying DNA articles, to identify whether or not they describe a product innovation as defined in the Oslo Manual (2018). For all of these proposed models, we may extend the frameworks to include non-binary classification; such as a model to classify into ordinal categories "No Innovation", "Moderate Innovation", "High Impact Innovation"/"Disruptive Innovation", etc. Further, we explicate the general synopsis of how each model works, each model's strengths and weaknesses, and translate this into the context of our specific problem.

1. Bag of Words (BoW)
---------------------

This type of model, used in both Image Classification and Natural Language Processing, is a relatively easy to implement and theoretically straight-forward model. After removing stop words (things like "the", "and", "a", etc.), we first take the first article's text body. From there, we tokenize each individual word found in the document, such that if a word shows up twice, we associate a 2 count with that word. If it only showed up once, we give it a 1, and 0 if does not appear at all. We now have what's called a *bag of words*. This has reduced an article's text body in a matrix of individual word keys and their associated counts (a long vector of numbers). Then, we repeat this process for all documents in the data, combining these all together to get a large matrix with observations being key words and columns being the documents (or vice versa). We then perform one of a variety of clustering algorithms, semi-supervised machine learning to utilize the underlying structure of the data to classify portions of it into groups (2, 3, 4, ...). These are then analyzed to decide which split is associated with your outcome of interest.

For our purposes, we are going to generate a DNA bag of words and cluster into two groups, one describing innovation and the other not. We can always extend this further to more ordered groups as mentioned above. The advantages of this model are that it is *relatively* easy computationally, interpretable in a sense (because it is semi-supervised), and can be repeated many times for validation of our results (sensitivity analysis, diagnostics, etc.). The main drawback is that since we are really just counting word frequency, this model does not take into account the order or relationship amongst/between words and sentences. This can be a really huge drawback if the key words describing innovation are actually longer phrases (words consistently occurring a specific order) rather than individual words.

[Citation / Methodology Details] 
Text Representations for Text Categorization: A Case Study in Biomedical Domain https://www.academia.edu/32161163/Text_representations_for_text_categorization_a_case_study_in_biomedical_domain 

2. Support Vector Machines (SVM)
--------------------------------

The underlying structure of support vector machines relies heavily on some very complicated multi-dimensional geometry, but at its core it also utilizing the underlying structure of the data along with geometric distances between articles to predict a numeric or classification outcome. The difference between the BoW and SVM is mainly that BoW utilizes a semi-supervised structure while SVM is entirely supervised, relying on a specific outcome (innovation), rather than a cluster associated with innovation. The main advantage of this algorithm is accuracy, it well known to be very precise and robust between training (model building) and test (new, untouched) data. The biggest drawback is computational speed and memory, it is a very complicated and large process, requiring a long time for optimization and validation. So for larger data this can become very slow. Also this relies on an actual outcome, i.e., that innovation/no innovation has already been labeled for each article. 

[Citation / Methodology Details]
Text Categorization with Support Vector Machines: Learning with Many Relevant Features (Thorsten Joachims)
https://www.cs.cornell.edu/people/tj/publications/joachims_98a.pdf

3. Neural Nets
--------------

Much like SVM, Neural Nets also rely an a defined/labeled article (innovation/no innovation). However, unlike SVM which is precise but rigidly defined geometrically, Neural Nets are extremely flexible and often useful when the data lacks a well-defined structure. At their core, much like a brain, the generate a series of nodes and evaluate associated connections between words and articles, generating a web of associations which is very flexible and powerful. In our context, we have both unstructured data and data which is stratified in differently unstructured ways. The main advantage of these types of models is their flexibility, which may be very useful for our specific DNA data. The main drawback however, as usually comes with flexibility, is that it can pick up words/phrases in the article that are actually unrelated to our outcome and we would be woefully unaware of this fact, due to the almost entirely unsupervised nature of Neural Nets. This means that we give the algorithm input articles and innovation outputs to build on, then new articles to predict the outcome of innovation, but it is generally impossible for us to know exactly how this is being done as we cannot view the whole network in its entirety or evaluate the associations it believes are meaningful. So while we may evaluate it for accuracy, we cannot generally tell if it is really working the way it should/the way we want it to. As a result, we may be able to obtain very accurate models on a training set but we can’t be completely sure it would work the same way in a real world scenario.

[Citation / Methodology Details]
Recurrent Convolutional Neural Networks for Text Classification- Lai, et al
https://www.aaai.org/ocs/index.php/AAAI/AAAI15/paper/download/9745/9552

