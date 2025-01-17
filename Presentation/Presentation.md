Next Word Prediction App
========================================================
author: Luis Salazar
date: August 17th, 2015

Overview
========================================================

The aim of this project is to make a next word predictor
based on the user input, similar to how SwiftKey predicts
the next word for iOS and Android devices.

The engine relies on a sampled corpus from [HC Corpora]
(http://www.corpora.heliohost.org), which contains entries
from blogs, news, and twitter. The corpus is cleaned
to remove non-graphical characters, numbers, punctuations,
tags, URIs, and unnecessary spaces.

The application is hosted [here]
(https://lmsv-mx123.shinyapps.io/next-word-app)

The repository for the capstone project is [here]
(https://github.com/lmsv-mx123/DSCapstone)

The application
========================================================

The developped application attempts to predict the next
word based on what the user inputs, using an approach
of the most frequent terms that can follow previous input.

In order to use the application, the user inputs a sentence
and presses "Next Word" so that the top ranked next word is
shown. As the top ranked word may not be precisely
the seeked word, for instance if it the input is "hello"
the top word could be "world" but the user may be seeking
instead "to", additional top words are shown as suggestions.

The app functions by grabbing the user input and cleaning
it. A search is then done on n-grams matching previous
terms. The user is finally given the predicted next word
and suggested alternatives.

Algorithm
========================================================

In order to predict the next word, the application uses
a variant of Katz Backoff model, i.e. for a given cleaned
sentence:
* Search for phrases, starting from the highest n-gram
that match n-1 previous terms.
* Go back one degree and search for more phrases matching
previous terms. If there were phrases found on higher order
n-grams, the normalized ocurrence of the phrases are adjusted
by a factor of 0.3. This step is repeated, decreasing the
factor by 0.3 each time.
* For the proposed phrases, the last term (suggested)
is kept and to remove dupplicates, terms are aggregated.
* The top terms are then presented to the user.

Results & Enhancements
========================================================

Searching with strings such as "When you were in Holland you
were like 1 inch away from me but you hadn't time to take a",
the predicted next word is "[1]:picture" and the suggested
alternative words are "[2]:few [3]:look [4]:break [5]:step"
which is quite good.

If previous terms are not found, the suggested terms come
from unigrams which may not necessarily go well.

Some of the possible enhancements for the app are:
- Re-structure the sampled corpus so that infrequent
terms are replaced by UNK and build the ngrams from there.
- Add a category to predict based on context and have a
much larger corpus to build up to octagrams.
