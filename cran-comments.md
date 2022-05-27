### Resubmission

Comments from reviewer Gregor Seyer:

```
Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. 
Please write about the structure of the output (class) and also what the output means. 
(If a function does not return a value, please document that too, e.g. 
\value{No return value, called for side effects} or similar) 

Missing Rd-tags:
      set_display_digits.Rd: \value

Please ensure that you do not use more than 2 cores in your examples, vignettes, etc.

Please fix and resubmit.

Best,
Gregor Seyer
```
In this resubmission, I have added a return value to the function `set_display_digits()`, and
set the default number of cores used for parallel processing to `2`.


### R CMD CHECK

0 errors | 0 warnings | 1 note


#### Note 1

---

```
New submission

Possibly misspelled words in DESCRIPTION:
  AdjPIN (11:232)
  Duarte (11:243)
  Easley (11:128, 11:157, 11:306)
  Ersan (11:204)
  MPIN (11:195)
  Multilayer (11:179)
  VPIN (11:297)
  al (11:167, 11:316)
  et (11:164, 11:313)

```
Explanation: 

The presumably misspelled words are either:

- concepts, or abbreviations related to statistical models in Finance
(e.g. AdjPIN stands for Adjusted Probability of informed trading). This
also concerns AdjPIN, MPIN, Multilayer, and VPIN.

- Researcher names: Ersan, Duarte, Easley

- 'et', and 'al', together form 'et al.', which is an abbreviation 
meaning “and others.”; used to shorten lists of author names in text 
citations.
