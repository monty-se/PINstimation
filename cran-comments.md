## R CMD check results

### Local, using devtools::check()

---

SUCCESS!

-- R CMD check results ---------------------------------------------------------------------------------------------- PINstimation 0.1.0 ----
Duration: 12m 42.7s

0 errors √ | 0 warnings √ | 0 notes √

### GitHub Actions 

- macOS-latest (release)
- windows-latest (release)
- ubuntu-latest (devel)
- ubuntu-latest (release)
- ubuntu-latest (oldrel-1)

All SUCCESS!√

### Platform checks using rhub

There were no ERRORs or WARNINGs. 

There was 3 NOTES:

#### Note 1

---


   New submission
   
   Possibly misspelled words in DESCRIPTION:
     AdjPIN (10:232)
     Duarte (10:243)
     Easley (10:128, 10:157, 10:306)
     Ersan (10:204)
     MPIN (10:195)
     Multilayer (10:179)
     al (10:167, 10:316)
     VPIN (10:297)
     et (10:164, 10:313)
 
Explanation: The presumably misspelled words are either:

- concepts, or abbreviations related to statistical models in Finance
(e.g. AdjPIN stands for Adjusted Probability of informed trading). This
also concernts MPIN, Multilayer, VPIN.
- Researcher names: Ersan, Duarte, Easley
- 'et', and 'al', together form 'et al.', which is an abbreviation 
meaning “and others.”; used to shorten lists of author names in text 
citations.

#### Note 2

---

N  checking examples (11m 53.2s)

-  Examples with CPU (user + system) or elapsed time > 5s

-  generatedata_mpin   296.54   3.55  300.11
-  mpin_ml              95.77   1.64   97.41
-  mpin_ecm             54.00   0.77   54.77
-  get_posteriors       49.64   0.86   50.50
-  initials_pin_yz      39.56   0.59   40.16
-  initials_adjpin_cl   26.97   0.35   27.31
-  detectlayers         21.64   0.25   21.89
-  pin_yz               19.72   0.23   19.95
-  initials_mpin        27.49   0.46   27.95
-  adjpin               17.76   0.25   18.02
-  initials_adjpin_rnd   7.03   0.12    7.15
-  vpin                  9.16   0.44    9.59
-  set_display_digits   18.86   0.33   19.19
-   initials_adjpin       5.80   0.09    5.90
   
Explanation: Extensive examples have been included in order
to give to the user sufficient examples to know how to use 
the packages, especially the features, newly developed.

#### Note 3

---

N  checking for detritus in the temp directory
   Found the following files/directories:
     'lastMiKTeXException'
     
Exaplanation: Extensive online research, trying different
computers did not allow me to know the cause of the error.


