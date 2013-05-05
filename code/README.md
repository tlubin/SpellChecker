Correctr: The Fast Autocorrect and Smart Spellcheck
============
Authors:
Todd Lubin, Wilson Qin, Peter Bang, Kristen Faulkner
A CS51 Final Project
============

QUICKSTART:
  1. Correctr is dependent on the following being installed on your computer:
    - OCaml, version 4.00.01 or greater.
    - Make
  2. To start, run:
      $make
      $./correctr

MODES:
  -Type menu() to return from inside a mode to the menu prompt to switch modes

  -[W] Word mode
          Input a word, and you will get a ranked output (highest to lowest) of all suggestions for corrections in the following format:
          
          Suggestion | Frequency Score | Keyboard Distance Score (truncated) | Aggregate Score (truncated)

          Note that at this time, Correctr will not make suggestions for non-alphabetic input.

  -[S] Sentence mode
          Input a sentence, and you will get an auto-corrected version of your sentence, based on Correctr's top ranked suggestion for each erroneous word detected. 

          Note that at this time, Correctr will clean all words with non-alphabetic input in this mode.

  -[F] File mode
          Input a filepath, and you will get notified of any alphabetic words in your file that may be spelling errors. The output will be of the format:

          (Line Number, Word Number) | Original Word | Top 3 Correctr Suggestions - comma separated

          Note that at this time, Correctr will clean all words with non-alphabetic input in this mode.

  -[H] Help mode
          Print instructions. (This file)
  
  -[Q] Quit Program


