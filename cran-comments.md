## Submission type

This is a new release.

## Test environments

* local macOS (R 4.5.1): 0 errors | 0 warnings | 0 notes
* win-builder R-devel (R 4.6.0 RC): 0 errors | 0 warnings | 1 note
* win-builder R-release (R 4.6.0 RC): 0 errors | 0 warnings | 1 note

## R CMD check results

0 errors | 0 warnings | 1 note

The single note on win-builder consists of:

* "New submission" — expected for a first-time submission.

* "Possibly misspelled words in DESCRIPTION" flagging:
  - Arun, CaoJuan, Deveaud, Griffiths, Murzintcev — author surnames
    from the cited literature
  - DocumentTermMatrix — class name from the 'topicmodels' package
  - ldatuning — package name (the deprecated package this one replaces)
  - et, al — standard abbreviation in academic citations

  All are spelled correctly and are tracked in inst/WORDLIST.

## Downstream dependencies

There are no downstream dependencies (first submission).