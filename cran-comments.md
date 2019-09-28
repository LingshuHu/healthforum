## Resubmission
This is a resubmission. In this version I have:

* replaced dontrun{} with donttest{}.

## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

In response the comments: 

> You write information messages to the console that cannot be easily 
suppressed. Instead of print()/cat() rather use message()/warning()  or 
if(verbose)cat(..) if you really have to write text to the console.
(except for print() and summary() functions)
\dontrun{} should be only used if the example really cannot be executed 
(e.g. because of missing additional software, missing API keys, ...) by 
the user. That's why wrapping examples in \dontrun{} adds the comment 
("# Not run:") as a warning for the user.
Please unwrap the examples if they are executable in < 5 sec, or replace 
\dontrun{} with \donttest{}.

We replaced cat() function with message() and replaced dontrun{} with donttest{}. 
