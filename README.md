# rulesgen
SCIgen-inspired program for generating random text matching a grammar.

## Building and running
rulesgen is written in [Haskell](http://haskell.org/). You can build it from source using [Stack](http://haskellstack.org/).

Run the program by giving an input file on the command line:

    rulesgen rules.txt

## File format
An input file consists of a list of _rules_, which are productions in a [context-free grammar](http://en.wikipedia.org/wiki/Context-free_grammar). A rule looks like this:

    name=text text text text text

Rules are separated by line breaks, and blank lines are ignored.

### Left-hand sides
The left-hand side of a rule is a nonterminal. **When rulesgen runs, it looks for a nonterminal named `Start` (case-sensitive)**, then recursively expands from there.

Multiple rules can appear with the same left-hand side, which give separate productions for that nonterminal. When rulesgen needs to expand a nonterminal, it will select a random production out of the ones given for that nonterminal. You can bias the rate at which a particular production is selected by inserting `*N` just before the equals sign, where `N` is a positive integer. This makes the production `N` times as likely to be selected.

    foo*10=very probable
    foo=not very probable

To decrease redundancy, **if a nonterminal has multiple productions, rulesgen will never select the same production twice in a row when expanding that nonterminal**. You can improve the quality of your generated output by constructing your rules files to exploit this.

### Right-hand sides
The right-hand side of a rule is a list of terminal characters interspersed with nonterminals. To insert a newline character as a terminal on the right-hand side of a rule, use the escape sequence `\n`. To insert a backslash character, use the escape sequence `\\`.

Nonterminals on the right-hand side are enclosed by percent signs:

    Start=Somewhere over the %object%...
    object=rainbow
    object=hedge

When percent signs are used around a nonterminal, rulesgen generates a fresh value each time in the randomized fashion described above. Sometimes, though, it is more desirable to get the same generated value repeatedly. If you enclose a nonterminal in at signs instead of percent signs, rulesgen will _bind_ the nonterminal to a generated value and return the same string every subsequent time that nonterminal is used enclosed in at signs:

    Start=@word@ @word@ @word@ @word@ @word@
    word=Badger
    word=Malkovich

### Preprocessing
rulesgen runs a variant of the [C preprocessor](http://en.wikipedia.org/wiki/C_preprocessor) on the input file before parsing it. This provides several useful features:
* You can use both multiline `/* ... */` comments and single-line `//` comments in your input files. (Avoid placing comments on the same line as a rule, since comments are replaced with whitespace by the preprocessor and whitespace is significant in rules.)
* You can use preprocessor directives such as `#include` and `#ifdef` to modify your rules before they are submitted for generation. This gives a simple mechanism for constructing parameterized rules files: write a library file with rules missing, then `#include` that file and fill in the gaps in another file.
