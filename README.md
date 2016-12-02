# Regulations Parser

This project parses the XML regulations from http://www.federalregister.gov and turns them into structured XML files
which conform to the CFPB's [RegML schema](https://github.com/cfpb/regulations-schema). Right now, only the "initial"
notices are supported; what this means is that the first time an agency issues a regulation, it posts the full new
regulation, but subsequent notices only indicate modifications of the previous version, and only that full new version
is supported by the parser. Support for notice updates is pending.

# Usage

`regparser` is `quicklisp`-loadable; I suggest making a symlink from your `local-projects` directory to wherever you
checked out this repo. Using your favorite REPL, do:

`(ql:quickload :regulations-parser)`

If all goes well, `quicklisp` will load all the project dependencies compile the project itself. If all does not go well,
check to see if your Lisp is using `UTF-8` as the default encoding for reading input files. This is usually the case on
Linux with SBCL, but on Macs running SBCL, the default seems to be ASCII. You can change this by evaluating

`(setf sb-impl::*default-external-format* :utf-8)`

assuming you are running SBCL. Other Lisps should have similar facilities.

If everything has loaded properly, you can now run the parser. The entry point to doing so is the `parse-reg-to-xml`
function, which takes as its arguments the file to be parsed (that's whatever you got from the Federal Register), the
document number (a unique string used by the FR to identify the notice), and the output file, which should have an
`.xml` extension. Keep in mind that the first argument is not a string but a pathspec, and if you pass a string, you'll
probably get a weird error. Your call should look something like this (using the `time` macro for timing):

```
(time
 (regparser::parse-reg-to-xml #p"/path/to/regparser-lisp/2011-31715.xml"
 "2011-31715"
 "/path/to/regparser-lisp/out.xml"))
```

and the result will look something like this:

```
Evaluation took:
 100.795 seconds of real time
 101.253473 seconds of total run time (92.306040 user, 8.947433 system)
 [ Run times consist of 29.511 seconds GC time, and 71.743 seconds non-GC time. ]
 100.45% CPU
 56 forms interpreted
 158 lambdas converted
 281,582,924,310 processor cycles
 12 page faults
 124,678,227,440 bytes consed
```

Congrats, you have parsed the first notice of Regulation Z in under two minutes. Feel free to take a look at `out.xml`
and validate it relative to the `eregs.xsd` schema linked above. It *should* validate, but there may be a few minor
mistakes that I have yet to correct. For the most part it will be valid, though.

# Future work

This code parses Regulations Z and E, issued by the CFPB, as test cases. As these were the original regs that the
Pythonic version of the regulation parser was validated on, I think that's pretty solid. What this does *not* do
is parse the subsequent updates to those regulations via notices. That is in the works and I hope to add that capability
in the near future. I should also note that this parser generally strives for adequacy over perfection. For example,
it does not do any kind of clever NLP to pick out definitions; it just looks at a few patterns that indicate that
something is being defined here (e.g. "foo means bar"). This suffices for 90% of cases, and the other 10% are more
easily fixed in "post-production" than coded into the grammar.

# Issues

If something breaks, please leave an issue. The code as presented is functional but obviously not mistake-free by
any means. The problem with parsing FR notices is that they tend to have inconsistencies in their XML depending
on who did the conversion, which notice, the phase of the moon, etc. Because the FR does not guarantee any sort
of consistent representation (unlike RegML, which does), it's impossible to promise that it will compile your
specific notice; it may not. However, if it does not, and you know why it failed, I'd appreciate it if you would
leave a note with an explanation.