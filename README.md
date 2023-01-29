At the moment, don't bother to readme.

Using this as a scratchpad for notes about problems and progress.

https://lhbg-book.link

so, found a nifty link to a nicely paced haskell blog generator. The thinking is do it in agda. 

https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

i like the tutorial above, nice to get things started - i think i'm blurring representation of a tree and the printing of the tree.

not sure how to encode constraints like, an html tag has one head and one body tag. types are obvious, but i don't know if i wanna try to capture everything in that structure.

the tutorial will move to parsing markdown soon enough. Then there's stuff like navigation that spans multiple documents, that maybe the printer should no about, but the docs themselves don't.

https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=be300dd04c12de85b1326f3b37ea127aa26e9ef7

so, html doesn't have a formal grammar. But i could generate and parse a subset. There are some weird questions around context. site navigation is embedded in the html, so the doc printer needs to know more than the doc itself. I also have questions about semantics in various contexts html needs more "stuff" than markdown. so, like, md -> md-parse-tree -> html-parse-tree -> html. That seems weird, but i suppose there is no canonical representation (just look at these random thoghts smeared into a text file).

https://gallais.github.io/pdf/agdarsec18.pdf

