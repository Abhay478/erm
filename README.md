# Erm

Erm is a very simple crate made for the enum `erm`. It started out as a way to be a little less binary than the `Some` and `None` variants of `Option` while returning stuff from a function. So I added the variants `Maybe`, `MaybeNot`, and `DontKnow`, which I thought reasonably covered the spectrum of uncertainty you'd expect from a function.

I tried to implement as many methods of `Option` as I could for `Erm`, but some of them appear to be unstable or confined to language components.

Fortuitously, I realised an entirely different application for this enum *after* writing most of the first version of the crate.

# Randomised Algorithms

Randomised algorithms are algorithms that rely on some source of randomness during their computations, and are subsequently correct only with some probability. Famous randomised algorithms include the Miller-Rabin primality test and the randomised quicksort algorithm. 

## The RP class

Much like P and NP, which are classes of problems with deterministic and nondeterministic algorithms, RP is a class of problems for which a *probabilistic* algorithm exists that returns a result in polynomial time. While this algorithm can accept or reject, its rejections are perfect - if the algorithm rejects an input, we can be certain that the input is not in the language. The algorithm may, however, accept inputs that are not in the language with some probability less than half.

RP stands for "Randomized Polynomial time".

That's how I remember it, anyway - RP is perfect rejection. It's often confused with the coRP class.

The output of an RP algorithm could be `No` or `Maybe`.

## The coRP class

The coRP class is NOT the complement of the RP class. It's perfect *acceptance* - the class of problems with algorithms that accept only those inputs which are actually in the language, but might reject some of them too.

The output of a coRP algorithm could be `Yes` or `MaybeNot`.

## BPP and ZPP

Bounded Probabilistic Polynomial time (BPP) is the class of problems for which a *probabilistic* algorithm exists that returns a result in polynomial time, which is correct with probability greater than half. The output of a BPP algorithm could be `Maybe` or `MaybeNot`.

Zero-Error Probabilistic Polynomial time (ZPP) is a little different. Algorithms for ZPP are defined to output `Yes`, `No`, or `DontKnow` on all inputs. 

# AI Disclaimer

I do not use ChatGPT, Claude, Perplexity, or any online tool for code generation or assistance. I do use GitHub Copilot (you know what copilot is) for generating boilerplate code or making multiple identical code changes, never for complex logic, or any logic really. While it's excellent for assignments, I believe it cannot produce truly original content, whether in code or literature.
