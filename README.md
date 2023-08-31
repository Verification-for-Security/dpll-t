# Assignment 2: DPLL(T)

In this assignment, you will implement an SMT solver over the theory of Linear
Rational Arithmetic (LRA). For the most part, this assignment consists of 
implementing an SAT solver using the DPLL algorithm. When your SAT solver is 
fully functioning, you can extend it into an SMT solver by incoporating a 
theory solver. In this case, one over LRA using the Simplex method.

## Running and testing

This code again features a test bench, which you may run in the same fashion
with `stack`. The tests again aim to direct you through to code base of this
assignment and we strongly suggest you follow this!

This assignment also features an executable which may be ran with the following
command.

```
$ stack run
```

By default, this will run the full SMT solver. This will only work once the
entire assignment is complete. We do allow you to modify `Main` to run only
a part of solver. This can be useful when you wish to test a larger sequence
of code. The executable code for a Haskell project can generally be found in
the `app` folder.

## Assignment Structure

In this assignment, we provide you with a skeleton which requires a number of
stubs to be implemented. Every stub contains some explanation of what it is 
supposed to do. Read this carefully!

## SAT solver

For the SAT solver, you essentially need to implement three things.

1. Conversion from proposition into Conjunct Normal Form (CNF)
2. Tseitins Transformation to create an equisatisfiable CNF
3. The DPLL algorithm, including BCP and PLE

### Propositions

You may notice that there are two datastructures that represent a proposition.
The first being `Prop a`, which is in an unconstrained form. When compared to
the second form, `CNF a`, you'll quickly see that the latter structure strictly
allows for propositions only in CNF. The nice thing about this is that it saves
us the tedious (and slow) task of checking whether the structure is in CNF when
checking satisfiability!

Before implementing the CNF conversion, try to understand both datastructures!
Your first tasks will be to implement the conversion from `Prop a` to `CNF a`.

## SMT solver

After you've implemented to SAT solver, you will extend it to an SMT solver.
This requires you to implement the following two things.

1. Conversion from LRA to a format supported by the theory solver (Simplex)
2. The DPLL(T) algorithm

Notice that you will not have to implement the theory solver itself.

## Grading

Your final grade corresponds directly to the one awarded to you by the test
infrastructure. Do make sure your submission correctly executes on our online
environment.

If there are issues with the submission system, don't panic! We will handle this
on a case-by-case basis.

If your uploaded submission somehow fail tests that work locally, ping
us and we will have a look!

If the online environment suddenly fails to work moments before the deadline,
don't hesitate to send us your submission through different means (e.g. email).

## Plagiarism

We have a strict zero tolerance policy against plagiarism. Sadly, we find cases
every year... This is not fun for you, nor us. Please, refrain from copying 
and/or sharing your code with other groups.

Since this is a group assignment, we expect that most of you will work together
via Git. Do make sure to make your repository **private**! Sharing your code in
this manner is sadly still plagiarism, even if unintentional.

