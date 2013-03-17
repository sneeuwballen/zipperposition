## Todos for making a lib

- add a 'a field to context; use ProofState.t ctx as main argument everywhere
- 'a context should contain:
    * selection function
    * the ord
    * the precedence 
    * hashconsing tables
    * a mutable pointer to some object of type 'a (proof state+config+calculus?...)
- functional ord and precedence (immutable record) with combinators
- combinators for clauseQueue and selection functions
- immutable term indexing (path indexing or substitution trees)
- most main functions should only take a ctx, and optional args
