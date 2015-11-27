# TODO

- nettoyer le code.
  * avoir un second langage d'entrée qui permette de décrire les features
    au dessus (marre de TPTP), et qui soit accessoirement plus lisible.
  * entrée SMTlib
  * écrire un système de tests directement lié à Sledgehammer
- améliorer le support du polymorphisme (avec pleins de tests venant de
  Sledgehammer en entrée, ça devrait être naturel).
- ajouter une notion d'égalité définitionnelle, au moins pour les
  définitions non récursives de termes (et peut-être de proposition); on
  verra si ça intersecte avec le travail de David sur la
  super-superposition.
- ajouter une notion d'application currifiée (un genre de `@` infixe
  qui fait que les fonctions deviennent des constantes, et donc qu'on
  peut appliques des variables `x @ y`);
  le cas `f @ a1 @ ... @ an` avec `f` n-aire se réduit à `f(a1,...,an)`.
  En l'absence de λ on reste fondamentalement au premier ordre.
- ajouter une vraie notion de type inductif (co-inductif: je ne sais
  pas) et réfléchir à un remplacement/encodage du pattern matching. J'ai déjà
  quasiment toutes les briques de base sous la main, mais c'est laid
  et je voudrais un traitement efficace et général. En particulier,
  gérer correctement l'acyclicité et l'injectivité.
- nettoyer l'induction en la basant sur les types inductifs au dessus;
  possiblement aussi améliorer les heuristiques qui sont pour l'instant
  faibles.
- extensionalité (avec `@` on peut parler d'égalité entre fonctions)
- implèm complète d'AVATAR (geler les clauses, etc.)
  * peut être plus simple en virant l'aspect QBF (et en spécialisant sur Msat)

## Ideas

- merge definitional rewriting and demodulation
- how to deal with definitions:
  * Build a symbol dependency graph such that if `f x y := t[x,y]` is non-recursive,
    then `f -> g` is in the graph for every symbol `g ∈ t`. Then, a DFS on
    the graph gives us a _layer level_ for every symbol (it also breaks
    cycles arbitrarily), such that `f -> g` implies `level(f) ≥ level(g)+1`
    (except for cycles of course).
    `compare(f,g) -> compare(level(f), level(g))` is then a proper order for
    computing precedences.
  * For inductive symbols, `n := 0 | succ(n2)` could be dealt with this way,
    by using possibly negative levels and enforcing `level(n2) = level(n)-1`.
- AVATAR:
  * store locks in each clause, and keep a map `b_lit -> clauses it locks`
    for efficiently retrieving clauses that might be unlocked when this lit
    changes
  * store interpretation as a `b_lit set`
  * compute difference between current and previous interpretation, and
    check whether the corresponding clauses are unlocked.
  * keep locked clauses in term indices! Makes model changes less costly
    (maybe this should be an option, performance impact unclear)

