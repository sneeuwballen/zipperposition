
- nettoyer le code.
  * avoir un second langage d'entrée qui permette de décrire les features
    au dessus (marre de TPTP), et qui soit accessoirement plus lisible.
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
