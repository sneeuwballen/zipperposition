Tutorial
========

Some small tools are provided with ``Logtk``'s source code. They can be
found in ``src/tools/`` and have various dependencies (parsers,
meta-prover, etc.). Their source code can be helpful to see how some
specific tools (meta-prover, type-checking, parsing) are used.

Let's start with the basics: symbols, terms and types. We assume in the
following that the basic library has been linked and ``Logtk`` has
been opened, for instance in an ocamlfind-enabled toplevel with

.. code-block:: ocaml

   #require "logtk";;  (* load module *)

   open Logtk;;  (* brings the content of Logtk into the scope *)

Symbols, Terms and Types
------------------------

Symbols
^^^^^^^

Automated Theorem Proving belongs to *symbolic reasoning*. As the name
hints, all we are going to do is manipulating *symbols*. For this
``Logtk`` provides a `Symbol module <../Symbol.html>`_. A symbol
can be either a numeric constant, a *connective* or a [hashconsed]_ string:

.. code-block:: ocaml

   let f = Symbol.of_string "f";;
   let g = Symbol.of_string "g";;

   (* machine integer *)
   let twelve = Symbol.of_int 12;;

   (* big integer, here built from string *)
   let very_big_num = Symbol.mk_int (Z.of_string "99999999999999999999999");;

Some symbols are already defined because they are pervasive in logic.
We call those *connectives*. They are defined in
`Symbol.Base <../Symbol.Base.html>`_. For instance:

.. code-block:: ocaml

   (* logic equivalence *)
   let equiv = Symbol.Base.equiv;;

   (* logic "or" *)
   let or_ = Symbol.Base.or_;;

Like many other modules, ``Symbol`` defines many operators such
as equality, comparison, hashing and printing.

.. code-block:: ocaml

   (* use a custom printer for symbols *)
   #install_printer Symbol.fmt;;

   let a = Symbol.of_string "a";;
   let b = Symbol.of_string "b";;
   (* printed as "a" and "b" respectively *)

   Symbol.eq a a;;
   (* true *)

   let foo0 = Symbol.gensym ~prefix:"foo" ();;
   (* a symbol named "foo_0" or something like this *)

   let foo1 = Symbol.gensym ~prefix:"foo" ();;
   (* another symbol named "foo_1" *)

   Symbol.cmp foo0 foo1;;
   (* total ordering on symbols:
      -1 because the first is smaller (has been created before) *)

   Symbol.TPTP.rat;;
   (* a symbol named "$rat", representing the types of rationals in TPTP. *)


Types
^^^^^

In ``Logtk``, terms are always typed. Dealing with untyped logic only
means dealing with terms that all have the same (unique) type. The type system
is rank-1 polymorphism, à la ML (following the
`TFF1 draft <http://www.cs.miami.edu/~tptp/TPTP/Proposals/TFF1.html>`_).

The module `Type <../Type.html>`_ represents such polymorphic types. We can
build them, print them, etc. in pretty much the same way as symbols:

.. code-block:: ocaml

   (* useful in the toplevel only, to print types nicely *)
   #install_printer Type.fmt;;

   let ty1 =
     let open Type in
     const a <== [const a; var 0];;

   (* or, without the infix operator nor the .() syntax: *)
   let ty1' = Type.arrow_list [Type.const a; Type.var 0] (Type.const a);;

Let us examine closer the structure of types. It is exposed in
``src/base/type.mli`` as:

.. code-block:: ocaml

   type t = private ScopedTerm.t
   (** Type is a subtype of the general structure ScopedTerm.t,
       with explicit conversion *)

   type view = private
     | Var of int              (** Type variable *)
     | BVar of int             (** Bound variable (De Bruijn index) *)
     | App of symbol * t list  (** parametrized type *)
     | Fun of t * t            (** Function type (left to right) *)
     | Record of (string*t) list * t option  (** Record type *)
     | Forall of t             (** explicit quantification using De Bruijn index *)

   val view : t -> view

So, ``Type.t`` is actually a *private alias* to the internal
type ``ScopedTerm.t``. This is explained in `the page about
the term hierarchy <term_hierarchy>`_. Then, a private type ``Type.view``
is defined, and a function ``view`` allows to pattern-match on
the root of any instance of ``Type.t``. Types are built of variables,
bound variables, symbol applications (including constants when
the list of arguments is empty), function types, record types (a more
advanced topic) and explicitely quantified types.

In practice we can use ``view`` this way:

.. code-block:: ocaml

   let ty2 =
     let open Type in
     app f [const a; const b];;

   let arity_of_ty2 =
     match Type.view ty2 with
     | Type.App (_, l) -> List.length l
     | _ -> 0;;
   (* arity is 2, l is (locally) the list of arguments *)

Types can only be built using the *constructors* exposed in the module. Those
are the functions whose return type is ``Type.t``, including ``var``,
``app``, ``const`` and ``forall``, but also infix synonyms. Some standard
TPTP types are pre-defined in the `Type.TPTP module <../Type.TPTP.html>`_.

.. code-block:: ocaml

   (* polymorphic equality, returning a proposition. *)
   let type_of_eq =
     let open Type in
     let x = var 0 in  (* type var *)
     forall [x] (TPTP.o <== [x; x]);;

   let list_ x = app (Symbol.of_string "list") [x];;

   (* the type of a polymorphic list constructor "cons": forall 'a. 'a * 'a list -> 'a list *)
   let type_of_cons =
     let open Type in
     let x = var 0 in
     forall [x] (list_ x <== [x; list_ x]);;

   (* the type of "nil", the empty list, parametrized by the type of the elements of the list *)
   let type_of_nil =
     let x = Type.var 0 in
     Type.forall [x] (list_ x);;

   (* type of "cons int", the constructor of list of integers *)
   let int_list = Type.apply type_of_cons Type.TPTP.int ;;


Note that we build quantified polymorphic types using free variables, because
the constructor ``forall`` takes care of the De Bruijn indices itself.
``x`` will not appear in the resulting type because it will be a bound
variable. Conversely, ``Type.apply`` is used to apply a type to another one.

- ``Type.apply (forall [x] T) a`` will be ``[T/x]a``, a (partial) monomorphization of the left argument
- ``Type.apply (a -> b) a`` will be ``b``, the application of a function type to a matching argument.

Terms
^^^^^

We focus on first-order (polymorhphic) terms. Those are defined
in `the module FOTerm <../FOTerm.html>`_. The structure of the
module is similar to ``Type``; first, let's see the definition of a term.

.. code-block:: ocaml

   type t = private ScopedTerm.t

   type view = private
     | Var of int              (** Term variable *)
     | BVar of int             (** Bound variable (De Bruijn index) *)
     | Const of Symbol.t       (** Typed constant *)
     | TyApp of t * Type.t     (** Application to type *)
     | App of t  * t list      (** Application to a list of terms (cannot be left-nested) *)

   val view : t -> view

We can also examine and build them in a similar way:

.. code-block:: ocaml

   #install_printer FOTerm.fmt;;

   module T = FOTerm;;

   (* the constructor of lists "cons", with its type. The ~ty is a named argument *)
   let cons = T.const ~ty:type_of_cons (Symbol.of_string "cons");;

   (* constructor of empty list *)
   let nil = T.const ~ty:type_of_nil (Symbol.of_string "nil");;

   (* build a numeric constant *)
   let const_i i =
     T.const ~ty:Type.TPTP.int (Symbol.of_int i);;

   (* the empty list of terms of the TPTP type $i *)
   let l_empty = T.tyapp nil Type.TPTP.i;;

   (* the integer list [1;2;3;4] as a term *)
   let l =
     List.fold_right
      (fun i tl ->
        T.app_full cons [Type.TPTP.int] [const_i i; tl]
      ) [1;2;3;4] (T.tyapp nil Type.TPTP.int) ;;

   (* the type of l is "list of integers" *)
   Type.eq (T.ty l) int_list;;

Links
-----

.. [hashconsed] Hashconsing is a technique that maximizes sharing
   of values using a (weak) hash table. In a hashconsed structure, physical
   equality and structural equality are the same, and memory usage is
   typically reduced.
